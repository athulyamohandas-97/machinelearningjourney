# tests/test_bank_privacy_pipeline.py
"""
Privacy test-suite for a banking deployment of Rasa.
- Static tests scan source/config for insecure patterns (http://, verify=False, plaintext trackers, hardcoded secrets, etc).
- Optional runtime/integration tests exercise a running Rasa server to check tracker access control and redaction.
Configure run-time tests with environment variables described below.
"""

import os
import re
import time
from pathlib import Path

import pytest

# Optional runtime dependencies
try:
    import requests
except Exception:
    requests = None  # tests that require requests will be skipped if missing

REPO_ROOT = Path(__file__).resolve().parent.parent

# Files/folders to skip scanning
SKIP_DIRS = {".git", "__pycache__", "node_modules", "venv", ".venv", "dist", "build"}

# Utility helpers
def iter_source_files(root=REPO_ROOT, exts=(".py", ".yml", ".yaml", ".md", ".json")):
    for p in root.rglob("*"):
        if any(part in SKIP_DIRS for part in p.parts):
            continue
        if p.is_file() and p.suffix.lower() in exts:
            yield p


def grep_files(pattern, root_iter=None, flags=0):
    """Return list of (Path relative to repo, line_no, line_text) matching regex pattern."""
    if root_iter is None:
        root_iter = iter_source_files()
    rx = re.compile(pattern, flags)
    hits = []
    for p in root_iter:
        try:
            text = p.read_text(encoding="utf-8")
        except Exception:
            continue
        for i, line in enumerate(text.splitlines(), 1):
            if rx.search(line):
                hits.append((p.relative_to(REPO_ROOT), i, line.strip()))
    return hits


def fail_with_hits(msg, hits):
    snippet = "\n".join(f"{f}:{ln}: {line}" for f, ln, line in hits[:200])
    pytest.fail(f"{msg}\n\nExample matches:\n{snippet}")


# -------------------------
# STATIC / CODE-BASED TESTS
# -------------------------

def test_no_plain_http_in_configs_and_code():
    """
    Fail if repository contains 'http://' in code/config artifacts.
    Rationale: plaintext endpoints (especially for production connectors and action endpoints) are a major risk.
    Note: Local dev 'http://localhost' will still be flagged — treat the findings as a manual review list.
    """
    hits = grep_files(r"http://")
    if hits:
        fail_with_hits(
            "Found 'http://' usages. Ensure production endpoints use HTTPS; mark dev-only occurrences clearly.",
            hits,
        )


def test_no_disabled_tls_verify_calls():
    """
    Fail if code disables TLS verification (e.g., verify=False in requests).
    Rationale: disabling TLS certificate verification leads to trivial MITM attacks.
    """
    hits = grep_files(r"\bverify\s*=\s*False\b|\bssl_verify\s*=\s*False\b", flags=re.I)
    if hits:
        fail_with_hits(
            "Found TLS verification disabled (verify=False). Do NOT disable cert verification in production.",
            hits,
        )


def test_tracker_serialization_plaintext():
    """
    Heuristic: detect common patterns of serializing trackers to JSON (as_dialogue + json.dumps).
    Rationale: trackers contain user messages and entities (PII) — must be encrypted or redacted before storing.
    """
    hits_as_dialogue = grep_files(r"\.as_dialogue\(")
    hits_json_dumps = grep_files(r"\bjson\.dumps\(")
    # flag files that contain both
    files_as = {h[0] for h in hits_as_dialogue}
    files_json = {h[0] for h in hits_json_dumps}
    common = sorted(files_as & files_json)
    if common:
        lines = []
        for f in common:
            lines.append(str(f))
        pytest.fail(
            "Potential plaintext tracker serialization detected in these files (they mention .as_dialogue and json.dumps):\n"
            + "\n".join(lines)
            + "\nEnsure you redact or encrypt user text before storing."
        )


def test_tracker_store_has_deletion_api():
    """
    Look for presence of a deletion API (tracker store deletion methods OR delete endpoint).
    Rationale: banks must support data subject requests (delete/erase).
    Heuristic: ensures tracker store implementations define 'delete' methods or code exposes a DELETE handler.
    """
    tracker_files = list((REPO_ROOT / "rasa").rglob("tracker_store.py"))
    # also search for classes implementing 'delete' in tracker store files
    missing = []
    for f in tracker_files:
        text = f.read_text(encoding="utf-8")
        if re.search(r"\bdef\s+delete\s*\(", text) or re.search(r"\bdef\s+delete_tracker\b", text):
            continue
        else:
            missing.append(f.relative_to(REPO_ROOT))
    if missing:
        pytest.fail(
            "Tracker store implementations appear to lack a delete() method. Required for GDPR/right-to-erasure.\n"
            + "\n".join(str(m) for m in missing)
        )


def test_no_sensitive_logging_statements():
    """
    Heuristic: find logger.*(...) calls that appear to include user text or tracker dumps.
    Rationale: logs often end up in long-term storage — do not log PII.
    """
    pattern = r"logger\.(debug|info|warning|error|exception)\s*\(.*(text|message|user|tracker|as_dialogue|as_dict).*?\)"
    hits = grep_files(pattern, flags=re.I)
    # Filter out tests and doc examples (heuristic)
    filtered = [h for h in hits if "tests/" not in str(h[0]) and "docs/" not in str(h[0])]
    if filtered:
        fail_with_hits(
            "Logging statements may include user messages or tracker content. Redact before logging.",
            filtered,
        )


def test_no_hardcoded_secrets_in_repo():
    """
    Detect likely hardcoded API keys, tokens, passwords in repo files.
    Heuristic pattern: (token|secret|password|api_key) = 'LONGSTRING' or YAML style token: "abcd..."
    """
    # look for obvious key names with string values
    hits = grep_files(
        r"(?:api[_-]?key|apikey|token|secret|password|aws_access_key_id|aws_secret_access_key)\s*[:=]\s*['\"]?([A-Za-z0-9\-_]{8,})['\"]?",
        flags=re.I,
    )
    # suppress obviously safe placeholders
    filtered = []
    for h in hits:
        _, _, line = h
        if re.search(r"(YOUR_|REPLACE_|example|<token>|<password>)", line, flags=re.I):
            continue
        filtered.append(h)
    if filtered:
        fail_with_hits(
            "Possible hardcoded credentials found. Use environment variables / secrets manager instead.",
            filtered,
        )


def test_channels_outbound_http_calls_and_verify():
    """
    Find outbound HTTP client calls in channel connectors and ensure they are not plainly insecure.
    Heuristics:
      - Look for 'requests.' usages in channels/.
      - Flag if the literal string URL includes http:// or verify=False within channel code.
    """
    channels_dir = REPO_ROOT / "rasa" / "core" / "channels"
    if not channels_dir.exists():
        pytest.skip("channels directory not present in this checkout")
    hits = grep_files(r"\brequests\.(get|post|put|patch|delete)\s*\(", root_iter=channels_dir.rglob("*.py"))
    http_hits = grep_files(r"http://", root_iter=channels_dir.rglob("*.py"))
    verify_false = grep_files(r"verify\s*=\s*False", root_iter=channels_dir.rglob("*.py"))
    # Report anything found — connectors are an important boundary and need manual review
    if hits or http_hits or verify_false:
        msg = []
        if hits:
            msg.append("Outbound HTTP client usage in channels (requests.*) found.")
        if http_hits:
            msg.append("Literal 'http://' found within channels (check connector configuration).")
        if verify_false:
            msg.append("TLS verify=False found inside channel code.")
        combined = (hits + http_hits + verify_false)
        fail_with_hits("\n".join(msg), combined)


def test_telemetry_module_no_user_text():
    """
    Ensure telemetry/usage modules do not reference or send raw user text.
    Heuristic: search for references to 'message', 'text', 'as_dialogue' in telemetry module.
    """
    telemetry_file = REPO_ROOT / "rasa" / "telemetry.py"
    if not telemetry_file.exists():
        pytest.skip("telemetry.py not present in this checkout")
    text = telemetry_file.read_text(encoding="utf-8")
    if re.search(r"\b(message|text|as_dialogue|as_dict|tracker)\b", text):
        pytest.fail(
            "Telemetry module references message/tracker/text. Telemetry should only send non-textual metadata."
        )


# -------------------------
# RUNTIME / INTEGRATION TESTS (guarded)
# -------------------------
# These tests only run when RUN_RUNTIME_TESTS env var is set to "1" and requests is available.
# They are intended to be run against a deployed/test Rasa server for additional runtime checks.

RUN_RUNTIME = os.getenv("RUN_RUNTIME_TESTS", "0") == "1"
RASA_SERVER_URL = os.getenv("RASA_SERVER_URL", "http://localhost:5005").rstrip("/")
# If you have API auth (recommended), set RASA_AUTH_HEADER to the full header value:
# Example: RASA_AUTH_HEADER="Authorization: Bearer yourtoken"
RASA_AUTH_HEADER = os.getenv("RASA_AUTH_HEADER", "")
EXPECT_TRACKER_AUTH = os.getenv("EXPECT_TRACKER_AUTH", "0") == "1"
EXPECT_REDACTION = os.getenv("EXPECT_REDACTION", "0") == "1"


def _send_message(sender_id: str, message: str, timeout=5):
    """
    Use REST webhook to send a single user message.
    """
    assert requests is not None, "requests lib required for runtime tests"
    url = f"{RASA_SERVER_URL}/webhooks/rest/webhook"
    payload = {"sender": sender_id, "message": message}
    headers = {}
    if RASA_AUTH_HEADER:
        # RASA_AUTH_HEADER should be like 'Authorization: Bearer token' or 'x-api-key: token'
        k, v = [x.strip() for x in RASA_AUTH_HEADER.split(":", 1)]
        headers[k] = v
    r = requests.post(url, json=payload, headers=headers, timeout=timeout)
    r.raise_for_status()
    return r.json()


def _get_tracker(sender_id: str, timeout=5):
    assert requests is not None, "requests lib required for runtime tests"
    headers = {}
    if RASA_AUTH_HEADER:
        k, v = [x.strip() for x in RASA_AUTH_HEADER.split(":", 1)]
        headers[k] = v
    url = f"{RASA_SERVER_URL}/conversations/{sender_id}/tracker"
    r = requests.get(url, headers=headers, timeout=timeout)
    return r


@pytest.mark.skipif(not RUN_RUNTIME or requests is None, reason="Runtime tests disabled or requests missing")
def test_tracker_api_requires_auth_if_expected():
    """
    If EXPECT_TRACKER_AUTH is enabled, verify that the tracker endpoint is protected (rejects unauthenticated GET).
    This is important for banks: conversation history endpoints must be access-controlled.
    """
    sender = f"privacy_test_{int(time.time())}"
    # send a message to create a conversation
    _send_message(sender, "hello")  # uses configured auth header if provided

    r_no_auth = requests.get(f"{RASA_SERVER_URL}/conversations/{sender}/tracker", timeout=5)
    if EXPECT_TRACKER_AUTH:
        # Expect 401/403 when unauthenticated
        assert r_no_auth.status_code in (401, 403), (
            "Tracker endpoint should be access-controlled and return 401/403 for unauthenticated requests."
        )
    else:
        # If not expecting auth, basic smoke check that endpoint returns 200 for troubleshooting
        assert r_no_auth.status_code == 200, "Tracker endpoint did not return 200 (or is unreachable)."


@pytest.mark.skipif(not RUN_RUNTIME or requests is None, reason="Runtime tests disabled or requests missing")
def test_tracker_redaction_runtime_check():
    """
    Send a clearly sensitive test string and assert the tracker API does not return the raw string.
    This verifies runtime redaction (if EXPECT_REDACTION=True). If your deployment uses encryption-at-rest instead
    of redaction, you may want to adapt this test to verify API responses do not expose raw text.
    """
    if not EXPECT_REDACTION:
        pytest.skip("Redaction runtime check not enabled (set EXPECT_REDACTION=1 to enable).")
    sender = f"redaction_test_{int(time.time())}"
    secret_text = "SSN_TEST_123-45-6789"
    _send_message(sender, secret_text)
    r = _get_tracker(sender)
    assert r.status_code == 200, f"Failed to fetch tracker: {r.status_code} {r.text}"
    body = r.text.lower()
    assert secret_text.lower() not in body, "Raw sensitive text was found in tracker API response. Must redact or not expose."
