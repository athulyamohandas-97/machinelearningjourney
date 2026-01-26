import re
import pytest
from unittest.mock import patch
from rasa.telemetry import telemetry

# ---- PII regex patterns ----
PII_PATTERNS = {
    "email": re.compile(r"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+"),
    "phone": re.compile(r"\b(\+?\d{1,3}[\s.-]?)?\(?\d{3,5}\)?[\s.-]?\d{3,5}[\s.-]?\d{3,5}\b"),
    "account_number": re.compile(r"\b\d{8,16}\b"),  # Generic 8–16 digit numbers
    "ssn": re.compile(r"\b\d{3}-\d{2}-\d{4}\b"),
    "credit_card": re.compile(r"\b(?:\d[ -]*?){13,16}\b"),
    "pan_number": re.compile(r"\b[A-Z]{5}[0-9]{4}[A-Z]\b"),  # Indian PAN
    "name_like": re.compile(r"\b([A-Z][a-z]{2,15}\s[A-Z][a-z]{2,15})\b"),  # e.g., John Doe
}

@pytest.fixture
def mock_telemetry_payload():
    """Simulate a telemetry payload possibly sent by Rasa."""
    return {
        "event": "user_message_processed",
        "properties": {
            "model_id": "model_2025_10_09",
            "user_text_sample": "Hi, my name is John Doe and my account number is 123456789012",
            "nlu_confidence": 0.97,
            "channel": "whatsapp"
        }
    }

def contains_pii(payload: dict) -> list[str]:
    """Return list of detected PII matches from payload values."""
    found = []
    serialized = str(payload)
    for label, pattern in PII_PATTERNS.items():
        matches = pattern.findall(serialized)
        if matches:
            found.append((label, matches))
    return found


@patch("rasa.telemetry.telemetry.track")
def test_telemetry_no_user_pii(mock_track, mock_telemetry_payload):
    """
    Ensure telemetry never includes PII such as names, account numbers, or emails.
    """

    # Simulate telemetry being sent
    telemetry.track(mock_telemetry_payload["event"], mock_telemetry_payload["properties"])

    # Extract the payload that would have been sent
    args, kwargs = mock_track.call_args
    event_data = args[1] if len(args) > 1 else kwargs.get("properties", {})

    # Check for PII
    pii_found = contains_pii(event_data)

    # Fail if any PII pattern is found
    assert not pii_found, (
        f"Telemetry payload contains PII: {pii_found}. "
        "Ensure user input and sensitive data are not logged or sent in telemetry."
    )
