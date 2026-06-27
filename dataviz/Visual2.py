import marimo
__generated_with = "0.23.2"
app = marimo.App(width="full")


@app.cell
def _():
    # 1. Imports needed for the project
    import marimo as mo
    import pandas as pd
    import numpy as np
    import math
    from svg import SVG, Circle, Rect, G, Title, Text, Path
    from sklearn.preprocessing import StandardScaler
    from sklearn.decomposition import PCA

    return Circle, G, PCA, Path, Rect, SVG, StandardScaler, Text, Title, math, mo, np, pd


@app.cell
def _(np, pd):
    # 2. Load the two data files we need
    soil = pd.read_csv("oumalik_soil_data.csv")
    env = pd.read_csv("oumalik_environmental_data.csv")
    soil.columns = soil.columns.str.strip()
    env.columns = env.columns.str.strip()
    soil = soil.replace(-9999, np.nan)
    env = env.replace(-9999, np.nan)
    return env, soil


@app.cell
def _(env, np, soil):
    # 3. Merge soil + disturbance, clip the disturbance score to its valid 1-10 range
    #    (some rows in the raw data go up to 12, which the documentation flags as a mistake)
    df = soil.merge(
        env[["plot_number", "disturbance_type", "disturbance_intensity", "disturbance_score"]],
        on="plot_number",
    )
    df["disturbance_score"] = np.clip(df["disturbance_score"], a_min=1, a_max=10)
    return (df,)


@app.cell
def _(PCA, StandardScaler, df):
    # 4. PCA on the 16 soil variables, summarised into 5 components for stacking
    pca_vars = [
        "organic_matter", "hygroscopic_moisture", "water_absorption",
        "field_capacity", "wilting_point", "available_water",
        "carbonates", "pH", "NH4", "NO3", "N", "P", "K", "Ca", "Mg",
        "cation_ex_capacity",
    ]
    X = df[pca_vars].fillna(df[pca_vars].median())
    pca = PCA(n_components=5)
    scores = pca.fit_transform(StandardScaler().fit_transform(X))

    # Shift each component non-negative so segments can stack
    for _i in range(5):
        df[f"pc{_i+1}"] = scores[:, _i] - scores[:, _i].min()

    comp_labels = ["Organic/water", "Nitrogen", "Alkaline min.", "P + K", "Carbonates"]
    comp_variance = [round(v * 100, 1) for v in pca.explained_variance_ratio_]
    return comp_labels, comp_variance, df


@app.cell
def _(df):
    # 5. Sort: group by disturbance type, low-to-high score within each group
    df_sorted = df.sort_values(["disturbance_type", "disturbance_score"]).reset_index(drop=True)
    return (df_sorted,)


@app.function
# 6. Linear rescale x from one range to another (used everywhere for radii/angles)
def rescale(x, dmin, dmax, rmin, rmax):
    return rmin + (x - dmin) * (rmax - rmin) / (dmax - dmin)


@app.cell
def _(
    Circle, G, Path, Rect, SVG, Text, Title,
    comp_labels, comp_variance, df_sorted, math
):
    # 7. Build the radial chart
    W, H = 880, 780
    cx, cy = W / 2, H / 2 - 10
    r_in, r_out = 70, 305
    max_score = 10

    pc_colors = ["#3d7a4f", "#4da8c4", "#e0a535", "#cc5c5c", "#8265a7"]
    type_names = {1: "Undisturbed", 2: "Naturally disturbed", 3: "Anthropogenically disturbed"}
    type_tint = {1: "#f1f6f1", 2: "#eff2f6", 3: "#f7efef"}

    # Angular layout: spread bars evenly with a small gap where the type changes
    types = df_sorted["disturbance_type"].astype(int).tolist()
    g_first, g_last = {}, {}
    for _i, dt in enumerate(types):
        g_first.setdefault(dt, _i); g_last[dt] = _i

    gap = 0.05
    bar_a = (2 * math.pi - gap * len(g_first)) / len(types)
    a_starts = []; a = -math.pi / 2; prev = None
    for dt in types:
        if prev is not None and dt != prev: a += gap
        a_starts.append(a); a += bar_a; prev = dt

    # Polar -> Cartesian + annular sector path string
    def polar(r, ang): return cx + r * math.cos(ang), cy + r * math.sin(ang)
    def sector_d(r1, r2, a1, a2):
        p1 = polar(r1, a1); p2 = polar(r2, a1); p3 = polar(r2, a2); p4 = polar(r1, a2)
        large = 1 if (a2 - a1) > math.pi else 0
        return (f"M {p1[0]:.2f} {p1[1]:.2f} L {p2[0]:.2f} {p2[1]:.2f} "
                f"A {r2:.2f} {r2:.2f} 0 {large} 1 {p3[0]:.2f} {p3[1]:.2f} "
                f"L {p4[0]:.2f} {p4[1]:.2f} "
                f"A {r1:.2f} {r1:.2f} 0 {large} 0 {p1[0]:.2f} {p1[1]:.2f} Z")

    elements = []

    # Background tints per disturbance type
    for dt, first in g_first.items():
        a1, a2 = a_starts[first], a_starts[g_last[dt]] + bar_a
        elements.append(Path(d=sector_d(r_in, r_out, a1, a2), fill=type_tint[dt]))

    # Guide rings at scores 3, 6, 9, 10
    for v in [3, 6, 9, 10]:
        r = rescale(v, 0, max_score, r_in, r_out)
        is_max = v == max_score
        elements.append(Circle(cx=cx, cy=cy, r=r, fill="none", stroke="#c0c0c0",
                               stroke_width=0.8 if is_max else 0.4,
                               stroke_dasharray=None if is_max else "2,3"))
        elements.append(Text(x=cx + 3, y=cy - r + 10, text=str(v), font_size=7, fill="#aaa"))

    # Inner hub
    elements.append(Circle(cx=cx, cy=cy, r=r_in, fill="white", stroke="#bbb", stroke_width=1))
    elements.append(Text(x=cx, y=cy - 4, text="disturbance",
                         font_size=10, fill="#666", text_anchor="middle"))
    elements.append(Text(x=cx, y=cy + 9, text="score 1-10",
                         font_size=9, fill="#999", text_anchor="middle"))

    # Group labels just outside the outer ring; labels > 20 chars wrap to two lines
    for dt, first in g_first.items():
        mid = (a_starts[first] + a_starts[g_last[dt]] + bar_a) / 2
        lx, ly = polar(r_out + 30, mid)
        lines = type_names[dt].rsplit(" ", 1) if len(type_names[dt]) > 20 else [type_names[dt]]
        for k, line in enumerate(lines):
            elements.append(Text(x=lx, y=ly + (k - (len(lines)-1)/2) * 12, text=line,
                                 font_size=10, fill="#333", font_weight="600",
                                 text_anchor="middle", dominant_baseline="central"))

    # Bars: one <g class="bar"> per plot, holding the stacked PCA segments + a Title for hover
    for idx, d in df_sorted.iterrows():
        a1 = a_starts[idx] + bar_a * 0.07
        a2 = a_starts[idx] + bar_a * 0.93
        ds = int(d["disturbance_score"])
        bar_len = rescale(ds, 0, max_score, 0, r_out - r_in)
        pcs = [float(d[f"pc{j+1}"]) for j in range(5)]
        total = sum(pcs) or 1.0

        tip = (
            f"Plot {int(d['plot_number'])} - {type_names[int(d['disturbance_type'])]}\n"
            f"Disturbance score {ds}/10  intensity {int(d['disturbance_intensity'])}/7\n"
            f"Organic matter {d['organic_matter']:.1f}%  pH {d['pH']:.2f}  "
            f"N {d['N']:.1f} ppm  Ca {d['Ca']:.0f} ppm\n"
            + "  ".join(f"PC{j+1} {pcs[j]/total*100:.0f}%" for j in range(5))
        )

        segs = [Title(elements=[tip])]
        r = r_in
        for j in range(5):
            seg = (pcs[j] / total) * bar_len
            if seg < 0.3: r += seg; continue
            segs.append(Path(d=sector_d(r, r + seg, a1, a2),
                             fill=pc_colors[j], stroke="white", stroke_width=0.3))
            r += seg
        elements.append(G(class_=["bar"], elements=segs))

        # Plot number at the bar tip, rotated to run along the bar direction;
        # flip anchor + subtract 180 on the left half so text always reads outward
        mid = (a1 + a2) / 2
        lx, ly = polar(r + 7, mid)
        deg = (mid * 180 / math.pi) % 360
        rot = deg - 180 if 90 < deg <= 270 else deg
        anchor = "end" if 90 < deg <= 270 else "start"
        elements.append(Text(x=lx, y=ly, text=str(int(d["plot_number"])),
                             font_size=5.5, fill="#999",
                             text_anchor=anchor, dominant_baseline="central",
                             transform=f"rotate({rot:.1f} {lx:.1f} {ly:.1f})"))

    # Legend along the bottom
    ly = H - 18
    for j in range(5):
        lx = 30 + j * 155
        elements.append(Rect(x=lx, y=ly - 9, width=10, height=10, fill=pc_colors[j]))
        elements.append(Text(x=lx + 14, y=ly,
                             text=f"PC{j+1}  {comp_labels[j]} ({comp_variance[j]}%)",
                             font_size=8, fill="#444"))

    plot = SVG(width=W, height=H, elements=elements)
    return W, H, plot


@app.cell
def _(H, W, comp_variance, mo, plot):
    # 8. Render the SVG inside an iframe with a tiny tooltip script
    css = """
    <style>
      svg { background: white; font-family: system-ui, sans-serif; }
      g.bar path { transition: opacity 0.12s; cursor: pointer; }
      g.bar:hover path { opacity: 1; stroke: #222; stroke-width: 0.6; }
      #tip { position: fixed; left: 0; top: 0; pointer-events: none; opacity: 0;
             background: rgba(255,255,255,0.97); border: 1px solid #ccc;
             padding: 8px 12px; border-radius: 6px; font: 12px system-ui, sans-serif;
             white-space: pre-line; line-height: 1.5; box-shadow: 0 4px 12px rgba(0,0,0,0.15);
             transition: opacity 0.05s; z-index: 9999; }
    </style>
    """

    script = """
    <script>
    (function () {
        const tip = Object.assign(document.createElement("div"), {id: "tip"});
        document.body.appendChild(tip);

        // Pre-cache tooltip text and strip the native <title> — deferred one tick so
        // the iframe has finished injecting the SVG into its document before we query it
        function init() {
            document.querySelectorAll("g.bar").forEach(bar => {
                const t = bar.querySelector("title");
                if (!t) return;
                bar.dataset.tip = t.textContent;
                t.remove();
            });
        }
        setTimeout(init, 0);

        document.body.addEventListener("mousemove", e => {
            const bar = e.target.closest("g.bar");
            if (!bar) { tip.style.opacity = 0; return; }
            tip.textContent = bar.dataset.tip;
            let x = e.clientX + 15, y = e.clientY + 15;
            if (x + tip.offsetWidth  > innerWidth)  x = e.clientX - tip.offsetWidth  - 10;
            if (y + tip.offsetHeight > innerHeight) y = e.clientY - tip.offsetHeight - 10;
            tip.style.transform = `translate(${x}px, ${y}px)`;
            tip.style.opacity = 1;
        });
    })();
    </script>
    """

    chart = mo.iframe(css + plot.as_str() + script, width=W + 20, height=H + 20)

    var_total = round(sum(comp_variance), 1)
    description = mo.md(f"""
# Soil composition and disturbance at Oumalik

87 tundra plots grouped into three arcs by disturbance type.
Bar length encodes disturbance score (1-10); stacked colours show soil composition
summarised by PCA over 16 soil variables ({var_total}% of variance). Hover over any bar for details.
""")

    mo.vstack([description, chart], align="center")
    return


if __name__ == "__main__":
    app.run()