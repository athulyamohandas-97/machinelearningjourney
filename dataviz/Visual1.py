import marimo

__generated_with = "0.21.1"
app = marimo.App(width="full")


@app.cell
def _():
    import marimo as mo
    import pandas as pd
    import numpy as np
    from sklearn.cluster import KMeans
    import json

    return KMeans, json, mo, np, pd


@app.cell
def _(pd):
    # for this visualization I only need the environmental data set
    df_env = pd.read_csv("data/oumalik_environmental_data.csv", parse_dates=["date"])
    return (df_env,)


@app.cell
def _(df_env):
    # there were some inconsistencies in the naming of some of the columns
    df_env.columns = df_env.columns.str.strip().str.lower()
    return


@app.cell
def _(df_env, np):
    # in the project description file it was mentioned that the missing values are encoded as -9999
    df_env["cover_total"] = df_env["cover_total"].replace(-9999, np.nan)
    return


@app.cell
def _(df_env):
    median_cover = df_env["cover_total"].median()
    return (median_cover,)


@app.cell
def _(df_env, median_cover):
    df_env["cover_total"] = df_env["cover_total"].fillna(median_cover)
    return


@app.cell
def _(df_env, np):
    # there are some values above 10 for the distrubance_score
    # clipping them to be between 1 and 10
    df_env["disturbance_score"] = np.clip(df_env["disturbance_score"], min=1, max=10)
    return


@app.cell
def _(df_env):
    # the mover plant cover, the better; so flip it for the penalty
    df_env["cover_penalty"] = 100 - df_env["cover_total"]
    return


@app.cell
def _(df_env):
    df_env["disturbance_penalty"] = (df_env["disturbance_score"].astype(float)) * 10
    return


@app.cell
def _(df_env):
    df_env["concern_score"] = (df_env["cover_penalty"] + df_env["disturbance_penalty"]) / 2
    return


@app.cell
def _(df_env):
    X = df_env["concern_score"].values.reshape(-1, 1)
    return (X,)


@app.cell
def _(KMeans):
    kmeans = KMeans(n_clusters=3, random_state=10)
    return (kmeans,)


@app.cell
def _(X, df_env, kmeans):
    df_env["raw_cluster"] = kmeans.fit_predict(X)
    return


@app.cell
def _(kmeans):
    centers = kmeans.cluster_centers_.flatten()
    return (centers,)


@app.cell
def _(centers, np):
    sorted_indices = np.argsort(centers)
    return (sorted_indices,)


@app.cell
def _(sorted_indices):
    color_mapping = {
        sorted_indices[0]: 'green',
        sorted_indices[1]: 'yellow',
        sorted_indices[2]: 'red'
    }
    return (color_mapping,)


@app.cell
def _(color_mapping, df_env):
    df_env["concern_tier"] = df_env["raw_cluster"].map(color_mapping)
    return


@app.cell
def _():
    animal_cols = [
        'disturbance_caribou', 'disturbance_microtine', 'disturbance_squirrel', 
        'disturbance_ptarmigan', 'disturbance_birds', 'disturbance_insects'
    ]

    label_map = {
        'disturbance_caribou': 'Caribou', 'disturbance_microtine': 'Rodents',
        'disturbance_squirrel': 'Squirrels', 'disturbance_ptarmigan': 'Ptarmigan',
        'disturbance_birds': 'Other Birds', 'disturbance_insects': 'Insects'
    }
    return animal_cols, label_map


@app.cell
def _(animal_cols, df_env, label_map, np, pd):
    cols_per_row = 10

    # sorting plots from north to south (latitude)
    df_grid = df_env.copy()
    df_grid = df_grid.sort_values(by='latitude', ascending=False).reset_index(drop=True)

    # putting them in rows
    df_grid['grid_row_temp'] = df_grid.index // cols_per_row

    # sorting west to east (longitude) inside each row
    df_grid = df_grid.groupby('grid_row_temp', group_keys=False).apply(lambda x: x.sort_values(by='longitude', ascending=True))

    # assign final 1-indexed CSS grid coordinates
    df_grid['grid_row'] = (np.arange(len(df_grid)) // cols_per_row) + 1
    df_grid['grid_col'] = (np.arange(len(df_grid)) % cols_per_row) + 1


    map_data = []

    for i, row in df_grid.iterrows():

        animal_data = []
        for col in animal_cols:
            val = row.get(col, 0)
            if pd.notna(val) and val > 0:
                animal_data.append({"animal": label_map[col], "score": int(val)})
        animal_data = sorted(animal_data, key=lambda x: x['score'], reverse=True)

        map_data.append({
            "plot_id": str(row.get('plot_number', i)),
            "latitude": float(row['latitude']),
            "longitude": float(row['longitude']),
            "grid_row": int(row['grid_row']),
            "grid_col": int(row['grid_col']),
            "releve_area": float(row.get('releve_area', 25)),
            "releve_shape": str(row.get('releve_shape', 'irregular')).strip().lower(),
            "concern_tier": str(row.get('concern_tier', 'green')).strip().lower(),
            "overall_dist_score": int(row.get('disturbance_score', 0)), 
            "dist_type_code": int(row.get('disturbance_type', 0)),    
            "animals": animal_data 
        })
    return (map_data,)


@app.cell
def _(json, map_data, mo):
    map_data_json = json.dumps(map_data)


    tile_url = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"

    map_html = f"""<!DOCTYPE html>
    <html>
    <head>
        <meta charset="utf-8"/>
        <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"/>
        <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
        <style>
            * {{ margin: 0; padding: 0; box-sizing: border-box; }}
            body {{ 
                font-family: sans-serif; 
                background: #f8f9fa; 
                display: flex; 
                flex-direction: row; 
                height: 100vh; 
                width: 100vw;
                overflow: hidden; 
            }}

            .sidebar {{
                width: 280px; background: white; border-right: 1px solid #ccc;
                padding: 20px; display: flex; flex-direction: column; gap: 24px;
                z-index: 10; box-shadow: 2px 0 8px rgba(0,0,0,0.05);
            }}
            .sidebar h2 {{ font-size: 18px; color: #222; margin-bottom: 8px; }}
            .sidebar p {{ font-size: 12px; color: #666; line-height: 1.4; }}

            .radio-group {{ display: flex; flex-direction: column; gap: 12px; }}
            .radio-group label {{
                cursor: pointer; font-size: 14px; font-weight: bold; color: #444; 
                display: flex; align-items: center; gap: 8px;
                padding: 8px; background: #f4f6f8; border-radius: 6px; border: 1px solid #eee;
                transition: background 0.2s;
            }}
            .radio-group label:hover {{ background: #e2e8f0; }}

            .static-legend {{ font-size: 12px; color: #333; line-height: 1.5; }}
            .static-legend h4 {{ font-size: 12px; text-transform: uppercase; margin-bottom: 10px; color: #555; border-bottom: 1px solid #ddd; padding-bottom: 4px; }}
            .legend-item {{ display: flex; align-items: center; margin-bottom: 8px; }}
            .legend-color {{ width: 16px; height: 16px; margin-right: 10px; border-radius: 3px; border: 1px solid rgba(0,0,0,0.3); }}

            .main-content {{ flex-grow: 1; position: relative; background: #e9ecef; overflow: hidden; }}

            .view-panel {{ position: absolute; top: 0; left: 0; right: 0; bottom: 0; }}
            #map-panel {{ display: block; }} 
            #puzzle-panel {{ display: none; align-items: center; justify-content: center; }}
            #map-view {{ height: 100%; width: 100%; }}


            #alaska-grid {{
                display: grid;
                grid-template-columns: repeat(10, 1fr); 
                grid-template-rows: repeat(9, 1fr);    
                gap: 6px; 
                width: 100%;
                max-width: 600px; 
                margin: 0 auto;
            }}

            .puzzle-piece {{
                aspect-ratio: 1; border-radius: 3px; border: 1px solid rgba(0,0,0,0.1);
                transition: transform 0.2s, box-shadow 0.2s; cursor: crosshair;
            }}
            .puzzle-piece:hover {{ transform: scale(1.15); box-shadow: 0 4px 8px rgba(0,0,0,0.3); z-index: 10; }}

            .custom-plot-marker {{ background: transparent; border: none; }}
            .marker-shape {{ border: 1px solid #222; box-shadow: 0 2px 4px rgba(0,0,0,0.4); opacity: 0.85; transition: transform 0.2s; }}
            .marker-shape:hover {{ transform: scale(1.4); opacity: 1; }}
            .leaflet-tooltip {{ padding: 0; border: none; box-shadow: none; background: transparent; }}

            .shared-tooltip-content {{ padding: 10px; border-radius: 6px; box-shadow: 0 4px 12px rgba(0,0,0,0.25); background: white; color: #333; min-width: 180px; max-width: 250px; word-wrap: break-word; }}
            #global-tooltip {{ position: fixed; pointer-events: none; display: none; z-index: 9999; }}

            .tooltip-header {{ border-bottom: 1px solid #ddd; padding-bottom: 5px; margin-bottom: 6px; text-align: center; }}
            .tooltip-subheader {{ font-size: 11px; margin-bottom: 6px; text-align: center; border-bottom: 1px solid #ddd; padding-bottom: 6px; line-height: 1.4; }}
            .bar-row {{ display: flex; align-items: center; margin-bottom: 4px; font-size: 11px; }}
            .bar-label {{ width: 65px; text-align: left; }}
            .bar-track {{ flex-grow: 1; height: 8px; background: #eee; border-radius: 4px; margin: 0 6px; width: 60px; }}
            .bar-fill {{ height: 100%; background: #607d8b; border-radius: 4px; }}
            .bar-value {{ width: 15px; text-align: right; font-weight: bold; }}
        </style>
    </head>
    <body>

        <div class="sidebar">
            <div>
                <h2>Disturbance Map</h2>
                <p>Hover over plots to view physical dimensions and local disturbance activity.</p>
            </div>

            <div class="radio-group">
                <label><input type="radio" name="viewToggle" value="map" checked> Satellite Map</label>
                <label><input type="radio" name="viewToggle" value="puzzle"> Grid Layout</label>
            </div>

            <div class="static-legend">
                <h4>Concern Tier</h4>
                <div class="legend-item"><div class="legend-color" style="background: #f44336;"></div>High (Red)</div>
                <div class="legend-item"><div class="legend-color" style="background: #ff9800;"></div>Medium (Yellow)</div>
                <div class="legend-item"><div class="legend-color" style="background: #4caf50;"></div>Low (Green)</div>

                <div id="shape-scale-legend" style="margin-top: 15px; padding-top: 15px; border-top: 1px solid #ddd; color: #777; display: block;">
                    <strong>Size</strong>: Scaled by Area<br>
                    <strong>Shape</strong>: Square, Rectangle, Irregular
                </div>
            </div>
        </div>

        <div class="main-content">
            <div id="map-panel" class="view-panel">
                <div id="map-view"></div>
            </div>
            <div id="puzzle-panel" class="view-panel">
                <div id="alaska-grid"></div>
            </div>

            <div id="global-tooltip"></div>
        </div>

        <script>
            const plots = {map_data_json};

            const colorMap = {{
                'red': '#f44336',
                'yellow': '#ff9800',
                'green': '#4caf50'
            }};

            function getTooltipHTML(plot, bgColor) {{
            
                // 1. Determine Disturbance Type explicitly
                let distTypeStr = "Unknown";
                if (plot.dist_type_code === 1) distTypeStr = "Undisturbed";
                else if (plot.dist_type_code === 2) distTypeStr = "Naturally Disturbed";
                else if (plot.dist_type_code === 3) distTypeStr = "Anthropogenically Disturbed";

                // 2. Build the Animal Bar Charts
                let lowerContentHTML = "";
                if (plot.animals && plot.animals.length > 0) {{
                    lowerContentHTML = plot.animals.map(a => `
                        <div class="bar-row">
                            <div class="bar-label">${{a.animal}}</div>
                            <div class="bar-track"><div class="bar-fill" style="width: ${{a.score * 10}}%"></div></div>
                            <div class="bar-value">${{a.score}}</div>
                        </div>
                    `).join('');
                }} else {{
                    lowerContentHTML = "<div style='font-size:11px; text-align:center; padding-top:4px; color:#777;'>No animal activity recorded</div>";
                }}

                // 3. Return the fully assembled tooltip
                return `
                    <div class="shared-tooltip-content">
                        <div class="tooltip-header"><strong>Plot ${{plot.plot_id}}</strong></div>
                        <div class="tooltip-subheader">
                            Area: ${{plot.releve_area}} m²<br>
                            Tier: <span style="color:${{bgColor}};font-weight:bold;">${{plot.concern_tier.toUpperCase()}}</span><br>
                            <strong>Disturbance: ${{plot.overall_dist_score}}/10</strong><br>
                        
                            <div style="margin-top: 4px; padding-top: 4px; border-top: 1px dashed #ddd; color: black; font-weight: bold;">
                                Impact: ${{distTypeStr}}
                            </div>
                        </div>
                        ${{lowerContentHTML}}
                    </div>
                `;
            }}

            const map = L.map('map-view').setView([69.845, -155.985], 14);
            L.tileLayer('{tile_url}', {{ attribution: 'Tiles &copy; Esri' }}).addTo(map);

            const domTooltip = document.getElementById('global-tooltip');

            function updateTooltipPosition(e) {{
                if (domTooltip.style.display === 'none') return;

                const tooltipWidth = domTooltip.offsetWidth;
                const tooltipHeight = domTooltip.offsetHeight;
                const padding = 25; 
                const margin = 10;  

                const cx = e.clientX;
                const cy = e.clientY;
                const vw = window.innerWidth;
                const vh = window.innerHeight;

                let left = cx + padding;
                let top = cy - (tooltipHeight / 2);

                if (top < margin) top = margin;
                else if (top + tooltipHeight > vh - margin) top = vh - tooltipHeight - margin;

                if (left + tooltipWidth > vw - margin) left = cx - tooltipWidth - padding;

                domTooltip.style.left = left + 'px';
                domTooltip.style.top  = top  + 'px';
            }}

            const gridContainer = document.getElementById('alaska-grid');

            plots.forEach(plot => {{
                const bgColor = colorMap[plot.concern_tier] || '#aaaaaa';

                const baseSize = Math.sqrt(plot.releve_area) * 2.5;
                let width = baseSize, height = baseSize, borderRadius = '50%';
                if (plot.releve_shape === 'square') borderRadius = '2px';
                else if (plot.releve_shape === 'rectangular') {{ borderRadius = '2px'; width *= 1.4; height *= 0.7; }}

                const markerHtml = `<div class="marker-shape" style="width: ${{width}}px; height: ${{height}}px; background: ${{bgColor}}; border-radius: ${{borderRadius}};"></div>`;
                const icon = L.divIcon({{ className: 'custom-plot-marker', html: markerHtml, iconSize: [width, height], iconAnchor: [width/2, height/2] }});

                L.marker([plot.latitude, plot.longitude], {{ icon: icon }})
                    .addTo(map)
                    .bindTooltip(getTooltipHTML(plot, bgColor), {{ direction: 'top', offset: [0, -(height/2)], opacity: 1 }});

                const cell = document.createElement('div');
                cell.className = 'puzzle-piece';
                cell.style.backgroundColor = bgColor;

                cell.style.gridRow = plot.grid_row;
                cell.style.gridColumn = plot.grid_col;

                cell.addEventListener('mouseenter', (e) => {{
                    domTooltip.innerHTML = getTooltipHTML(plot, bgColor);
                    domTooltip.style.display = 'block';
                    updateTooltipPosition(e);
                }});
                cell.addEventListener('mousemove', updateTooltipPosition);
                cell.addEventListener('mouseleave', () => domTooltip.style.display = 'none');

                gridContainer.appendChild(cell);
            }});

            const mapPanel = document.getElementById('map-panel');
            const puzzlePanel = document.getElementById('puzzle-panel');
            const radios = document.querySelectorAll('input[name="viewToggle"]');
            const shapeScaleLegend = document.getElementById('shape-scale-legend'); 

            radios.forEach(radio => {{
                radio.addEventListener('change', (e) => {{
                    if (e.target.value === 'map') {{
                        puzzlePanel.style.display = 'none';
                        mapPanel.style.display = 'block';
                        shapeScaleLegend.style.display = 'block'; 
                        setTimeout(() => map.invalidateSize(), 10); 
                    }} else {{
                        mapPanel.style.display = 'none';
                        puzzlePanel.style.display = 'flex'; 
                        shapeScaleLegend.style.display = 'none'; 
                    }}
                }});
            }});
        </script>
    </body>
    </html>"""

    mo.Html(f"""
    <div style="border-radius:8px; border:2px solid #ccc; overflow:hidden;">
        <iframe
            srcdoc="{map_html.replace('"', '&quot;')}"
            style="width:100%; height:650px; border:none; display:block;"
            scrolling="no"
            sandbox="allow-scripts allow-same-origin"
        ></iframe>
    </div>
    """)
    return


@app.cell
def _():
    return


if __name__ == "__main__":
    app.run()
