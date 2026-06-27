import marimo
__generated_with = "0.21.1"
app = marimo.App(width="medium")


@app.cell
def _():
    # 1. Imports needed for the project
    import marimo as mo
    import pandas as pd
    import numpy as np
    import plotly.graph_objects as go
    from sklearn.cluster import KMeans
    from sklearn.preprocessing import StandardScaler

    return StandardScaler, go, mo, np, pd


@app.cell
def _(np, pd):
    # 2. Load all 3 data files
    env = pd.read_csv('data_viz/oumalik_environmental_data.csv')
    soil = pd.read_csv('data_viz/oumalik_soil_data.csv')
    species_raw = pd.read_csv('data_viz/oumalik_species_data.csv', encoding='latin1', header=None)
    env = env.replace(-9999, np.nan)
    soil = soil.replace(-9999, np.nan)
    species_raw = species_raw.replace(-9999, np.nan)

    return env, soil, species_raw


@app.cell
def _(np, species_raw):
    # 3. Map the species into functional categories through the biological names
    # this was cross-checked to see whether the plant category in the files matches as a verification
    species_names = species_raw.iloc[3:, 0].values
    plot_ids_raw = species_raw.iloc[1, 3:].values.astype(float).astype(int)
    plot_ids = plot_ids_raw 
    species_matrix = np.nan_to_num(species_raw.iloc[3:, 3:].values.astype(float))

    abundance_map = {p: np.sum(species_matrix[:, i]) for i, p in enumerate(plot_ids)}

    def classify_botany(name):
        n = str(name).lower()
        if any(x in n for x in ['salix', 'betula', 'alnus']): return "Deciduous Shrubs"
        if any(x in n for x in ['dryas', 'cassiope', 'vaccinium', 'empetrum', 'andromeda', 'arctous', 'rhododendron']): return "Dwarf/Evergreen Shrubs"
        if any(x in n for x in ['carex', 'eriophorum', 'poa', 'festuca', 'arctagrostis', 'calamagrostis', 'juncus', 'kobresia', 'luzula']): return "Graminoids"
        if any(x in n for x in ['moss', 'sphagnum', 'aulacomnium', 'dicranum', 'hylocomium', 'polytrichum', 'bryum', 'calliergon', 'tomentypnum', 'sanionia']): return "Bryophytes (Mosses)"
        if any(x in n for x in ['lichen', 'cladonia', 'cetraria', 'peltigera', 'stereocaulon', 'flavocetraria', 'alectoria', 'thamnolia']): return "Lichens"
        return "Forbs & Others"

    functional_groups = [classify_botany(name) for name in species_names]
    group_order = ["Deciduous Shrubs", "Dwarf/Evergreen Shrubs", "Graminoids", "Bryophytes (Mosses)", "Lichens", "Forbs & Others"]
    return (
        abundance_map,
        functional_groups,
        group_order,
        plot_ids,
        species_matrix,
    )


@app.cell
def _(StandardScaler, pd, soil):
    # 4. PCA on the soil components 
    from sklearn.decomposition import PCA
    mineral_nutrients = ['NH4', 'NO3', 'N', 'P', 'K', 'Ca', 'Mg']
    humus_physical = ['organic_matter', 'pH', 'soil_moisture ', 'available_water']
    all_soil_f = list(set(mineral_nutrients + humus_physical))
    soil_clean = soil[['plot_number'] + all_soil_f].copy()
    # Fill NaNs with median 
    for col in all_soil_f:
        soil_clean[col] = soil_clean[col].fillna(soil_clean[col].median())
    
    # Standardise the variables before PCA
    scaler = StandardScaler()
    soil_z_scores = pd.DataFrame(scaler.fit_transform(soil_clean[all_soil_f]), columns=all_soil_f, index=soil_clean.index)
    pca = PCA(n_components=1)
    soil_clean['pca_axis'] = pca.fit_transform(soil_z_scores)
    median_val = soil_clean['pca_axis'].median()
    soil_clean['temp_cluster'] = (soil_clean['pca_axis'] > median_val).astype(int)

    # 5. Based on organic matter classify as Humus or Mineral rich
    # using a very simple process here because the main goal is just to identify the main differentiators between the soils in the plots 
    if soil_clean[soil_clean['temp_cluster'] == 1]['organic_matter'].mean() > soil_clean[soil_clean['temp_cluster'] == 0]['organic_matter'].mean():
        s_type_map = {1: "Humus-Rich", 0: "Mineral-Heavy"}
    else:
        s_type_map = {0: "Humus-Rich", 1: "Mineral-Heavy"}
    soil_clean['soil_type'] = soil_clean['temp_cluster'].map(s_type_map)

    # 6. Find dominant feature based on Z-score fluctuation
    def get_targeted_dominant(row):
        idx = row.name
        return soil_z_scores.loc[idx, mineral_nutrients].idxmax() if row['soil_type'] == "Mineral-Heavy" else soil_z_scores.loc[idx, humus_physical].idxmax()
    soil_clean['dominant_mineral'] = soil_clean.apply(get_targeted_dominant, axis=1)
    return (soil_clean,)


@app.cell
def _(abundance_map, env, np, plot_ids, soil_clean, species_matrix):
    # 7. Merge to create master df
    master_df = env.merge(soil_clean[['plot_number', 'soil_type', 'dominant_mineral']], on='plot_number', how='left')

    # 8. Fill missing soil types with NA to ensure we don't leave any plot out 
    master_df['soil_type'] = master_df['soil_type'].fillna("NA")
    master_df['dominant_mineral'] = master_df['dominant_mineral'].fillna("No Data")

    # 9. Add species diversity as richness 
    richness_map = {p: np.sum(species_matrix[:, i] > 0) for i, p in enumerate(plot_ids)}
    master_df['richness'] = master_df['plot_number'].map(richness_map).fillna(-1) #NO species data
    master_df['total_abundance'] = master_df['plot_number'].map(abundance_map).fillna(0)

    # 10. Create tiles based on the inverse disturbance score 
    master_df['tile_size'] = (10 - master_df['disturbance_score']) + 0.5
    master_df = master_df.sort_values('tile_size', ascending=False)
    master_df['tile_size'] = master_df['tile_size'].fillna(0.5) 
    #Force a minimum size for high disturbance plots
    master_df.loc[master_df['tile_size'] <= 0, 'tile_size'] = 0.5 

    return (master_df,)


@app.function
# 11. Recursive partioning to get a symmetric treemap
def get_rects(sizes, x, y, w, h):
    if not sizes: return []
    if len(sizes) == 1: return [{'x': x, 'y': y, 'dx': w, 'dy': h}]
    half = len(sizes) // 2
    s1, s2 = sum(sizes[:half]), sum(sizes[half:])
    total = s1 + s2
    if w > h:
        w1 = w * (s1 / total); return get_rects(sizes[:half], x, y, w1, h) + get_rects(sizes[half:], x + w1, y, w - w1, h)
    else:
        h1 = h * (s1 / total); return get_rects(sizes[:half], x, y, w, h1) + get_rects(sizes[half:], x, y + h1, w, h - h1)


@app.cell
def _(group_order, master_df, mo):
    # 12. UI Elements for interactivity
    substrate_filter = mo.ui.dropdown(
        options=['All Soils'] + sorted(master_df['soil_type'].unique().tolist()), 
        value='All Soils', label="Soil Groups"
    )
    dist_slider = mo.ui.slider(0, 12, step=0.1, value=12.0, label="Max Disturbance")
    min_rich_slider = mo.ui.slider(0, 100, step=1, value=0, label="Min Richness")

    max_abs = int(master_df['total_abundance'].max())
    abundance_range = mo.ui.range_slider(0, max_abs, step=1, value=[0, max_abs], label="Plant Abundance (Density)")

    plant_selector = mo.ui.multiselect(options=group_order, value=group_order, label="Visible Categories")
    return (
        abundance_range,
        dist_slider,
        min_rich_slider,
        plant_selector,
        substrate_filter,
    )


@app.cell
def _(
    abundance_range,
    dist_slider,
    functional_groups,
    go,
    group_order,
    master_df,
    min_rich_slider,
    mo,
    np,
    plant_selector,
    plot_ids,
    species_matrix,
    substrate_filter,
):
    # 13. Visualise

    petal_colors = ['#FF9999', '#FFCC99', '#FFFF99', '#99FF99', '#99CCFF', '#CC99FF']

    
    description_text = mo.md(
    """
    This dashboard visualizes **vegetative recovery** across the Oumalik zone, with each tile representing a different plot (with tile size being inversely proportional to the disturbance) and each flower explaining the ecological diversity of the plot. Users can dive deeper through the following options: 
    * **Soil Types:** Filter plots by their underlying soil-type which is dominant in mineral or organic substrate.
    * **Disturbance Limit:** Set a threshold to isolate plots based on disturbance impact.
    * **Abundance Filter:** Narrow down plots by the volume of vegetation present.
    * **Species Diversity:** Filter by the number of unique species found in the plot.
    * **Toggle Categories:** Show or hide specific plant groups to see their distribution patterns.
    """
                            )

    # Legend for the plant categories colour-coding
    legend_html = f"""
    <div style='background:#f4f4f4; padding:15px; border-radius:8px; border:1px solid #ddd; font-family:sans-serif;'>
        <div style='margin-bottom:10px;'>
            <b>Plant Categories:</b><br>
            {" ".join([f"<span style='display:inline-block; margin-right:12px;'><span style='display:inline-block; width:10px; height:10px; background:{petal_colors[i]}; border-radius:50%;'></span> {name}</span>" for i, name in enumerate(group_order)])}
        </div>
        <div>
            <b>Richness (Central Bud):</b> 
            <span style='margin-left:10px;'><span style='display:inline-block; width:10px; height:10px; background:#D3D3D3; border:1px solid #000; border-radius:50%;'></span> Low (<15)</span>
            <span style='margin-left:10px;'><span style='display:inline-block; width:10px; height:10px; background:#ADD8E6; border:1px solid #000; border-radius:50%;'></span> Mid (16-40)</span>
            <span style='margin-left:10px;'><span style='display:inline-block; width:10px; height:10px; background:#228B22; border:1px solid #000; border-radius:50%;'></span> High (>40)</span>
        </div>
    </div>
    """

    def render_viz(sub_val, max_dist, min_rich, abund_vals, selected_plants):
        df = master_df.copy()
    
        if sub_val != 'All Soils':
            df = df[df['soil_type'] == sub_val]
    
        # filtering logic for the sliders
        df['is_active'] = (
            (df['disturbance_score'].fillna(5.0) <= max_dist) & 
            (df['richness'].fillna(0) >= min_rich) &
            (df['total_abundance'] >= abund_vals[0]) &
            (df['total_abundance'] <= abund_vals[1])
        )

        rect_list = get_rects(df['tile_size'].tolist(), 0, 0, 100, 100)
        fig = go.Figure()

        for rect, (_, row) in zip(rect_list, df.iterrows()):
            p_id = int(row['plot_number'])
            active = row['is_active']
        
            # tooltip Data
            p_idx_list = np.where(plot_ids == p_id)[0]
            breakdown = ""
            if len(p_idx_list) > 0:
                p_idx = p_idx_list[0]
                cat_covers = [np.sum(species_matrix[[g == name for g in functional_groups], p_idx]) for name in group_order]
                total_plot_cov = sum(cat_covers) if sum(cat_covers) > 0 else 1
                for i, g_name in enumerate(group_order):
                    g_count = np.sum(species_matrix[[g == g_name for g in functional_groups], p_idx] > 0)
                    if g_count > 0:
                        breakdown += f"<br>• {g_name}: {g_count} species ({(cat_covers[i]/total_plot_cov*100):.1f}%)"

            full_hover = (
                f"<b>Plot {p_id}</b> ({row['soil_type']})<br>"
                f"<b>Disturbance:</b> {row['disturbance_score']:.2f}<br>"
                f"<b>Total Count:</b> {row['total_abundance']}<br>"
                f"<b>Dominant Factor:</b> {row['dominant_mineral']}<br>"            
                f"<b>Richness:</b> {row['richness']}<br>"
                f"<b>Composition:</b>{breakdown if breakdown else '<br>No species data'}"
            )

            # Drawing of Tiles
            tile_hue = max(0, min(120, (10 - row['disturbance_score']) * 12))
            tile_color = f'hsla({tile_hue}, 60%, 50%, 1.0)' if active else 'rgba(220, 220, 220, 0.2)'
        
            fig.add_trace(go.Scatter(
                x=[rect['x'], rect['x']+rect['dx'], rect['x']+rect['dx'], rect['x'], rect['x']],
                y=[rect['y'], rect['y'], rect['y']+rect['dy'], rect['y']+rect['dy'], rect['y']],
                fill="toself", line=dict(color="white", width=1), fillcolor=tile_color,
                hoverinfo="skip", showlegend=False
            ))

            # Plot Number Label 
            fig.add_trace(go.Scatter(
                x=[rect['x'] + 1.5], y=[rect['y'] + rect['dy'] - 1.5],
                mode="text", text=[str(p_id)], 
                textfont=dict(size=8, color="black" if active else "#888", family="Arial Black"),
                hoverinfo="skip", showlegend=False
            ))
        
            # Petals logic 
            cx, cy = rect['x'] + rect['dx']/2, rect['y'] + rect['dy']/2
            base_dim = max(0.5, min(abs(rect['dx']), abs(rect['dy'])))
            bud_r = base_dim * 0.15

            if active and len(p_idx_list) > 0:
                max_in_plot = max(cat_covers) if max(cat_covers) > 0 else 1
                for i, g_name in enumerate(group_order):
                    if g_name in selected_plants and cat_covers[i] > 0:
                        p_r = (np.sqrt(cat_covers[i] / max_in_plot)) * (base_dim * 0.18)
                        ang = np.deg2rad(i * 60)
                        px, py = cx + np.cos(ang)*(bud_r + p_r*0.3), cy + np.sin(ang)*(bud_r + p_r*0.3)
                        fig.add_trace(go.Scatter(
                            x=[px], y=[py], mode="markers",
                            marker=dict(size=max(2, p_r*22), color=petal_colors[i], line=dict(color="white", width=0.5)),
                            hoverinfo="skip", showlegend=False
                        ))

            # Central Bud logic
            rich = row['richness']
            bud_fill = "#D3D3D3" if rich <= 15 else ("#ADD8E6" if rich <= 40 else "#228B22")
            fig.add_trace(go.Scatter(
                x=[cx], y=[cy], mode="markers",
                marker=dict(size=max(3, bud_r*20), color=bud_fill if active else "white", line=dict(color="black", width=0.8)),
                hovertext=full_hover, hoverinfo="text", showlegend=False
            ))

        fig.update_layout(
        autosize=True,      
        height=850,         
        template="plotly_white", 
        xaxis_visible=False, 
        yaxis_visible=False, 
        margin=dict(t=0, b=0, l=0, r=0) 
    )
        return fig

    # UI 
    control_panel = mo.md(
        f"""
        <div style='background: #ffffff; padding: 15px; border: 1px solid #e0e0e0; border-radius: 10px; margin-bottom: 10px; width: 100%;'>
            <div style='display: flex; gap: 20px; flex-wrap: wrap;'>
                <div style='flex: 1;'><b>Soil:</b><br>{substrate_filter}</div>
                <div style='flex: 1;'><b>Max Disturbance:</b><br>{dist_slider}</div>
                <div style='flex: 1;'><b>Abundance:</b><br>{abundance_range}</div>
                <div style='flex: 1;'><b>Min Richness:</b><br>{min_rich_slider}</div>
            </div>
            <div style='margin-top: 15px;'><b>Toggles:</b><br>{plant_selector}</div>
        </div>
        """
    )


    viz_output = render_viz(
        substrate_filter.value, 
        dist_slider.value, 
        min_rich_slider.value, 
        abundance_range.value, 
        plant_selector.value
    )

    mo.vstack([
        mo.md(f"# Oumalik Ecology Dashboard"),
        control_panel,
        description_text,
        mo.Html(legend_html),
        mo.ui.plotly(render_viz(
            substrate_filter.value, 
            dist_slider.value, 
            min_rich_slider.value, 
            abundance_range.value, 
            plant_selector.value
        ))
    ], align="stretch")
    return


@app.cell
def _():
    return


if __name__ == "__main__":
    app.run()
