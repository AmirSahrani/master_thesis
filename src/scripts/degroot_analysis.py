import marimo

__generated_with = "0.12.0"
app = marimo.App(width="full")


@app.cell
def _():
    import marimo as mo
    import pandas as pd
    import numpy as np
    from scipy import stats
    import pingouin as pg
    import matplotlib.pyplot as plt
    import matplotlib.lines as mlines
    import seaborn as sns
    import altair as alt
    import arviz as az
    import pymc as pm

    from pymc import Model, Normal, sample
    return (
        Model,
        Normal,
        alt,
        az,
        mlines,
        mo,
        np,
        pd,
        pg,
        plt,
        pm,
        sample,
        sns,
        stats,
    )


@app.cell
def _(np, plt):
    plt.style.use("default")
    plt.rcParams.update(
        {
            "font.size": 20,
            "figure.figsize": [10, 8],
            "axes.linewidth": 1,
            "grid.linewidth": 1,
            "grid.alpha": 0.3,
            "image.cmap": "cividis",
            "text.usetex": True,
            "font.family": "Charter",
        }
    )



    def compute_proportion(data, col_start, col_end, new_col, group):
        # Ensure 'cyclic_start' and 'cyclic_end' are numeric
        data[col_start] = data[col_start].astype(float)
        data[col_end] = data[col_end].astype(float)

        # Group by bias
        aggregated_start = data.groupby(group)[col_start].mean()
        aggregated_end = data.groupby(group)[col_end].mean()

        # Compute proportion (avoid division by zero)
        agg_prop = (aggregated_end / aggregated_start).replace(np.nan, 0)

        # Convert Series to DataFrame and reset index
        return aggregated_end.reset_index(name=new_col)


    def compute_average(data, col_start, new_col, group):
        # Ensure 'cyclic_start' and 'cyclic_end' are numeric
        data[col_start] = data[col_start].astype(float)

        # Group by bias
        aggregated_start = data.groupby(group)[col_start].mean()

        # Convert Series to DataFrame and reset index
        return aggregated_start.reset_index(name=new_col)


    def compute_percentage_change(data, col_start, col_end, new_col, group):
        # Ensure 'cyclic_start' and 'cyclic_end' are numeric
        data[col_start] = data[col_start].astype(float)
        data[col_end] = data[col_end].astype(float)

        # Group by bias
        aggregated_start = data.groupby(group)[col_start].mean()
        aggregated_end = data.groupby(group)[col_end].mean()

        # Compute proportion (avoid division by zero)
        agg_prop = (
            (aggregated_end - aggregated_start) / aggregated_start
        ).replace(np.nan, 0)

        # Convert Series to DataFrame and reset index
        return agg_prop.reset_index(name=new_col)
    return compute_average, compute_percentage_change, compute_proportion


@app.cell
def _(compute_proportion, mlines, pd, plt):
    def plot(df, x, y, ylabel):
        fig, ax = plt.subplots()

        # Define markers for each sampler (cycling if needed)
        marker_styles = ["o", "X", "^", "D", "v", "P", "X", "*"]
        sampler_markers = {
            sampler: marker_styles[i % len(marker_styles)]
            for i, sampler in enumerate(df["cand_sampler"].unique())
        }

        # Define colors for 'Type' — consistent ordering
        type_colors = {"Start": "#A93C93", "End": "#008B72", "Final": "#613F99"}

        for sampler in df["cand_sampler"].unique():
            for typ in df["Type"].unique():
                subset = df[(df["cand_sampler"] == sampler) & (df["Type"] == typ)]
                if not subset.empty:
                    ax.plot(
                        subset[x],
                        subset[y],
                        label=f"{typ} / {sampler}",
                        color=type_colors.get(typ, "black"),
                        marker=sampler_markers[sampler],
                        markersize=10,
                        linestyle="--",
                        alpha=0.4,
                    )

        ax.set_xlabel(x)
        ax.set_ylabel(ylabel)
        ax.set_title(ylabel)
        ax.grid(True)
        type_handles = [
            mlines.Line2D(
                [], [], color=color, marker="o", linestyle="-", label=typ
            )
            for typ, color in type_colors.items()
        ]

        # Legend for Sampler (marker)
        sampler_handles = [
            mlines.Line2D(
                [],
                [],
                color="black",
                marker=marker,
                markersize=10,
                linestyle="None",
                label=sampler,
            )
            for sampler, marker in sampler_markers.items()
        ]

        # Combine and show
        legend1 = ax.legend(
            handles=type_handles + sampler_handles, title="Type (color),\nSampler (marker)", loc="upper left"
        )

        ax.add_artist(legend1)  # Keep both legends visible
        plt.tight_layout()
        plt.show()

    def compute_and_merge_proportions(
        data, start_col, end_col, true_col, name, group_by
    ):
        df_start = compute_proportion(data, start_col, start_col, name, group_by)
        df_end = compute_proportion(data, start_col, end_col, name, group_by)
        df_true = compute_proportion(data, start_col, true_col, name, group_by)

        df_start["Type"] = "Start"
        df_end["Type"] = "End"
        df_true["Type"] = "Final"
        df_combined = pd.concat([df_start, df_end, df_true])

        return df_combined
    return compute_and_merge_proportions, plot


@app.cell
def _(np, pd):
    data_delib = pd.read_csv("results/degroot_deliberation_100.csv")
    data_control= pd.read_csv("results/degroot_control_100.csv", index_col=False)

    prox_cols = ["proximity_to_cand_sp_" + x for x in ["start", "end", "true"]]
    prox_voter_cols = ["proximity_to_voter_sp_" + x for x in ["start", "end", "true"]]

    for c in prox_cols:
        data_control[c] = data_control[c] /data_control["n_candidates"]
        data_delib[c] = data_delib[c] /data_delib["n_candidates"]

    for c in prox_voter_cols:
        data_control[c] = data_control[c].replace(-1, np.nan)
        data_delib[c] = data_delib[c].replace(-1, np.nan)
        data_control[c] = data_control[c] /data_control["n_voters"]
        data_delib[c] = data_delib[c] /data_delib["n_voters"]

    print(data_delib)
    return c, data_control, data_delib, prox_cols, prox_voter_cols


@app.cell
def _(data_delib):
    voter_str = "n_voters"
    cand_str = "n_candidates"
    bias_str = "bias"
    time_str = "time_steps"
    sampler_str = "cand_sampler"
    voter_df = {x: data_delib.loc[data_delib[voter_str] == x ] for x in data_delib[voter_str].unique()}
    cand_df = {x: data_delib.loc[data_delib[cand_str] == x ] for x in data_delib[cand_str].unique()}
    time_df = {x: data_delib.loc[data_delib[time_str] == x ] for x in data_delib[time_str].unique()}
    measurements = ['cyclic_start', 'condorcet_start', 'unique_start',  'cyclic_end', 'condorcet_end', 'unique_end',  'cyclic_true', 'condorcet_true', 'unique_true' ]
    print(voter_df.keys())
    print(cand_df.keys())
    print(time_df.keys())
    print(measurements)
    return (
        bias_str,
        cand_df,
        cand_str,
        measurements,
        sampler_str,
        time_df,
        time_str,
        voter_df,
        voter_str,
    )


@app.cell
def _(compute_and_merge_proportions, compute_average, data_delib, pd, plot):
    # data_delib_51_5 = data_delib.loc[(data_delib[voter_str] == 11) & (data_delib[cand_str] == 5) & (data_delib[time_str] == 51.)].copy()
    data_delib_51_5 = data_delib

    cyclic_51_5 = compute_and_merge_proportions(
        data_delib_51_5,
        "cyclic_start",
        "cyclic_end",
        "cyclic_true",
        "cyclic_proportion",
        ["bias", "cand_sampler"],
    )


    condorcet_51_5 = compute_and_merge_proportions(
        data_delib_51_5,
        "condorcet_start",
        "condorcet_end",
        "condorcet_true",
        "condorcet_proportion",
        ["bias", "cand_sampler"],
    )

    proximity_to_sp_end_51_5 = compute_average(
        data_delib_51_5, "proximity_to_cand_sp_end", "proximity_to_cand_sp", ["bias", "cand_sampler"]
    )
    proximity_to_sp_true_51_5 = compute_average(
        data_delib_51_5, "proximity_to_cand_sp_true", "proximity_to_cand_sp", ["bias", "cand_sampler"]
    )
    proximity_to_sp_end_51_5["Type"] = "End"
    proximity_to_sp_true_51_5["Type"] = "True"
    df_combined_51_5 = pd.concat([proximity_to_sp_end_51_5, proximity_to_sp_true_51_5])
    proximity_to_sp_51_5 = df_combined_51_5.rename(
        columns={"proximity_to_start": "proximity_to_cand_sp"}
    )

    proximity_to_voter_sp_end_51_5 = compute_average(
        data_delib_51_5, "proximity_to_voter_sp_end", "proximity_to_voter_sp", ["bias", "cand_sampler"]
    )
    proximity_to_voter_sp_true_51_5 = compute_average(
        data_delib_51_5, "proximity_to_voter_sp_true", "proximity_to_voter_sp", ["bias", "cand_sampler"]
    )
    proximity_to_voter_sp_end_51_5["Type"] = "End"
    proximity_to_voter_sp_true_51_5["Type"] = "True"
    df_combined_51_5 = pd.concat([proximity_to_voter_sp_end_51_5, proximity_to_voter_sp_true_51_5])
    proximity_to_voter_sp_51_5 = df_combined_51_5.rename(
        columns={"proximity_to_start": "proximity_to_voter_sp"}
    )

    unique_profiles_end_51_5 = compute_average(
        data_delib_51_5, "unique_end", "unique", ["bias", "cand_sampler"]
    )
    unique_profiles_true_51_5 = compute_average(
        data_delib_51_5, "unique_true", "unique", ["bias", "cand_sampler"]
    )
    unique_profiles_end_51_5["Type"] = "End"
    unique_profiles_true_51_5["Type"] = "True"
    df_combined_51_5 = pd.concat([unique_profiles_end_51_5, unique_profiles_true_51_5])
    unique_profiles_51_5 = df_combined_51_5.rename(
        columns={"unique_start": "unique_profiles"}
    )

    # === Plotting all variants in one figure ===
    plot(cyclic_51_5, "bias", "cyclic_proportion", "Mean Number of Cyclic Profiles")
    plot(proximity_to_sp_51_5, "bias", "proximity_to_cand_sp", "Mean candidate proximity to single peaked Profiles")
    plot(proximity_to_voter_sp_51_5, "bias", "proximity_to_voter_sp", "Mean voter proximity to single peaked Profiles")
    plot(condorcet_51_5, "bias", "condorcet_proportion", "Mean number of Condorcet winners")
    plot(unique_profiles_51_5, "bias", "unique", r"\#Unique Preferences")
    return (
        condorcet_51_5,
        cyclic_51_5,
        data_delib_51_5,
        df_combined_51_5,
        proximity_to_sp_51_5,
        proximity_to_sp_end_51_5,
        proximity_to_sp_true_51_5,
        proximity_to_voter_sp_51_5,
        proximity_to_voter_sp_end_51_5,
        proximity_to_voter_sp_true_51_5,
        unique_profiles_51_5,
        unique_profiles_end_51_5,
        unique_profiles_true_51_5,
    )


@app.cell(hide_code=True)
def _(cand_str, data_delib, mo, time_str, voter_str):
    # Create UI controls
    voter_dropdown = mo.ui.dropdown(
        options={str(v): v for v in sorted(data_delib[voter_str].unique())},
        value="9",
        label="Number of Voters"
    )

    cand_dropdown = mo.ui.dropdown(
        options={str(c): c for c in sorted(data_delib[cand_str].unique())},
        value="7",
        label="Number of Candidates"
    )

    time_dropdown = mo.ui.dropdown(
        options={str(t): t for t in sorted(data_delib[time_str].unique())},
        value="1.0",
        label="Time Value"
    )

    # Display UI controls
    controls = mo.hstack([voter_dropdown, cand_dropdown, time_dropdown])
    return cand_dropdown, controls, time_dropdown, voter_dropdown


@app.cell(hide_code=True)
def _(cand_str, data_control, mo, time_str, voter_str):
    # Create UI controls
    voter_dropdown_c = mo.ui.dropdown(
        options={str(v): v for v in sorted(data_control[voter_str].unique())},
        value="31",
        label="Number of Voters"
    )

    cand_dropdown_c = mo.ui.dropdown(
        options={str(c): c for c in sorted(data_control[cand_str].unique())},
        value="5",
        label="Number of Candidates"
    )

    time_dropdown_c = mo.ui.dropdown(
        options={str(t): t for t in sorted(data_control[time_str].unique())},
        value="1.0",
        label="Time Value"
    )

    # Display UI controls
    controls_control = mo.hstack([voter_dropdown_c, cand_dropdown_c, time_dropdown_c])
    return cand_dropdown_c, controls_control, time_dropdown_c, voter_dropdown_c


@app.cell
def _(alt):
    def create_altair_chart(df, x_col, y_col, title):
        # Define color scale for different Type values
        type_colors = {"Start": "#A93C93", "End": "#008B72", "Final": "#613F99"}

        # Define markers for different cand_sampler values
        markers = ["circle", "cross", "triangle", "diamond", "triangle-down", "square", "cross", "star"]
        sampler_markers = {
            sampler: markers[i % len(markers)]
            for i, sampler in enumerate(df["cand_sampler"].unique())
        }

        # Create the chart with proper encoding
        chart = alt.Chart(df).mark_point().encode(
            x=alt.X(f'{x_col}:Q', title=x_col),
            y=alt.Y(f'{y_col}:Q', title=title),
            color=alt.Color('Type:N', 
                           scale=alt.Scale(domain=list(type_colors.keys()), 
                                          range=list(type_colors.values())),
                           title='Type'),
            shape=alt.Shape('cand_sampler:N', 
                          scale=alt.Scale(domain=list(sampler_markers.keys()),
                                         range=list(sampler_markers.values())),
                          title='Candidate Sampler'),
            tooltip=['bias', 'cand_sampler', 'Type', y_col]
        )

        # Add connecting lines, grouped by both Type and cand_sampler
        lines = alt.Chart(df).mark_line(
            strokeDash=[4, 4],
            opacity=0.7
        ).encode(
            x=alt.X(f'{x_col}:Q'),
            y=alt.Y(f'{y_col}:Q'),
            color=alt.Color('Type:N'),
            detail='cand_sampler:N'  # Group lines by sampler too
        )

        # Combine points and lines
        combined = (lines + chart).properties(
            width=800,
            height=500,
            title=title
        ).interactive()

        return combined
    return (create_altair_chart,)


@app.cell
def _(
    cand_dropdown,
    cand_str,
    compute_and_merge_proportions,
    compute_average,
    controls,
    create_altair_chart,
    data_delib,
    mo,
    pd,
    time_dropdown,
    time_str,
    voter_dropdown,
    voter_str,
):
    # Get values from dropdowns
    voter_value = int(voter_dropdown.value)
    cand_value = int(cand_dropdown.value)
    time_value = int(time_dropdown.value)

    # Filter data_delib based on selections
    filtered_data_delib = data_delib.loc[
        (data_delib[voter_str] == voter_value) & 
        (data_delib[cand_str] == cand_value) & 
        (data_delib[time_str] == time_value)
    ].copy()

    # Get the processed data_delibsets using your existing helper functions
    # Assuming these functions already handle the "Type" column correctly
    cyclic = compute_and_merge_proportions(
        filtered_data_delib,
        "cyclic_start", "cyclic_end", "cyclic_true", "cyclic_proportion",
        ["bias", "cand_sampler"]
    )

    condorcet = compute_and_merge_proportions(
        filtered_data_delib,
        "condorcet_start", "condorcet_end", "condorcet_true", "condorcet_proportion",
        ["bias", "cand_sampler"]
    )

    proximity_to_cand_sp_profiles_start = compute_average(
        filtered_data_delib, "proximity_to_cand_sp_start", "proximity_to_cand_sp", ["bias", "cand_sampler"]
    )
    proximity_to_cand_sp_profiles_start["Type"] = "Start"
    proximity_to_cand_sp_profiles_end = compute_average(
        filtered_data_delib, "proximity_to_cand_sp_end", "proximity_to_cand_sp", ["bias", "cand_sampler"]
    )
    proximity_to_cand_sp_profiles_end["Type"] = "End"

    proximity_to_cand_sp_profiles_true = compute_average(
        filtered_data_delib, "proximity_to_cand_sp_true", "proximity_to_cand_sp", ["bias", "cand_sampler"]
    )
    proximity_to_cand_sp_profiles_true["Type"] = "Final"

    proximity_to_cand_sp_profiles = pd.concat([proximity_to_cand_sp_profiles_start, proximity_to_cand_sp_profiles_end, proximity_to_cand_sp_profiles_true])

    unique_profiles_start = compute_average(
        filtered_data_delib, "unique_start", "unique", ["bias", "cand_sampler"]
    )
    unique_profiles_start["Type"] = "Start"

    unique_profiles_end = compute_average(
        filtered_data_delib, "unique_end", "unique", ["bias", "cand_sampler"]
    )
    unique_profiles_end["Type"] = "End"

    unique_profiles_true = compute_average(
        filtered_data_delib, "unique_true", "unique", ["bias", "cand_sampler"]
    )
    unique_profiles_true["Type"] = "Final"

    unique_profiles = pd.concat([unique_profiles_start,unique_profiles_end, unique_profiles_true])

    # Create the charts
    # You'll need to specify the correct column names based on your actual data_delib structure
    chart1 = create_altair_chart(cyclic, "bias", "cyclic_proportion", "Mean Number of Cyclic Profiles")
    chart2 = create_altair_chart(proximity_to_cand_sp_profiles,"bias", "proximity_to_cand_sp", "Mean candidate proximity to SP")
    chart3 = create_altair_chart(condorcet, "bias", "condorcet_proportion", "Mean Number of Condorcet Winners")
    chart4 = create_altair_chart(unique_profiles, "bias", "unique", "#Unique Preferences")

    # Display title and charts
    mo.center(mo.vstack([
        mo.center(mo.md("#Results for Deliberation")),
        mo.center(controls),
    mo.hstack([chart3, chart4]),
    mo.hstack([chart1, chart2])]))
    return (
        cand_value,
        chart1,
        chart2,
        chart3,
        chart4,
        condorcet,
        cyclic,
        filtered_data_delib,
        proximity_to_cand_sp_profiles,
        proximity_to_cand_sp_profiles_end,
        proximity_to_cand_sp_profiles_start,
        proximity_to_cand_sp_profiles_true,
        time_value,
        unique_profiles,
        unique_profiles_end,
        unique_profiles_start,
        unique_profiles_true,
        voter_value,
    )


@app.cell(hide_code=True)
def _(
    cand_dropdown_c,
    cand_str,
    compute_and_merge_proportions,
    compute_average,
    controls_control,
    create_altair_chart,
    data_control,
    mo,
    pd,
    time_dropdown_c,
    time_str,
    voter_dropdown_c,
    voter_str,
):
    # Get values from dropdowns
    voter_value_c = int(voter_dropdown_c.value)
    cand_value_c = int(cand_dropdown_c.value)
    time_value_c = int(time_dropdown_c.value)

    # Filter data_control based on selections
    filtered_data_control = data_control.loc[
        (data_control[voter_str] == voter_value_c) & 
        (data_control[cand_str] == cand_value_c) & 
        (data_control[time_str] == time_value_c)
    ].copy()

    # Get the processed data_controlsets using your existing helper functions
    # Assuming these functions already handle the "Type" column correctly
    cyclic_c = compute_and_merge_proportions(
        filtered_data_control,
        "cyclic_start", "cyclic_end", "cyclic_true", "cyclic_proportion",
        ["bias", "cand_sampler"]
    )


    condorcet_c = compute_and_merge_proportions(
        filtered_data_control,
        "condorcet_start", "condorcet_end", "condorcet_true", "condorcet_proportion",
        ["bias", "cand_sampler"]
    )


    proximity_to_cand_sp_start_c = compute_average(
        filtered_data_control, "proximity_to_cand_sp_start", "proximity_to_cand_sp", ["bias", "cand_sampler"]
    )
    proximity_to_cand_sp_start_c["Type"] = "Start"

    proximity_to_cand_sp_end_c = compute_average(
        filtered_data_control, "proximity_to_cand_sp_end", "proximity_to_cand_sp", ["bias", "cand_sampler"]
    )
    proximity_to_cand_sp_end_c["Type"] = "End"

    proximity_to_cand_sp_true_c = compute_average(
        filtered_data_control, "proximity_to_cand_sp_true", "proximity_to_cand_sp", ["bias", "cand_sampler"]
    )
    proximity_to_cand_sp_true_c["Type"] = "Final"

    proximity_to_cand_sp_c = pd.concat([proximity_to_cand_sp_start_c, proximity_to_cand_sp_end_c, proximity_to_cand_sp_true_c])

    unique_profiles_start_c = compute_average(
        filtered_data_control, "unique_start", "unique", ["bias", "cand_sampler"]
    )
    unique_profiles_start_c["Type"] = "Start"

    unique_profiles_end_c = compute_average(
        filtered_data_control, "unique_end", "unique", ["bias", "cand_sampler"]
    )
    unique_profiles_end_c["Type"] = "End"

    unique_profiles_true_c = compute_average(
        filtered_data_control, "unique_true", "unique", ["bias", "cand_sampler"]
    )
    unique_profiles_true_c["Type"] = "Final"

    unique_profiles_c = pd.concat([unique_profiles_start_c, unique_profiles_end_c, unique_profiles_true_c])

    # Create the charts
    # You'll need to specify the correct column names based on your actual data_control structure
    chart1_c = create_altair_chart(cyclic_c, "bias", "cyclic_proportion", "Mean Number of Cyclic Profiles")
    chart2_c = create_altair_chart(proximity_to_cand_sp_c, "bias", "proximity_to_cand_sp", "#Unique Preferences")
    chart3_c = create_altair_chart(condorcet_c, "bias", "condorcet_proportion", "Mean Number of Condorcet Winners")
    chart4_c = create_altair_chart(unique_profiles_c, "bias", "unique", "#Unique Preferences")

    # Display title and charts
    mo.center(mo.vstack([
        mo.center(mo.md("#Results for Control")),
        mo.center(controls_control),
    mo.hstack([chart3_c, chart4_c]),
    mo.hstack([chart1_c, chart2_c])]))
    return (
        cand_value_c,
        chart1_c,
        chart2_c,
        chart3_c,
        chart4_c,
        condorcet_c,
        cyclic_c,
        filtered_data_control,
        proximity_to_cand_sp_c,
        proximity_to_cand_sp_end_c,
        proximity_to_cand_sp_start_c,
        proximity_to_cand_sp_true_c,
        time_value_c,
        unique_profiles_c,
        unique_profiles_end_c,
        unique_profiles_start_c,
        unique_profiles_true_c,
        voter_value_c,
    )


@app.cell
def _(mo):
    mo.md(
        r"""
        # Statistical Analysis
        We now proceed to analyze fit of this model compared to the `final` data. Here `final` is the data of the voters in the second time measurement.
        """
    )
    return


@app.cell
def _(bias_str, data_delib, measurements, mo, time_str):
    def describe_data (data):
        bool_cols = [x for x in data.columns if data[x].dtype == bool]
        for c in bool_cols:
            data[c] = data[c].astype(int)
        grouped_time= data.groupby([time_str, bias_str])[measurements].agg(["mean", "sum"])
        return mo.ui.dataframe(grouped_time, page_size=24)

    describe_data(data_delib)
    return (describe_data,)


@app.cell
def _(data_control, describe_data):
    describe_data(data_control)
    return


@app.cell
def _(data_control, data_delib):
    def normalize(df):
        return (df - df.min() )/ df.max()

    # Compute differences for each metric
    def diff_data_on_metrics(df):
        metrics = ['cyclic', 'condorcet', 'unique', 'proximity_to_cand_sp', 'proximity_to_voter_sp']
        for m in metrics:
            df[f'{m}_diff'] = normalize(df[f'{m}_end']) - normalize(df[f'{m}_true'])
            df[f'{m}_absdiff'] = df[f'{m}_diff']**2

        # Example: combine into a total error score (sum of absolute differences)
        df['total_absdiff'] = df[[f'{m}_absdiff' for m in metrics]].sum(axis=1)
        return df.groupby(['bias','cand_sampler','n_voters','n_candidates','time_steps']) \
                ['total_absdiff'].agg(['mean','std']).reset_index()

    summary_delib = diff_data_on_metrics(data_delib)
    summary_control = diff_data_on_metrics(data_control)
    summary_delib
    return diff_data_on_metrics, normalize, summary_control, summary_delib


@app.cell
def _(bias_str, plt, summary_control, summary_delib):
    def plot_summary(summary):
        lab = [int(x) for x in summary["time_steps"].unique().tolist()]
        scatter = plt.scatter(summary[bias_str], summary["mean"], c=summary["time_steps"])
        plt.xlabel("Bias")
        plt.ylabel("Total Absolute difference")
        legend = plt.legend(
            handles=scatter.legend_elements()[0],
            labels=lab,
            title="Time step",
            loc="upper right",
            bbox_to_anchor=(1.24, 0.8),  # center it under the plot
            ncol= 1,
        )

        plt.show()
    plot_summary(summary_delib)
    plot_summary(summary_control)
    return (plot_summary,)


@app.cell
def _(summary_control, summary_delib):
    for i in range (3,8):
        sub_s = summary_control.loc[summary_control["n_candidates"] == i].reset_index()
        print(sub_s.iloc[sub_s["mean"].idxmin()].to_markdown())

    print("----------------------")
    for i in range (3,8,2):
        sub_d = summary_delib.loc[summary_delib["n_candidates"] == i].reset_index()
        print(sub_d.iloc[sub_d["mean"].idxmin()].to_markdown())
    return i, sub_d, sub_s


@app.cell(hide_code=True)
def _(mo):
    mo.md(
        r"""
        From this, we see that a bias of 1.3 seems to perform best for the deliberation group. For the control group, however, it seems that people have a weaker bias of 0.5. This seems to indicate that people take their opinion to be about 1.3 more important than the opinion of all other voters when deliberating.  For the control group, it seems a less bias is needed, this might be a result of voters talking more to like-minded people, and therefore need to be less strict about their own opinion.

        We also note that as we increase the deliberation time, we get closer to the original preferences. We must note, however, that if we do not normalize the different outcomes, then a bias of 1.1-1.4 become the best for both control and deliberation.

        Finally, it seems that the `Sample` method for generating alternatives is most successful for the control group, while the `voter` method is best under the deliberation group.
        """
    )
    return


@app.cell
def _(summary_delib):
    # Step 1: Filter for time_steps == 151
    df_151 = summary_delib[summary_delib["time_steps"] == 151]

    # Step 2: For each group, find the row with the minimum 'mean'
    idx_min = df_151.groupby(
        ["n_candidates", "n_voters", "cand_sampler"]
    )["mean"].idxmin()

    # Step 3: Use the indices to get the full rows
    min_rows = df_151.loc[idx_min].copy()

    # Step 4: Pivot the dataframe so each 'cand_sampler' becomes columns for both 'mean' and 'bias'
    pivoted = min_rows.pivot(
        index=["n_candidates", "n_voters"],
        columns="cand_sampler",
        values=["mean", "bias"]
    ).reset_index()

    # Optional: flatten MultiIndex columns
    pivoted.columns = ['_'.join(col).strip('_') for col in pivoted.columns.values]

    print(pivoted.head(10))
    return df_151, idx_min, min_rows, pivoted


@app.cell
def _(pd):
    convergence_data = pd.read_csv("results/degroot_deliberation_100_convergence_groups.csv")
    convergence_data["entrywise_distance"] = convergence_data["entrywise_distance"].apply(
        lambda x: x if x != 0  else None
    )
    convergence_data = convergence_data.bfill()
    convergence_data
    return (convergence_data,)


@app.cell
def _(cand_str, convergence_data, sampler_str, time_str):
    grouped_by_cand_and_sampler = convergence_data.groupby([cand_str, sampler_str, time_str]).agg("mean")
    print(grouped_by_cand_and_sampler)
    return (grouped_by_cand_and_sampler,)


@app.cell
def _(grouped_by_cand_and_sampler, np, plt):
    df_reset = grouped_by_cand_and_sampler.reset_index()

    # Unique values for styling
    candidate_counts = sorted(df_reset['n_candidates'].unique())
    sampler_styles = {'Sample': 'X', 'Voter': 's'}
    colors = plt.cm.viridis_r(np.linspace(0, 1, len(candidate_counts)))

    # Begin plotting
    fig, ax = plt.subplots(3, 1, figsize=(15, 15), sharex=True)

    # Plot each combination
    for idx, n in enumerate(candidate_counts):
        for sampler, marker in sampler_styles.items():
            subset = df_reset[(df_reset['n_candidates'] == n) & (df_reset['cand_sampler'] == sampler)]
            if not subset.empty:
                ax[0].plot(subset['time_steps'], subset['ks_distance_true'], 
                           label=f'{n} candidates, {sampler}', 
                           marker=marker, color=colors[idx], linestyle='-')
                ax[1].plot(subset['time_steps'], subset['cs_distance_true'], 
                           label=f'{n} candidates, {sampler}', 
                           marker=marker, color=colors[idx], linestyle='-')
                ax[2].plot(subset['time_steps'], subset['entrywise_distance'], 
                           label=f'{n} candidates, {sampler}', 
                           marker=marker, color=colors[idx], linestyle='-')

    # Labels and titles
    ax[0].set_ylabel('KS Distance')
    ax[1].set_ylabel('CS Distance')
    ax[1].set_xlabel('Time Steps')
    ax[2].set_ylabel('Entrywise distance')
    ax[2].set_xlabel('Time Steps')

    # Legend
    handles, labels = ax[0].get_legend_handles_labels()
    fig.legend(handles, labels, loc='lower center', ncol=3,
               bbox_to_anchor=(0.51, -0.05))

    # plt.tight_layout(rect=[0, 0, 1, 0.95])
    plt.show()
    return (
        ax,
        candidate_counts,
        colors,
        df_reset,
        fig,
        handles,
        idx,
        labels,
        marker,
        n,
        sampler,
        sampler_styles,
        subset,
    )


@app.cell
def _(mo):
    mo.md(
        r"""
        ## Parameter estimation

        We now use `pymc` to estimate the most likely parameters for the deliberation and the control group, conditional on the number of voters and candidates, where the goal is to get the probability distribution of parameters that minimizes the `total_absdiff
        """
    )
    return


@app.cell
def _():
    return


@app.cell
def _(az, data_delib, np, plt, pm, sns):
    def pymc_model(data):# Convert categorical variables to numeric for the model
        data['cand_sampler_numeric'] = data['cand_sampler'].map({'Sample': 0, 'Voter': 1})
    
        # Prepare data for modeling - focus on key parameters
        # We'll model how bias, n_voters, n_candidates, and sampler type affect total_absdiff
        model_data = data[['bias', 'n_voters', 'n_candidates', 'cand_sampler_numeric', 'total_absdiff']].copy()
    
        # Remove duplicate parameter combinations to get unique configurations
        unique_params = model_data.drop_duplicates(subset=['bias', 'n_voters', 'n_candidates', 'cand_sampler_numeric'])
        print(f"Number of unique parameter combinations: {len(unique_params)}")
    
        # Group by the parameter combinations and compute mean total_absdiff
        grouped_data = model_data.groupby(['bias', 'n_voters', 'n_candidates', 'cand_sampler_numeric'])['total_absdiff'].mean().reset_index()
        print(f"Grouped data shape: {grouped_data.shape}")
    
        # Scale the features for better model convergence
        from sklearn.preprocessing import StandardScaler
        scaler = StandardScaler()
        X = grouped_data[['bias', 'n_voters', 'n_candidates', 'cand_sampler_numeric']].values
        X_scaled = scaler.fit_transform(X)
        y = grouped_data['total_absdiff'].values
    
        # Build PyMC model
        with pm.Model() as model:
            voters = pm.Data('n_voters', data['n_voters'])
            candidates = pm.Data('n_candidates', data['n_candidates'])

            intercept = pm.Normal('intercept', mu=0, sigma=1)
            beta_bias = pm.TruncatedNormal('beta_bias', lower=0, upper=3, mu=0, sigma=1)
            beta_n_voters = pm.Normal('beta_n_voters', mu=0, sigma=1)
            beta_n_candidates = pm.Normal('beta_n_candidates', mu=0, sigma=1)
            beta_sampler = pm.Bernoulli('beta_sampler', p=0.5)
        
            # Interaction terms
            beta_bias_voters = pm.Normal('beta_bias_voters', mu=0, sigma=0.5)
            beta_bias_candidates = pm.Normal('beta_bias_candidates', mu=0, sigma=0.5)
            beta_bias_sampler = pm.Normal('beta_bias_sampler', mu=0, sigma=0.5)
        
            # Model error
            sigma = pm.HalfNormal('sigma', sigma=1)
        
            mu = (intercept + 
              beta_bias * X_scaled[:, 0] + 
              beta_n_voters * X_scaled[:, 1] + 
              beta_n_candidates * X_scaled[:, 2] + 
              beta_sampler * X_scaled[:, 3] +
              beta_bias_voters * X_scaled[:, 0] * X_scaled[:, 1] +
              beta_bias_candidates * X_scaled[:, 0] * X_scaled[:, 2] +
              beta_bias_sampler * X_scaled[:, 0] * X_scaled[:, 3])
        
            # Likelihood
            likelihood = pm.Normal('likelihood', mu=mu, sigma=sigma, observed=y)
        
            # Sample from the posterior
            trace = pm.sample(2000, tune=1000, return_inferencedata=True)
    
        # Analyze the results
        summary = az.summary(trace)
        az.plot_trace(trace)
        plt.show()
        print("Model parameter summary:")
        print(summary)
    
        # Plot the posterior distributions
        az.plot_posterior(trace)
        plt.tight_layout()
        plt.savefig('parameter_posteriors.png')
    
        # Calculate the R-squared to assess model fit
        y_pred = trace.posterior['intercept'].mean().item()
        for i, param in enumerate(['beta_bias', 'beta_n_voters', 'beta_n_candidates', 'beta_sampler', 
                                 'beta_bias_voters', 'beta_bias_candidates', 'beta_bias_sampler']):
            if i < 4:  # Main effects
                y_pred += trace.posterior[param].mean().item() * X_scaled[:, i % 4]
            else:  # Interaction terms
                idx1 = 0  # bias is always the first component of interactions
                idx2 = i - 3  # the other component
                y_pred += trace.posterior[param].mean().item() * X_scaled[:, idx1] * X_scaled[:, idx2]
    
        ss_total = np.sum((y - np.mean(y))**2)
        ss_residual = np.sum((y - y_pred)**2)
        r_squared = 1 - (ss_residual / ss_total)
        print(f"R-squared: {r_squared:.4f}")
    
        # Find optimal parameters that minimize total_absdiff
        # For each sampler type
        samplers = ['Sample', 'Voter']
        sampler_nums = [0, 1]
    
        for sampler, sampler_num in zip(samplers, sampler_nums):
            print(f"\nOptimal parameters for {sampler} sampler:")
        
            # Create a grid of parameter values
            biases = np.unique(data['bias'])
            n_voters_values = np.unique(data['n_voters'])
            n_candidates_values = np.unique(data['n_candidates'])
        
            best_params = None
            lowest_absdiff = float('inf')
        
            # Filter data for this sampler
            sampler_data = data[data['cand_sampler'] == sampler]
        
            for bias in biases:
                for n_voters in n_voters_values:
                    for n_candidates in n_candidates_values:
                        subset = sampler_data[(sampler_data['bias'] == bias) & 
                                            (sampler_data['n_voters'] == n_voters) & 
                                            (sampler_data['n_candidates'] == n_candidates)]
                    
                        if len(subset) > 0:
                            mean_absdiff = subset['total_absdiff'].mean()
                        
                            if mean_absdiff < lowest_absdiff:
                                lowest_absdiff = mean_absdiff
                                best_params = (bias, n_voters, n_candidates)
        
            if best_params:
                print(f"Bias: {best_params[0]}, N_voters: {best_params[1]}, N_candidates: {best_params[2]}")
                print(f"Minimum total_absdiff: {lowest_absdiff:.6f}")
    
        # Create more detailed visualizations
        # Plot relationship between parameters and total_absdiff for each sampler
        plt.figure(figsize=(15, 10))
    
        # Plot bias vs total_absdiff for different voter counts
        plt.subplot(2, 2, 1)
        for n_voters in np.unique(data['n_voters']):
            subset = data[data['n_voters'] == n_voters]
            sns.lineplot(x='bias', y='total_absdiff', data=subset, label=f'Voters: {n_voters}')
        plt.title('Bias vs Total Absolute Difference by Voter Count')
        plt.xlabel('Bias')
        plt.ylabel('Total Absolute Difference')
    
        # Plot bias vs total_absdiff for different candidate counts
        plt.subplot(2, 2, 2)
        for n_candidates in np.unique(data['n_candidates']):
            subset = data[data['n_candidates'] == n_candidates]
            sns.lineplot(x='bias', y='total_absdiff', data=subset, label=f'Candidates: {n_candidates}')
        plt.title('Bias vs Total Absolute Difference by Candidate Count')
        plt.xlabel('Bias')
        plt.ylabel('Total Absolute Difference')
    
        # Heatmap of bias and n_voters on total_absdiff (Sample)
        plt.subplot(2, 2, 3)
        pivot_sample = data[data['cand_sampler'] == 'Sample'].groupby(['bias', 'n_voters'])['total_absdiff'].mean().reset_index()
        sns.heatmap(pivot_sample, annot=True, fmt=".3f", cmap="YlGnBu")
        plt.title('Mean Total Absolute Difference (Sample)')
        plt.xlabel('Number of Voters')
        plt.ylabel('Bias')
    
        # Heatmap of bias and n_voters on total_absdiff (Voter)
        plt.subplot(2, 2, 4)
        pivot_voter = data[data['cand_sampler'] == 'Voter'].groupby(['bias', 'n_voters'])['total_absdiff'].mean().reset_index()
        sns.heatmap(pivot_voter, annot=True, fmt=".3f", cmap="YlGnBu")
        plt.title('Mean Total Absolute Difference (Voter)')
        plt.xlabel('Number of Voters')
        plt.ylabel('Bias')
    
        plt.tight_layout()
        plt.savefig('detailed_parameter_analysis.png')
    
        # Generate predictive analysis for different parameter combinations
        def predict_absdiff(bias, n_voters, n_candidates, sampler_numeric):
            # Scale input using the same scaler
            X_new = np.array([[bias, n_voters, n_candidates, sampler_numeric]])
            X_new_scaled = scaler.transform(X_new)
        
            # Make prediction
            pred = (trace.posterior['intercept'].mean().item() + 
                    trace.posterior['beta_bias'].mean().item() * X_new_scaled[0, 0] + 
                    trace.posterior['beta_n_voters'].mean().item() * X_new_scaled[0, 1] + 
                    trace.posterior['beta_n_candidates'].mean().item() * X_new_scaled[0, 2] + 
                    trace.posterior['beta_sampler'].mean().item() * X_new_scaled[0, 3] +
                    trace.posterior['beta_bias_voters'].mean().item() * X_new_scaled[0, 0] * X_new_scaled[0, 1] +
                    trace.posterior['beta_bias_candidates'].mean().item() * X_new_scaled[0, 0] * X_new_scaled[0, 2] +
                    trace.posterior['beta_bias_sampler'].mean().item() * X_new_scaled[0, 0] * X_new_scaled[0, 3])
        
            return pred
    
        print("\nPredictions for some parameter combinations:")
        combinations = [
            (0.8, 9, 3, 0),  # Low bias, low voters/candidates, Sample
            (1.4, 15, 7, 1),  # High bias, high voters/candidates, Voter
            (1.0, 12, 5, 0),  # Medium bias, medium voters/candidates, Sample
            (1.0, 12, 5, 1),  # Medium bias, medium voters/candidates, Voter
        ]
    
        for bias, n_voters, n_candidates, sampler_numeric in combinations:
            sampler_name = "Sample" if sampler_numeric == 0 else "Voter"
            pred = predict_absdiff(bias, n_voters, n_candidates, sampler_numeric)
            print(f"Bias: {bias}, Voters: {n_voters}, Candidates: {n_candidates}, Sampler: {sampler_name} → Predicted total_absdiff: {pred:.6f}")
    
        # Find optimal parameter combination using the model
        best_bias = None
        best_n_voters = None
        best_n_candidates = None
        best_sampler = None
        lowest_pred = float('inf')
    
        # Grid search through parameter space
        for bias in biases:
            for n_voters in n_voters_values:
                for n_candidates in n_candidates_values:
                    for sampler_numeric in [0, 1]:
                        pred = predict_absdiff(bias, n_voters, n_candidates, sampler_numeric)
                        if pred < lowest_pred:
                            lowest_pred = pred
                            best_bias = bias
                            best_n_voters = n_voters
                            best_n_candidates = n_candidates
                            best_sampler = "Sample" if sampler_numeric == 0 else "Voter"
    
        print("\nOptimal parameter combination based on model predictions:")
        print(f"Bias: {best_bias}, Voters: {best_n_voters}, Candidates: {best_n_candidates}, Sampler: {best_sampler}")
        print(f"Predicted minimum total_absdiff: {lowest_pred:.6f}")

    pymc_model(data_delib)
    return (pymc_model,)


@app.cell
def _(data_control, pymc_model):
    pymc_model(data_control)
    return


if __name__ == "__main__":
    app.run()
