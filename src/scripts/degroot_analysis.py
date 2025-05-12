import marimo

__generated_with = "0.11.31"
app = marimo.App(width="full")


@app.cell
def _():
    import marimo as mo
    import pandas as pd
    import numpy as np
    from scipy import stats
    import matplotlib.pyplot as plt
    import matplotlib.lines as mlines
    import seaborn as sns
    import altair as alt
    import arviz as az
    import pymc as pm
    from sklearn.preprocessing import StandardScaler
    from pymc import Model, Normal, sample
    return (
        Model,
        Normal,
        StandardScaler,
        alt,
        az,
        mlines,
        mo,
        np,
        pd,
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
            "axes.grid"  : True,
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
def _(compute_proportion, mlines, np, pd, plt):
    def plot(df, x, y, ylabel, file_prefix):
        fig, ax = plt.subplots(figsize=(12,8))

        # Define markers for each sampler (cycling if needed)
        marker_styles = ["o", "X", "^", "D", "v", "P", "X", "*"]
        sampler_markers = {
            sampler: marker_styles[i % len(marker_styles)]
            for i, sampler in enumerate(df["cand_sampler"].unique())
        }

        # Define colors for 'Type' — consistent ordering
        type_colors = {"Start": "#A93C93", "End": "#008B72", "Final": "#613F99", "True": "#613F99"}
        types_used = {}

        for sampler in df["cand_sampler"].unique():
            for typ in df["Type"].unique():
                subset = df[(df["cand_sampler"] == sampler) & (df["Type"] == typ)]
                bins = np.linspace(subset[x].min(), subset[x].max(), 10)
                digitized = np.digitize(subset[x], bins)
                bin_means = [subset[y][digitized == i].mean() for i in range(0, len(bins))]
                if not subset.empty:
                    types_used[typ] = type_colors[typ]
                    ax.plot(
                        bins,
                        bin_means,
                        label=f"{typ} / {sampler}",
                        color=type_colors.get(typ, "black"),
                        marker=sampler_markers[sampler],
                        markersize=10,
                        linestyle="--",
                        linewidth=1,
                        alpha=0.4,
                    )

        ax.set_xlabel(x.capitalize())
        ax.set_ylabel(ylabel.capitalize())
        # ax.set_title(ylabel.capitalize())
        ax.grid(True)
        type_handles = [
            mlines.Line2D(
                [], [], color=color, marker="o", linestyle="-", label=typ
            )
            for typ, color in types_used.items()
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

        legend1 = ax.legend(
            handles=type_handles + sampler_handles,
            title="Type (color),\nSampler (marker)",
            loc="upper left",
            bbox_to_anchor=(1.05, 1.0),  # Outside the axes, to the right
            borderaxespad=0.
        )

        # ax.add_artist(legend1) 
        plt.tight_layout()
        plt.savefig(f"figures/{file_prefix}_{ylabel}")
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
    data_delib = pd.read_csv("results/degroot_deliberation_100_random.csv")
    data_control= pd.read_csv("results/degroot_deliberation_100_control_random.csv", index_col=False)

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
    return c, data_control, data_delib, prox_cols, prox_voter_cols


@app.cell
def _(data_control):
    data_control.describe()
    return


@app.cell
def _(data_control, data_delib):
    voter_str = "n_voters"
    cand_str = "n_candidates"
    bias_str = "bias"
    time_str = "time_steps"
    sampler_str = "cand_sampler"
    voter_df = {x: data_delib.loc[data_delib[voter_str] == x ] for x in data_delib[voter_str].unique()}
    cand_df = {x: data_delib.loc[data_delib[cand_str] == x ] for x in data_delib[cand_str].unique()}
    time_df = {x: data_delib.loc[data_delib[time_str] == x ] for x in data_delib[time_str].unique()}
    measurements_bool = ['cyclic_start', 'condorcet_start', 'unique_start',  'cyclic_end', 'condorcet_end', 'unique_end',  'cyclic_true', 'condorcet_true', 'unique_true' ]
    for mb in measurements_bool:
        data_delib[mb] = data_delib[mb].astype(int)
        data_control[mb] = data_control[mb].astype(int)

    return (
        bias_str,
        cand_df,
        cand_str,
        mb,
        measurements_bool,
        sampler_str,
        time_df,
        time_str,
        voter_df,
        voter_str,
    )


@app.cell
def _(data_control, data_delib, time_str):
    data_delib_151 = data_delib.loc[data_delib[time_str] == 151].copy()
    data_control_151 = data_control.loc[data_control[time_str] == 151].copy()
    return data_control_151, data_delib_151


@app.cell
def _(
    compute_and_merge_proportions,
    compute_average,
    data_control,
    data_delib,
    pd,
    plot,
    time_str,
):
    def generate_general_graphs(data, file_prefix):

        cyclic = compute_and_merge_proportions(
            data_delib,
            "cyclic_start",
            "cyclic_end",
            "cyclic_true",
            "cyclic_proportion",
            ["bias", "cand_sampler"],
        )


        condorcet = compute_and_merge_proportions(
            data_delib,
            "condorcet_start",
            "condorcet_end",
            "condorcet_true",
            "condorcet_proportion",
            ["bias", "cand_sampler"],
        )

        proximity_to_sp_end = compute_average(
            data_delib, "proximity_to_cand_sp_end", "proximity_to_cand_sp", ["bias", "cand_sampler"]
        )
        proximity_to_sp_true = compute_average(
            data_delib, "proximity_to_cand_sp_true", "proximity_to_cand_sp", ["bias", "cand_sampler"]
        )
        proximity_to_sp_end["Type"] = "End"
        proximity_to_sp_true["Type"] = "True"
        df_combined = pd.concat([proximity_to_sp_end, proximity_to_sp_true])
        proximity_to_sp = df_combined.rename(
            columns={"proximity_to_start": "proximity_to_cand_sp"}
        )

        proximity_to_voter_sp_end = compute_average(
            data_delib, "proximity_to_voter_sp_end", "proximity_to_voter_sp", ["bias", "cand_sampler"]
        )
        proximity_to_voter_sp_true = compute_average(
            data_delib, "proximity_to_voter_sp_true", "proximity_to_voter_sp", ["bias", "cand_sampler"]
        )
        proximity_to_voter_sp_end["Type"] = "End"
        proximity_to_voter_sp_true["Type"] = "True"
        df_combined = pd.concat([proximity_to_voter_sp_end, proximity_to_voter_sp_true])
        proximity_to_voter_sp = df_combined.rename(
            columns={"proximity_to_start": "proximity_to_voter_sp"}
        )

        unique_profiles_end = compute_average(
            data_delib, "unique_end", "unique", ["bias", "cand_sampler"]
        )
        unique_profiles_true = compute_average(
            data_delib, "unique_true", "unique", ["bias", "cand_sampler"]
        )
        unique_profiles_end["Type"] = "End"
        unique_profiles_true["Type"] = "True"
        df_combined = pd.concat([unique_profiles_end, unique_profiles_true])
        unique_profiles = df_combined.rename(
            columns={"unique_start": "unique_profiles"}
        )

        # === Plotting all variants in one figure ===
        plot(cyclic, "bias", "cyclic_proportion", "Mean Number of Cyclic Profiles", file_prefix)
        plot(proximity_to_sp, "bias", "proximity_to_cand_sp", "Mean candidate proximity to single peaked Profiles", file_prefix)
        plot(proximity_to_voter_sp, "bias", "proximity_to_voter_sp", "Mean voter proximity to single peaked Profiles", file_prefix)
        plot(condorcet, "bias", "condorcet_proportion", "Mean number of Condorcet winners", file_prefix)
        plot(unique_profiles, "bias", "unique", r"\#Unique Preferences", file_prefix)


    generate_general_graphs(data_delib.loc[data_delib[time_str] == 151], "delib")
    generate_general_graphs(data_control.loc[data_control[time_str] == 151], "control")
    return (generate_general_graphs,)


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


@app.cell(hide_code=True)
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


@app.cell(hide_code=True)
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


@app.cell(hide_code=True)
def _(mo):
    mo.md(
        r"""
        # Statistical Analysis
        We now proceed to analyze fit of this model compared to the `final` data. Here `final` is the data of the voters in the second time measurement.
        """
    )
    return


@app.cell
def _(data_control, data_delib, time_str):
    def normalize(df):
        return (df - df.min() ) / df.max()

    # Compute differences for each metric
    def diff_data_on_metrics(df):
        metrics = ['proximity_to_cand_sp', 'proximity_to_voter_sp']
        for m in metrics:
            df[f'{m}_diff'] = normalize(df[f'{m}_end']) - normalize(df[f'{m}_true'])
            df[f'{m}_absdiff'] = df[f'{m}_diff']**2

        # Example: combine into a total error score (sum of absolute differences)
        df['total_absdiff'] = df[[f'{m}_absdiff' for m in metrics]].sum(axis=1)
        return df.groupby(['bias','cand_sampler','n_voters','n_candidates','time_steps']) \
                ['total_absdiff'].agg(['mean','std']).reset_index()

    summary_delib = diff_data_on_metrics(data_delib.loc[data_delib[time_str] < 52].copy())
    summary_control = diff_data_on_metrics(data_control.loc[data_control[time_str] < 52].copy())
    return diff_data_on_metrics, normalize, summary_control, summary_delib


@app.cell
def _(bias_str, plt, summary_control, summary_delib):
    def plot_summary(summary,title):
        lab = [int(x) for x in summary["time_steps"].unique().tolist()]
        scatter = plt.scatter(summary[bias_str], summary["mean"], c=summary["time_steps"])
        plt.xlabel("Bias")
        plt.ylabel("Total Absolute difference")
        legend = plt.legend(
            handles=scatter.legend_elements()[0],
            labels=lab,
            title="Time step",
            loc="upper right",
            bbox_to_anchor=(1.24, 1),  # center it under the plot
            ncol= 1,
        )
        plt.grid()
        plt.tight_layout()
        plt.savefig(f"figures/{title}")
        plt.show()
    plot_summary(summary_delib, "error_scatter_delib.pdf")
    plot_summary(summary_control, "error_scatter_control.pdf")
    return (plot_summary,)


@app.cell
def _(data_delib, np, pd, time_str):
    def bin_biases(data):
        bias_bins = np.linspace(data["bias"].min(), data["bias"].max(), 15)
        bins_index = np.digitize(data["bias"], bias_bins)
        data["binned_bias"] = bias_bins[bins_index-1]
        return data

    def find_min_bias(df, metric_col):
        # Group by bias and calculate mean of the metric
        means = df.groupby(['binned_bias', 'knowledge', time_str])[metric_col].mean()
        # Find the bias value with the minimum mean metric value
        return means.idxmin() if not means.empty else np.nan


    def pivot_of_minimum_values(data, metrics):
        # Create an empty DataFrame to store results
        result_df = pd.DataFrame()

        # For each combination of cand_sampler and knowledge
        for (sampler), group in data.groupby(['cand_sampler']):
            row_data = {'cand_sampler': sampler}

            # For each metric, find the bias that minimizes it
            for metric in metrics:
                min_bias = find_min_bias(group, metric)
                row_data[f'{metric.replace("_", " ")}'] = min_bias

            # Append to results
            result_df = pd.concat([result_df, pd.DataFrame([row_data])], ignore_index=True)

        # Set the index for the final table
        result_df = result_df.set_index(['cand_sampler'])

        # You can reshape it if you want a different format
        # This puts metrics as columns and shows the optimal bias value
        return result_df

    m = ['proximity_to_voter_sp_absdiff', 'proximity_to_cand_sp_absdiff', 'total_absdiff']

    def to_tex(df: pd.DataFrame):
        print(df.to_latex( multicolumn=True))
    
    to_tex(pivot_of_minimum_values(bin_biases(data_delib), m))
    return bin_biases, find_min_bias, m, pivot_of_minimum_values, to_tex


@app.cell
def _(bin_biases, data_control, m, pivot_of_minimum_values, to_tex):
    to_tex(pivot_of_minimum_values(bin_biases(data_control), m))
    return


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
def _(pd):
    convergence_data_cred = pd.read_csv("results/degroot_deliberation_trials_100_convergence_dense_knowledge.csv")
    convergence_data_know = pd.read_csv("results/degroot_deliberation_trials_100_convergence_sparse_knowledge.csv")
    convergence_data_cred_group = pd.read_csv("results/degroot_deliberation_100_convergence_credibility_grouped.csv")
    convergence_data_know_group = pd.read_csv("results/degroot_deliberation_100_convergence_knowledge_grouped.csv")
    return (
        convergence_data_cred,
        convergence_data_cred_group,
        convergence_data_know,
        convergence_data_know_group,
    )


@app.cell
def _(
    cand_str,
    convergence_data_cred,
    convergence_data_cred_group,
    convergence_data_know,
    convergence_data_know_group,
    np,
    plt,
    sampler_str,
    time_str,
):
    # Begin plotting
    fig, ax = plt.subplots(3, 4, figsize=(20, 15), sharex=True)
    ax = ax.ravel()
    for i, convergence_data in enumerate([convergence_data_cred,  convergence_data_know,convergence_data_cred_group,  convergence_data_know_group]):

        grouped_by_cand_and_sampler = convergence_data.groupby([cand_str, sampler_str, time_str]).agg("mean")
        df_reset = grouped_by_cand_and_sampler.reset_index()
        df_reset = df_reset.loc[df_reset[time_str] < 27]
        candidate_counts = sorted(df_reset['n_candidates'].unique())
        # Unique values for styling
        sampler_styles = {'Sample': 'X', 'Voter': 's'}
        colors = plt.cm.viridis_r(np.linspace(0, 1, len(candidate_counts)))

        # Plot each combination
        for idx, n in enumerate(candidate_counts):
            for sampler, marker in sampler_styles.items():
                subset = df_reset[(df_reset['n_candidates'] == n) & (df_reset['cand_sampler'] == sampler)]
                if not subset.empty:
                    ax[0+i].plot(subset['time_steps'], subset['ks_distance_true'], 
                               label=f'{n} candidates, {sampler}', 
                               marker=marker, color=colors[idx], linestyle='-')
                    ax[4+i].plot(subset['time_steps'], subset['cs_distance_true'], 
                               label=f'{n} candidates, {sampler}', 
                               marker=marker, color=colors[idx], linestyle='-')
                    ax[8+i].plot(subset['time_steps'], subset['entrywise_distance'], 
                               label=f'{n} candidates, {sampler}', 
                               marker=marker, color=colors[idx], linestyle='-')
    ax[0].set_title('Knowledge (Control, Dense)')
    ax[1].set_title('Knowledge (Control, Sparse)')
    ax[2].set_title('Uniform Original Groups')
    ax[3].set_title('Knowledge Original Groups')

    # Labels and titles
    ax[0].set_ylabel('KS Distance')
    ax[4].set_ylabel('CS Distance')
    ax[8].set_ylabel('Entrywise distance')
    ax[8].set_xlabel('Time Steps')
    ax[9].set_xlabel('Time Steps')
    ax[10].set_xlabel('Time Steps')
    ax[11].set_xlabel('Time Steps')

    # Legend
    handles, labels = ax[1].get_legend_handles_labels()
    fig.legend(handles, labels, loc='lower center', ncol=3,
               bbox_to_anchor=(0.51, -0.15))

    plt.tight_layout()
    fig.savefig("figures/convergence_groups.pdf", bbox_inches='tight')
    plt.show()
    return (
        ax,
        candidate_counts,
        colors,
        convergence_data,
        df_reset,
        fig,
        grouped_by_cand_and_sampler,
        handles,
        i,
        idx,
        labels,
        marker,
        n,
        sampler,
        sampler_styles,
        subset,
    )


@app.cell(hide_code=True)
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


if __name__ == "__main__":
    app.run()
