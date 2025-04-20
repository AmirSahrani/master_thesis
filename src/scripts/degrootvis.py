import marimo

__generated_with = "0.12.0"
app = marimo.App(width="full")


@app.cell
def _():
    import marimo as mo
    import pandas as pd
    import numpy as np
    import matplotlib.pyplot as plt
    import matplotlib.lines as mlines
    import altair as alt
    return alt, mlines, mo, np, pd, plt


@app.cell
def _(np, pd, plt):
    plt.style.use("default")
    plt.rcParams.update(
        {
            "font.size": 20,
            "figure.figsize": [15, 10],
            "axes.linewidth": 1,
            "grid.linewidth": 1,
            "grid.alpha": 0.3,
            "image.cmap": "viridis",
            "text.usetex": True,
            "font.family": "Charter",
        }
    )


    def read_data(filename):
        return pd.read_csv(filename)


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
    return (
        compute_average,
        compute_percentage_change,
        compute_proportion,
        read_data,
    )


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
def _(read_data):
    data = read_data("results/data_degroot_mapping.csv")




    # cyclic = compute_and_merge_proportions(
    #     data,
    #     "cyclic_start",
    #     "cyclic_end",
    #     "cyclic_true",
    #     "cyclic_proportion",
    #     ["bias", "cand_sampler"],
    # )

    # intransitive = compute_and_merge_proportions(
    #     data,
    #     "intransative_start",
    #     "intransative_end",
    #     "intransative_true",
    #     "intransative_proportion",
    #     ["bias", "cand_sampler"],
    # )

    # condorcet = compute_and_merge_proportions(
    #     data,
    #     "condorcet_start",
    #     "condorcet_end",
    #     "condorcet_true",
    #     "condorcet_proportion",
    #     ["bias", "cand_sampler"],
    # )

    # unique_profiles_end = compute_average(
    #     data, "unique_end", "unique", ["bias", "cand_sampler"]
    # )
    # unique_profiles_true = compute_average(
    #     data, "unique_true", "unique", ["bias", "cand_sampler"]
    # )
    # unique_profiles_end["Type"] = "End"
    # unique_profiles_true["Type"] = "True"
    # df_combined = pd.concat([unique_profiles_end, unique_profiles_true])
    # unique_profiles = df_combined.rename(
    #     columns={"unique_start": "unique_profiles"}
    # )

    # # === Plotting all variants in one figure ===
    # plot(cyclic, "bias", "cyclic_proportion", "Mean Number of Cyclic Profiles")
    # plot(intransitive, "bias", "intransative_proportion", "Mean Number of Transative Profiles")
    # plot(condorcet, "bias", "condorcet_proportion", "Mean number of Condorcet winners")
    # plot(unique_profiles, "bias", "unique", r"\#Unique Preferences")
    return (data,)


@app.cell
def _(data):
    voter_str = "n_voters"
    cand_str = "n_candidates"
    bias_str = "bias"
    time_str = "time_steps"
    sampler_str = "cand_sampler"
    voter_df = {x: data.loc[data[voter_str] == x ] for x in data[voter_str].unique()}
    cand_df = {x: data.loc[data[cand_str] == x ] for x in data[cand_str].unique()}
    time_df = {x: data.loc[data[time_str] == x ] for x in data[time_str].unique()}
    print(voter_df.keys())
    print(cand_df.keys())
    print(time_df.keys())
    return (
        bias_str,
        cand_df,
        cand_str,
        sampler_str,
        time_df,
        time_str,
        voter_df,
        voter_str,
    )


@app.cell
def _(
    cand_str,
    compute_and_merge_proportions,
    compute_average,
    data,
    pd,
    plot,
    time_str,
    unique_profiles_end,
    unique_profiles_true,
    voter_str,
):
    data_51_5 = data.loc[(data[voter_str] == 51) & (data[cand_str] == 5) & (data[time_str] == 50)].copy()

    cyclic_51_5 = compute_and_merge_proportions(
        data_51_5,
        "cyclic_start",
        "cyclic_end",
        "cyclic_true",
        "cyclic_proportion",
        ["bias", "cand_sampler"],
    )

    intransitive_51_5 = compute_and_merge_proportions(
        data_51_5,
        "intransative_start",
        "intransative_end",
        "intransative_true",
        "intransative_proportion",
        ["bias", "cand_sampler"],
    )

    condorcet_51_5 = compute_and_merge_proportions(
        data_51_5,
        "condorcet_start",
        "condorcet_end",
        "condorcet_true",
        "condorcet_proportion",
        ["bias", "cand_sampler"],
    )

    unique_profiles_end_51_5 = compute_average(
        data_51_5, "unique_end", "unique", ["bias", "cand_sampler"]
    )
    unique_profiles_true_51_5 = compute_average(
        data_51_5, "unique_true", "unique", ["bias", "cand_sampler"]
    )
    unique_profiles_end["Type"] = "End"
    unique_profiles_true["Type"] = "True"
    df_combined_51_5 = pd.concat([unique_profiles_end, unique_profiles_true])
    unique_profiles_51_5 = df_combined_51_5.rename(
        columns={"unique_start": "unique_profiles"}
    )

    # === Plotting all variants in one figure ===
    plot(cyclic_51_5, "bias", "cyclic_proportion", "Mean Number of Cyclic Profiles")
    plot(intransitive_51_5, "bias", "intransative_proportion", "Mean Number of Transative Profiles")
    plot(condorcet_51_5, "bias", "condorcet_proportion", "Mean number of Condorcet winners")
    plot(unique_profiles_51_5, "bias", "unique", r"\#Unique Preferences")
    return (
        condorcet_51_5,
        cyclic_51_5,
        data_51_5,
        df_combined_51_5,
        intransitive_51_5,
        unique_profiles_51_5,
        unique_profiles_end_51_5,
        unique_profiles_true_51_5,
    )


@app.cell
def _(cand_str, data, mo, time_str, voter_str):
    # Create UI controls
    voter_dropdown = mo.ui.dropdown(
        options={str(v): v for v in sorted(data[voter_str].unique())},
        value="51",
        label="Number of Voters"
    )

    cand_dropdown = mo.ui.dropdown(
        options={str(c): c for c in sorted(data[cand_str].unique())},
        value="7",
        label="Number of Candidates"
    )

    time_dropdown = mo.ui.dropdown(
        options={str(t): t for t in sorted(data[time_str].unique())},
        value="1.0",
        label="Time Value"
    )

    # Display UI controls
    controls = mo.hstack([voter_dropdown, cand_dropdown, time_dropdown])
    controls
    return cand_dropdown, controls, time_dropdown, voter_dropdown


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
        chart = alt.Chart(df).mark_line(
            point=True,
            strokeDash=[4, 4],  # Dashed line similar to linestyle="--"
            opacity=0.7
        ).encode(
            x=alt.X(f'{x_col}:Q', title=x_col),
            y=alt.Y(f'{y_col}:Q', title=title),
            color=alt.Color('Type:N', scale=alt.Scale(domain=list(type_colors.keys()), 
                                                      range=list(type_colors.values())),
                           title='Type'),
            shape=alt.Shape('cand_sampler:N', 
                           scale=alt.Scale(domain=list(sampler_markers.keys()),
                                           range=list(sampler_markers.values())),
                           title='Candidate Sampler'),
            tooltip=['bias', 'cand_sampler', 'Type', y_col]
        ).properties(
            width=800,
            height=500,
            title=title
        ).interactive()
    
        return chart

    # This function can be used to create a combined chart with legend
    def create_combined_chart(df, x_col, y_col, title):
        base = create_altair_chart(df, x_col, y_col, title)
    
        # Add a proper legend similar to the matplotlib version
        # First, create a legend for Type (color)
        type_legend = alt.Chart(df).mark_point().encode(
            y=alt.Y('Type:N', axis=alt.Axis(orient='right')),
            color=alt.Color('Type:N', legend=None)
        ).properties(width=100, height=100)
    
        # Then, create a legend for cand_sampler (shape)
        sampler_legend = alt.Chart(df).mark_point().encode(
            y=alt.Y('cand_sampler:N', axis=alt.Axis(orient='right')),
            shape=alt.Shape('cand_sampler:N', legend=None)
        ).properties(width=100, height=100)
    
        return base
    return create_altair_chart, create_combined_chart


@app.cell(hide_code=True)
def _(
    cand_dropdown,
    cand_str,
    compute_and_merge_proportions,
    compute_average,
    create_altair_chart,
    data,
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

    # Filter data based on selections
    filtered_data = data.loc[
        (data[voter_str] == voter_value) & 
        (data[cand_str] == cand_value) & 
        (data[time_str] == time_value)
    ].copy()

    # Get the processed datasets using your existing helper functions
    # Assuming these functions already handle the "Type" column correctly
    cyclic = compute_and_merge_proportions(
        filtered_data,
        "cyclic_start", "cyclic_end", "cyclic_true", "cyclic_proportion",
        ["bias", "cand_sampler"]
    )

    intransitive = compute_and_merge_proportions(
        filtered_data,
        "intransative_start", "intransative_end", "intransative_true", "intransative_proportion",
        ["bias", "cand_sampler"]
    )

    condorcet = compute_and_merge_proportions(
        filtered_data,
        "condorcet_start", "condorcet_end", "condorcet_true", "condorcet_proportion",
        ["bias", "cand_sampler"]
    )

    unique_profiles_end = compute_average(
        filtered_data, "unique_end", "unique", ["bias", "cand_sampler"]
    )
    unique_profiles_end["Type"] = "End"

    unique_profiles_true = compute_average(
        filtered_data, "unique_true", "unique", ["bias", "cand_sampler"]
    )
    unique_profiles_true["Type"] = "True"

    unique_profiles = pd.concat([unique_profiles_end, unique_profiles_true])

    # Create the charts
    # You'll need to specify the correct column names based on your actual data structure
    chart1 = create_altair_chart(cyclic, "bias", "cyclic_proportion", "Mean Number of Cyclic Profiles")
    chart2 = create_altair_chart(intransitive, "bias", "intransative_proportion", "Mean Number of Transitive Profiles")
    chart3 = create_altair_chart(condorcet, "bias", "condorcet_proportion", "Mean Number of Condorcet Winners")
    chart4 = create_altair_chart(unique_profiles, "bias", "unique", "#Unique Preferences")

    # Display title and charts
    mo.hstack([chart1, chart2])
    return (
        cand_value,
        chart1,
        chart2,
        chart3,
        chart4,
        condorcet,
        cyclic,
        filtered_data,
        intransitive,
        time_value,
        unique_profiles,
        unique_profiles_end,
        unique_profiles_true,
        voter_value,
    )


@app.cell
def _(chart3, chart4, mo):
    mo.hstack([chart3, chart4])
    return


if __name__ == "__main__":
    app.run()
