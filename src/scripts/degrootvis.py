import marimo

__generated_with = "0.12.0"
app = marimo.App(width="medium")


@app.cell
def _():
    import marimo as mo
    import pandas as pd
    import numpy as np
    import matplotlib.pyplot as plt
    import matplotlib.lines as mlines
    return mlines, mo, np, pd, plt


@app.cell
def _(np, pd, plt):
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


    def compute_proportion(data, col_start, col_end, group):
        # Ensure 'cyclic_start' and 'cyclic_end' are numeric
        data[col_start] = data[col_start].astype(float)
        data[col_end] = data[col_end].astype(float)

        # Group by bias
        aggregated_start = data.groupby(group)[col_start].mean()
        aggregated_end = data.groupby(group)[col_end].mean()

        # Compute proportion (avoid division by zero)
        agg_prop = (aggregated_end / aggregated_start).replace(np.nan, 0)

        # Convert Series to DataFrame and reset index
        return agg_prop.reset_index()


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
def _(mlines, plt):
    def plot(df, x, y, ylabel):
        fig, ax = plt.subplots(figsize=(8, 5))

        # Define markers for each sampler (cycling if needed)
        marker_styles = ["o", "s", "^", "D", "v", "P", "X", "*"]
        sampler_markers = {
            sampler: marker_styles[i % len(marker_styles)]
            for i, sampler in enumerate(df["cand_sampler"].unique())
        }

        # Define colors for 'Type' — consistent ordering
        type_colors = {"Start": "#A93C93", "End": "#008B72", "True": "#613F99"}

        for sampler in df["cand_sampler"].unique():
            for typ in df["Type"].unique():
                subset = df[(df["cand_sampler"] == sampler) & (df["Type"] == typ)]
                ax.plot(
                    subset[x],
                    subset[y],
                    label=f"{typ} / {sampler}",
                    color=type_colors.get(typ, "black"),
                    marker=sampler_markers[sampler],
                    linestyle="-",
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
                linestyle="None",
                label=sampler,
            )
            for sampler, marker in sampler_markers.items()
        ]

        # Combine and show
        legend1 = ax.legend(
            handles=type_handles, title="Type (color)", loc="upper left"
        )
        legend2 = ax.legend(
            handles=sampler_handles, title="Sampler (marker)", loc="lower left"
        )

        ax.add_artist(legend1)  # Keep both legends visible
        plt.tight_layout()
        plt.show()
    return (plot,)


@app.cell
def _(compute_average, compute_proportion, pd, plot, read_data):
    data = read_data("results/data_degroot_mapping_test.csv")
    data = data[data["time_steps"] == 100]
    data = data[data["n_candidates"] == 5]


    def compute_and_merge_proportions(
        data, start_col, end_col, true_col, name, group_by
    ):
        df_start = compute_proportion(data, start_col, start_col, group_by)
        df_end = compute_proportion(data, start_col, end_col, group_by)
        df_true = compute_proportion(data, start_col, true_col, group_by)

        df_start["Type"] = "Start"
        df_end["Type"] = "End"
        df_true["Type"] = "True"

        df_combined = pd.concat([df_start, df_end, df_true])
        df_combined = df_combined.rename(columns={start_col: name})

        return df_combined


    cyclic = compute_and_merge_proportions(
        data,
        "cyclic_start",
        "cyclic_end",
        "cyclic_true",
        "cyclic_proportion",
        ["bias", "cand_sampler"],
    )

    intransitive = compute_and_merge_proportions(
        data,
        "intransative_start",
        "intransative_end",
        "intransative_true",
        "intransative_proportion",
        ["bias", "cand_sampler"],
    )

    condorcet = compute_and_merge_proportions(
        data,
        "condorcet_start",
        "condorcet_end",
        "condorcet_true",
        "condorcet_proportion",
        ["bias", "cand_sampler"],
    )

    unique_profiles_end = compute_average(
        data, "unique_end", "unique", ["bias", "cand_sampler"]
    )
    unique_profiles_true = compute_average(
        data, "unique_true", "unique", ["bias", "cand_sampler"]
    )
    unique_profiles_end["Type"] = "End"
    unique_profiles_true["Type"] = "True"
    df_combined = pd.concat([unique_profiles_end, unique_profiles_true])
    unique_profiles = df_combined.rename(
        columns={"unique_start": "unique_profiles"}
    )

    # === Plotting all variants in one figure ===
    plot(cyclic, "bias", "cyclic_proportion", "Proportion Cyclic")
    plot(intransitive, "bias", "intransative_proportion", "Proportion Transitive")
    plot(condorcet, "bias", "condorcet_proportion", "Proportion Condorcet")
    plot(unique_profiles, "bias", "unique", r"\#Unique Preferences")
    return (
        compute_and_merge_proportions,
        condorcet,
        cyclic,
        data,
        df_combined,
        intransitive,
        unique_profiles,
        unique_profiles_end,
        unique_profiles_true,
    )


if __name__ == "__main__":
    app.run()
