import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Generate a color cycle from the 'viridis' colormap

plt.rcParams.update(
    {
        "font.size": 23,
        "axes.labelsize": 23,
        "axes.titlesize": 23,
        "xtick.labelsize": 23,
        "ytick.labelsize": 23,
        "legend.fontsize": 16,
        "axes.linewidth": 1,
        "grid.linewidth": 1,
        "grid.alpha": 0.3,
        "image.cmap": "viridis",
        "text.usetex": True,
        "font.family": "Computer Modern",
    }
)


def read_data(filename):
    return pd.read_csv(filename)


def compute_proportion(data, col_start, col_end, new_col):
    # Ensure 'cyclic_start' and 'cyclic_end' are numeric
    data[col_start] = data[col_start].astype(float)
    data[col_end] = data[col_end].astype(float)

    # Group by bias
    aggregated_start = data.groupby(["bias", "metric_space"])[col_start].mean()
    aggregated_end = data.groupby(["bias", "metric_space"])[col_end].mean()

    # Compute proportion (avoid division by zero)
    agg_prop = (aggregated_end / aggregated_start).replace(np.nan, 0)

    # Convert Series to DataFrame and reset index
    return agg_prop.reset_index(name=new_col)


def compute_average(data, col_start, new_col):
    # Ensure 'cyclic_start' and 'cyclic_end' are numeric
    data[col_start] = data[col_start].astype(float)

    # Group by bias
    aggregated_start = data.groupby(["bias", "metric_space"])[col_start].mean()

    # Convert Series to DataFrame and reset index
    return aggregated_start.reset_index(name=new_col)


def compute_percentage_change(data, col_start, col_end, new_col):
    # Ensure 'cyclic_start' and 'cyclic_end' are numeric
    data[col_start] = data[col_start].astype(float)
    data[col_end] = data[col_end].astype(float)

    # Group by bias
    aggregated_start = data.groupby(["bias", "metric_space"])[col_start].mean()
    aggregated_end = data.groupby(["bias", "metric_space"])[col_end].mean()

    # Compute proportion (avoid division by zero)
    agg_prop = ((aggregated_end - aggregated_start) / aggregated_start).replace(
        np.nan, 0
    )

    # Convert Series to DataFrame and reset index
    return agg_prop.reset_index(name=new_col)


def plot(data, col, ylab):
    spaces = ["KS", "DP", "CS"]
    marker = ["o", "s", "^"]
    colors = ["#A93C93", "#008B72", "#613F99"]
    for marker, space, color in zip(marker, spaces, colors):
        subset = data[data["metric_space"] == space]
        x = subset["bias"]
        y = subset[col]

        # Plot
        plt.plot(
            x,
            y,
            label=space,
            marker=marker,
            color=color,
            linestyle="-",
        )
    plt.xticks(rotation=45)
    plt.xlabel("Bias")
    plt.ylabel(ylab)
    plt.legend()
    plt.grid(axis="y")
    plt.tight_layout()
    plt.savefig(f"figures/{col}_{ylab}.pdf")
    plt.show()


if __name__ == "__main__":
    data = pd.read_csv("results/data_weak.csv")
    cyclic_proportion = compute_proportion(
        data, "cyclic_start", "cyclic_end", "cyclic_proportion"
    )
    consensus_change = compute_percentage_change(
        data, "consensus_dist_start", "consensus_dist_end", "consensus_dist_change"
    )
    transtive_proportion = compute_proportion(
        data, "intransative_start", "intransative_end", "intransative_proportion"
    )
    condorcet_proportion = compute_proportion(
        data, "condorcet_start", "condorcet_end", "condorcet_proportion"
    )
    unique_profiles = compute_average(data, "unique_end", "unique")
    sp_proximity = compute_average(data, "proximity_to_sp_end", "sp_proximity")
    sp_proximity = sp_proximity.loc[sp_proximity["bias"] > 0.5]
    plot(
        cyclic_proportion,
        "cyclic_proportion",
        "Proportion",
    )
    plot(
        transtive_proportion,
        "intransative_proportion",
        "Proportion",
    )
    plot(
        consensus_change,
        "consensus_dist_change",
        "Percentage",
    )
    plot(
        condorcet_proportion,
        "condorcet_proportion",
        "Proportion",
    )
    plot(
        unique_profiles,
        "unique",
        "Count",
    )
    plot(
        sp_proximity,
        "sp_proximity",
        "proximity to Single-peakedness",
    )
