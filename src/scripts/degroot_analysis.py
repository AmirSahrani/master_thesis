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
    import sklearn
    import statsmodels.api as sm
    import pyabc
    from datetime import timedelta
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
        pyabc,
        sample,
        sklearn,
        sm,
        sns,
        stats,
        timedelta,
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


    sim_color = "#008B72"
    true_color = "#613F99"
    start_color = "#A93C93"

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
        sim_color,
        start_color,
        true_color,
    )


@app.cell
def _(compute_proportion, mlines, np, pd, plt):
    def plot(df, x, y, ylabel, file_prefix):
        fig, ax = plt.subplots(figsize=(10,7))

        # Define markers for each sampler (cycling if needed)
        marker_styles = ["o", "X", "^", "D", "v", "P", "X", "*"]
        sampler_markers = {
            sampler: marker_styles[i % len(marker_styles)]
            for i, sampler in enumerate(df["cand_sampler"].unique())
        }
        # Define colors for 'Type' — consistent ordering
        type_colors = {"Start": "#A93C93", "Simulated": "#008B72", "Final": "#613F99", "True": "#613F99"}
        types_used = {}

        for sampler in df["cand_sampler"].unique():
            for typ in df["Type"].unique():
                subset = df[(df["cand_sampler"] == sampler) & (df["Type"] == typ)]
                bins = np.linspace(subset[x].min(), subset[x].max(), len(subset[x]))
                digitized = np.digitize(subset[x], bins)
                bin_means = [subset[y][digitized == i].mean() for i in range(0, len(bins))]
                marker = sampler_markers[sampler]
                if not subset.empty:
                    types_used[typ] = type_colors[typ]
                    ax.plot(
                        bins,
                        bin_means,
                        label=f"{typ} / {sampler}",
                        color=type_colors.get(typ, "black"),
                        marker=marker,
                        markersize=10,
                        linestyle="--",
                        linewidth=1,
                        alpha=0.4,
                    )

        ax.set_xlabel("Time")
        ax.set_ylabel(ylabel.capitalize())
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

        df_end["Type"] = "Simulated"
        df_true["Type"] = "True"
        df_combined = pd.concat([df_end, df_true])

        return df_combined
    return compute_and_merge_proportions, plot


@app.cell
def _(np, pd):
    data_delib = pd.read_csv("results/degroot_deliberation_100.csv")
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
def _(data_delib):
    data_delib
    return


@app.cell
def _(compute_and_merge_proportions, compute_average, data_delib, pd, plot):
    def generate_general_graphs(data, file_prefix):

        cyclic = compute_and_merge_proportions(
            data_delib,
            "cyclic_start",
            "cyclic_end",
            "cyclic_true",
            "cyclic_proportion",
            ["time_steps", "cand_sampler"],
        )


        condorcet = compute_and_merge_proportions(
            data_delib,
            "condorcet_start",
            "condorcet_end",
            "condorcet_true",
            "condorcet_proportion",
            ["time_steps", "cand_sampler"],
        )

        proximity_to_sp_end = compute_average(
            data_delib, "proximity_to_cand_sp_end", "proximity_to_cand_sp", ["time_steps", "cand_sampler"]
        )
        proximity_to_sp_true = compute_average(
            data_delib, "proximity_to_cand_sp_true", "proximity_to_cand_sp", ["time_steps", "cand_sampler"]
        )
        proximity_to_sp_end["Type"] = "Simulated"
        proximity_to_sp_true["Type"] = "True"
        df_combined = pd.concat([proximity_to_sp_end, proximity_to_sp_true])
        proximity_to_sp = df_combined.rename(
            columns={"proximity_to_start": "proximity_to_cand_sp"}
        )

        proximity_to_voter_sp_end = compute_average(
            data_delib, "proximity_to_voter_sp_end", "proximity_to_voter_sp", ["time_steps", "cand_sampler"]
        )
        proximity_to_voter_sp_true = compute_average(
            data_delib, "proximity_to_voter_sp_true", "proximity_to_voter_sp", ["time_steps", "cand_sampler"]
        )
        proximity_to_voter_sp_end["Type"] = "Simulated"
        proximity_to_voter_sp_true["Type"] = "True"
        df_combined = pd.concat([proximity_to_voter_sp_end, proximity_to_voter_sp_true])
        proximity_to_voter_sp = df_combined.rename(
            columns={"proximity_to_start": "proximity_to_voter_sp"}
        )

        unique_profiles_end = compute_average(
            data_delib, "unique_end", "unique", ["time_steps", "cand_sampler"]
        )
        unique_profiles_true = compute_average(
            data_delib, "unique_true", "unique", ["time_steps", "cand_sampler"]
        )
        unique_profiles_end["Type"] = "Simulated"
        unique_profiles_true["Type"] = "True"
        df_combined = pd.concat([unique_profiles_end, unique_profiles_true])
        unique_profiles = df_combined.rename(
            columns={"unique_start": "unique_profiles"}
        )

        # === Plotting all variants in one figure ===
        plot(cyclic, "time_steps", "cyclic_proportion", "Mean Number of Cyclic Profiles", file_prefix)
        plot(proximity_to_sp, "time_steps", "proximity_to_cand_sp", "Mean candidate proximity to single peaked Profiles", file_prefix)
        plot(proximity_to_voter_sp, "time_steps", "proximity_to_voter_sp", "Mean voter proximity to single peaked Profiles", file_prefix)
        plot(condorcet, "time_steps", "condorcet_proportion", "Mean number of Condorcet winners", file_prefix)
        plot(unique_profiles, "time_steps", "unique", "Mean number of Unique Preferences", file_prefix)


    generate_general_graphs(data_delib, "delib")
    # generate_general_graphs(data_control, "control")
    return (generate_general_graphs,)


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
    plot_summary(summary_delib, "error_scatter_delib.png")
    plot_summary(summary_control, "error_scatter_control.png")
    return (plot_summary,)


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
    convergence_data_simi = pd.read_csv("results/degroot_deliberation_100_convergence_similarity.csv")
    convergence_data_know = pd.read_csv("results/degroot_deliberation_100_convergence_knowledge.csv")
    convergence_data_simi_group = pd.read_csv("results/degroot_deliberation_100_convergence_similarity_grouped.csv")
    convergence_data_know_group = pd.read_csv("results/degroot_deliberation_100_convergence_knowledge_grouped.csv")
    return (
        convergence_data_know,
        convergence_data_know_group,
        convergence_data_simi,
        convergence_data_simi_group,
    )


@app.cell
def _(
    convergence_data_know,
    convergence_data_know_group,
    convergence_data_simi,
    convergence_data_simi_group,
    plt,
    sim_color,
    time_str,
):
    # Begin plotting
    fig, ax = plt.subplots(1, 4, figsize=(20, 5), sharex=True)
    ax = ax.ravel()
    for i, convergence_data in enumerate([convergence_data_simi,  convergence_data_know, convergence_data_simi_group,  convergence_data_know_group]):

        grouped_by_cand_and_sampler = convergence_data.loc[convergence_data[time_str] > 1].groupby([time_str]).mean(numeric_only=True)

        # Plot each combination
        ax[0+i].plot(grouped_by_cand_and_sampler.index, grouped_by_cand_and_sampler['entrywise_distance'], 
                     linestyle='--', color=sim_color)

    ax[0].set_title('Similarity')
    ax[1].set_title('Knowledge')
    ax[2].set_title('Similarity, Original Groups')
    ax[3].set_title('Knowledge, Original Groups')

    # Labels and titles
    ax[0].set_ylabel('$\ell_1$-norm to Starting Trust')
    ax[0].set_xlabel('Time Steps')
    ax[1].set_xlabel('Time Steps')
    ax[2].set_xlabel('Time Steps')
    ax[3].set_xlabel('Time Steps')

    # Legend
    handles, labels = ax[1].get_legend_handles_labels()
    fig.legend(handles, labels, loc='lower center', ncol=3,
               bbox_to_anchor=(0.51, -0.15))

    plt.tight_layout()
    fig.savefig("figures/convergence_groups.png", bbox_inches='tight')
    plt.show()
    return (
        ax,
        convergence_data,
        fig,
        grouped_by_cand_and_sampler,
        handles,
        i,
        labels,
    )


@app.cell(hide_code=True)
def _(mo):
    mo.md(
        r"""
        ## Opinion Replication


        We now proceed to compare the opinions of voters in the simulation to their true opinion after deliberation
        """
    )
    return


@app.cell
def _(np, pd):
    opinion_delib_df = pd.read_csv("results/degroot_pbs.csv")
    opinion_control_df = pd.read_csv("results/degroot_pbs_control.csv")
    pbs_measures = ["PBS_start", "PBS_simulated", "PBS_true"]

    def get_exploded_df(opinion_df):

        for pbs in pbs_measures:
            opinion_df[pbs] = opinion_df[pbs].apply(lambda x: list(map(np.float64,x.strip("\"\',").split(","))))

        opinion_df = opinion_df.explode(pbs_measures)

        for pbs in pbs_measures:
            opinion_df[pbs] = opinion_df[pbs].astype(np.float64)


        opinion_df["PBS_error"] = pow(opinion_df["PBS_simulated"] - opinion_df["PBS_true"], 2)
        return opinion_df

    opinion_delib_df = get_exploded_df(opinion_delib_df)
    opinion_control_df = get_exploded_df(opinion_control_df)
    opinion_delib_df
    return get_exploded_df, opinion_control_df, opinion_delib_df, pbs_measures


@app.cell
def _(np, opinion_delib_df, pd, sklearn, sm, time_str):
    independent_variables = ['knowledge', 'ego', 'similarity', 'selfknowledge']
    def fit_regression(opinion_df):

        opinion_group = opinion_df.loc[opinion_df[time_str]> 0].groupby(independent_variables).mean(numeric_only=True)

        xs = opinion_group.index.to_numpy()
        x = np.array([np.array(x) for x in xs])
        y = opinion_group["PBS_simulated"]

        poly = sklearn.preprocessing.PolynomialFeatures(degree=2, include_bias=False)
        X_poly = poly.fit_transform(x)
        feature_names = poly.get_feature_names_out(input_features=[independent_variables[i] for i in range(x.shape[1])])

        # Create DataFrame for X_poly with column names
        X_poly_df = pd.DataFrame(X_poly, columns=feature_names, index=y.index)

        model = sm.OLS(y, X_poly_df).fit()
        print(model.summary())

    fit_regression(opinion_delib_df)
    # fit_regression(opinion_control_df)
    return fit_regression, independent_variables


@app.cell
def _(
    np,
    opinion_control_df,
    opinion_delib_df,
    pd,
    plt,
    sim_color,
    time_str,
    true_color,
):
    def plot_opinion(opinion_df, axes):
        times = opinion_df[time_str].unique()
        times = np.sort(times)  # ensure consistency

        for i, ax in enumerate(axes):
            time_val = times[i]  # skip every other time point
            filtered = opinion_df[np.isclose(opinion_df[time_str], time_val)]
            if len(filtered) == 0:
                continue

            opinion_plotting_data = filtered.sample(n=min(100000, len(filtered)))
            _, bins = pd.cut(opinion_plotting_data["PBS_start"], 80, retbins=True)


            pbs_start = opinion_plotting_data["PBS_start"]
            pbs_sim = opinion_plotting_data["PBS_simulated"]
            pbs_true = opinion_plotting_data["PBS_true"]
            digitized_start = np.digitize(pbs_start, bins, right=True)
            bin_means_start = np.array([pbs_start[digitized_start == i].mean() for i in range(1, len(bins))])
            bin_means_sim = np.array([pbs_sim[digitized_start == i].mean() for i in range(1, len(bins))])
            bin_means_true = np.array([pbs_true[digitized_start == i].mean() for i in range(1, len(bins))])

            ax.scatter(pbs_start, pbs_sim, alpha=0.1, color=sim_color, s=3, label="Simulated")
            ax.scatter(pbs_start, pbs_true, alpha=0.05, color=true_color, s=3, label="True")
            ax.scatter(bin_means_start, bin_means_sim, color=sim_color, label="Binned Sim")
            ax.scatter(bin_means_start, bin_means_true, color=true_color, label="Binned True")

            ax.set_xlabel(f"t = {time_val:.2f}")
            ax.set_title(f"Mean Absolute Error: {np.abs((bin_means_sim[~np.isnan(bin_means_sim)] - bin_means_true[~np.isnan(bin_means_true)])).mean():.2f}")
            ax.set_ylim((0,10))
            ax.grid(True)


    all_zero_delib = opinion_delib_df.loc[(opinion_delib_df["selfknowledge"] == 0)]
    # Usage
    figure_opinion, axes = plt.subplots(2, 4, figsize=(20, 10))
    axes = axes.ravel()
    plot_opinion(all_zero_delib, axes[:4])
    plot_opinion(opinion_control_df.loc[opinion_control_df["ego"] == 1], axes[4:])
    axes[0].set_ylabel("Deliberation\n PBS")
    axes[4].set_ylabel("Control\n PBS")
    plt.tight_layout()
    plt.savefig("figures/pbs_scores.png")
    plt.show()
    return all_zero_delib, axes, figure_opinion, plot_opinion


@app.cell
def _(opinion_delib_df):
    opinion_delib_df
    return


@app.cell
def _(all_zero_delib, np, pd, plt, sim_color, time_str, true_color):
    def plot_change_in_opinion(opinion_df, axes):
        times = opinion_df[time_str].unique()
        times = np.sort(times)  # ensure consistency

        for i, ax in enumerate(axes):
            time_val = times[i]  # skip every other time point
            filtered = opinion_df[np.isclose(opinion_df[time_str], time_val)]
            if len(filtered) == 0:
                continue

            opinion_plotting_data = filtered.sample(n=min(100000, len(filtered)))
            _, bins = pd.cut(opinion_plotting_data["PBS_start"], 80, retbins=True)

            # Prepare data
            pbs_start = opinion_plotting_data["PBS_start"]
            pbs_sim = opinion_plotting_data["PBS_simulated"] - pbs_start
            pbs_true = opinion_plotting_data["PBS_true"] - pbs_start

            # Use bin_edges with np.digitize
            digitized_start = np.digitize(pbs_start, bins, right=True)
            bin_means_start = np.array([pbs_start[digitized_start == i].mean(skipna=True) for i in range(1, len(bins))])
            bin_means_sim = np.array([pbs_sim[digitized_start == i].mean(skipna=True) for i in range(1, len(bins))])
            bin_means_true = np.array([pbs_true[digitized_start == i].mean(skipna=True) for i in range(1, len(bins))])
            ax.scatter(pbs_start, pbs_sim, alpha=0.05, color=sim_color, s=3, label="Simulated")
            ax.scatter(pbs_start, pbs_true, alpha=0.05, color=true_color, s=3, label="True")
            ax.scatter(bin_means_start, bin_means_sim, color=sim_color, label="Binned Sim")
            ax.scatter(bin_means_start, bin_means_true, color=true_color, label="Binned True")

            ax.set_xlabel(f"t = {time_val:.2f}")
            ax.set_title(f"Mean Absolute Error: {np.abs((bin_means_sim[~np.isnan(bin_means_sim)] - bin_means_true[~np.isnan(bin_means_true)])).mean():.2f}")
            ax.set_ylim((-3,3))
            ax.grid(True)


    # Usage
    figure_opinion_change, axes_change = plt.subplots(1, 4, figsize=(24, 5))
    axes_change = axes_change.ravel()
    plot_change_in_opinion(all_zero_delib, axes_change[:4])
    axes_change[0].set_ylabel("$\\Delta$PBS score")
    plt.tight_layout()
    plt.savefig("figures/change_pbs_scores.png")
    plt.show()
    return axes_change, figure_opinion_change, plot_change_in_opinion


@app.cell
def _(opinion_delib_df, time_str):
    opinion_delib_df["uniform"] = ~opinion_delib_df[["credibility", "knowledge", "ego", "similarity"]].any(axis=1)

    def plot_errors(opinion_df, ax):
        independent_variables = ['knowledge', 'selfknowledge', 'ego', 'similarity','uniform']
        for indep in independent_variables:
            avg_error = opinion_df.loc[opinion_df[indep] == 1].groupby(time_str).mean(numeric_only=True)["PBS_error"]
            ax.plot(avg_error.index, avg_error, "o-",alpha=0.3, label=indep)
        ax.set_xlabel("Time")
    return (plot_errors,)


@app.cell
def _(np, opinion_delib_df, plot_errors, plt, time_str):
    def plot_errors_binned(opinion_df, ax):
        bins = np.linspace(0, 10, 100)

        independent_variables = ['knowledge', 'selfknowledge', 'ego', 'similarity','uniform']
        for indep in independent_variables:
            time_errors = []
            times = opinion_df[time_str].unique()
            times.sort()
            for time in times:
                opinion_plotting_data = opinion_df.loc[(opinion_df[indep] == 1) & (opinion_df[time_str] == time)]
                pbs_start = opinion_plotting_data["PBS_start"]
                pbs_sim = opinion_plotting_data["PBS_simulated"] - pbs_start
                pbs_true = opinion_plotting_data["PBS_true"] - pbs_start

                digitized_start = np.digitize(pbs_start, bins, right=True)
                bin_means_sim = np.array([pbs_sim[digitized_start == i].mean() for i in range(1, len(bins))])
                bin_means_true = np.array([pbs_true[digitized_start == i].mean() for i in range(1, len(bins))])
                time_errors.append(np.abs((bin_means_sim[~np.isnan(bin_means_sim)] - bin_means_true[~np.isnan(bin_means_true)])).mean())

            ax.plot(times, time_errors,"o-", alpha=0.3, label=indep.capitalize())
            ax.set_xlabel("Time")
            ax.grid(True)

    figure_errors_bin, axes_err_bin = plt.subplots(1,2, figsize=(18,8))
    plot_errors(opinion_delib_df, axes_err_bin[0])
    plot_errors_binned(opinion_delib_df, axes_err_bin[1])
    axes_err_bin[0].set_ylabel("PBS Error")

    plt.legend()
    plt.savefig("figures/errors_binned.png")
    plt.show()
    return axes_err_bin, figure_errors_bin, plot_errors_binned


@app.cell
def _(np, opinion_delib_df, pd, plt, time_str):
    # Filter the DataFrame
    # filtered_df = opinion_delib_df[opinion_delib_df["selfknowledge"]==0]
    filtered_df = opinion_delib_df

    # Create bins of width 0.5 for bias
    bin_edges = np.arange(filtered_df["bias"].min(), filtered_df["bias"].max() + 0.2, 0.2)
    filtered_df["bias_bin"] = pd.cut(filtered_df["bias"], bins=bin_edges)

    # Group, aggregate, and pivot
    im_show_bias_time_df = (
        filtered_df
        .groupby(["bias_bin", time_str])
        .mean(numeric_only=True)
        .reset_index()
        .pivot(index="bias_bin", columns=time_str, values="PBS_error")
    )
    # Create the fig_imshowure
    fig_imshow, ax_imshow = plt.subplots(figsize=(8, 8))  # Square figure

    # Show the heatmap
    cax_imshow = ax_imshow.imshow(im_show_bias_time_df.values, aspect='auto', origin='lower')

    # Add colorbar
    fig_imshow.colorbar(cax_imshow, ax=ax_imshow, label="PBS error")

    # Set ax_imshowis ticks
    ax_imshow.set_xticks(range(0,im_show_bias_time_df.columns.shape[0],3))
    ax_imshow.set_xticklabels(im_show_bias_time_df.columns[::3], rotation=90)

    label_step_size = im_show_bias_time_df.index.shape[0]  // 10
    ax_imshow.set_yticks(range(0, im_show_bias_time_df.index.shape[0], label_step_size))
    ax_imshow.set_yticklabels([f"{bias.left:.2f}" for bias in im_show_bias_time_df.index[::label_step_size]])

    # Labels
    ax_imshow.set_xlabel("Time")
    ax_imshow.set_ylabel("Bias")

    plt.tight_layout()
    plt.savefig("figures/bias_time_imshow.png")
    plt.show()
    return (
        ax_imshow,
        bin_edges,
        cax_imshow,
        fig_imshow,
        filtered_df,
        im_show_bias_time_df,
        label_step_size,
    )


@app.cell
def _(independent_variables, opinion_delib_df):
    opinion_delib_df.groupby(independent_variables).mean(numeric_only=True)["PBS_error"]
    return


@app.cell
def _(opinion_delib_df, sm, time_str):
    from statsmodels.formula.api import ols
    fit_data = opinion_delib_df.loc[opinion_delib_df[time_str] == 1]
    model = ols("PBS_error ~ C(knowledge) * C(ego) * C(similarity) * C(selfknowledge)", data=fit_data).fit()
    anova_table = sm.stats.anova_lm(model, test="F", typ=2, robust="hc3")
    anova_table.iloc[0:4][["F", "PR(>F)"]]
    return anova_table, fit_data, model, ols


if __name__ == "__main__":
    app.run()
