import marimo

__generated_with = "0.11.14"
app = marimo.App(width="full")


@app.cell
def _():
    import marimo as mo
    import polars as pl
    import sklearn as sk
    import numpy as np
    import matplotlib.pyplot as plt
    from enum import Enum
    import textwrap

    plt.style.use("default")
    plt.rcParams.update(
        {
            "font.size": 23,
            "axes.labelsize": 23,
            "axes.titlesize": 23,
            "axes.facecolor": "white",
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
    return Enum, mo, np, pl, plt, sk, textwrap


@app.cell
def _(Enum):
    class Response(Enum):
        VALID = "0"
        NO_OPINION = "77"
        REFUSED = "98"
        INVALID = "-1"
        MULTIPLE_REPONSE = "-8"

    def from_value(value: int):
        if value in {"98", "99", "998", "999"}:
            return 98
        if value in {"77", "777"}:
            return 77
        elif value == " ":
            return -1
        elif value == "-8":
            return -8
        else:
            return 0
    return Response, from_value


@app.cell
def _(pl):
    questionnaire_questions = pl.read_csv(
        "data/questionnaire.csv",
        separator=";",
        has_header=False,
        new_columns=["code", "question"],
    )

    question_dict = {}
    for [q_code, text] in questionnaire_questions.iter_rows():
        question_dict[q_code] = text.replace("]", "").replace("[", "")

    data = pl.read_csv("data/copy of Stanford_A1R_Dataset_APSR.csv")
    data = data.with_columns(pl.int_range(data.shape[0]).alias("ID"))
    sql_context = pl.SQLContext()

    print(questionnaire_questions)
    print(data)
    print(data.columns)
    return (
        data,
        q_code,
        question_dict,
        questionnaire_questions,
        sql_context,
        text,
    )


@app.cell
def _(data, from_value, pl, sql_context):
    df_long = data.unpivot(
        index=[
            "ID",
            "WEIGHT_CONTROL",
            "WEIGHT_DELEGATE",
            "POST",
            "GROUP",
            "CONDITION",
            "GENDER",
            "AGE",
            "AGE4",
            "RACETHNICITY",
            "EDUC4",
        ],  # Identify individuals
        on=[
            col
            for col in data.columns
            if col.startswith("Q")
            or col.startswith("T2Q")
            or (col.startswith("PK") and not col.endswith("TIME"))
            or (col.startswith("T2PK") and not col.endswith("TIME"))
            or "PARTYID3" in col
            or col.startswith("D")
            or col.startswith("T2D")
            or "LIBCONV" in col
        ],  # Select all question columns
        variable_name="Question",
        value_name="Response",
    )
    df_long = df_long.with_columns(
        pl.when(df_long["Question"].str.starts_with("T2"))
        .then(pl.lit("Post"))
        .otherwise(pl.lit("Pre"))
        .alias("Timepoint")
    )

    df_long = df_long.with_columns(
        df_long["Question"].str.replace("^T2", "").alias("Question")
    )
    df_long = df_long.with_columns(
        df_long["Response"]
        .map_elements(from_value, return_dtype=int)
        .alias("ResponseType")
    )

    sql_context.register("data_long", df_long)
    print(df_long)
    return (df_long,)


@app.cell(hide_code=True)
def _(mo):
    mo.md(
        r"""
        We now have a sql database containing all data points, which we can now query with asking for the pre- and post-deliberation questionnaire results.

        Now we focus on cleaning the data.
        """
    )
    return


@app.cell
def _(np, pl, plt, question_dict, sql_context, textwrap):
    def gen_response_query(cols, q, response_type, time):
        return f"""
            SELECT {" ".join(cols)}
            FROM data_long
            WHERE Question = '{q}' AND ResponseType = {response_type} AND Timepoint = '{time}' AND CONDITION = 1
        """

    questions_query = """
        SELECT DISTINCT Question from data_long
    """

    questions = sql_context.execute(questions_query).collect()

    num_questions = len(questions)
    # Ceiling division to get number of figures
    num_figures = (num_questions + 9) // 10

    for fig_num in range(num_figures):
        # Create a new figure with increased size for better readability
        plt.figure(figsize=(35, 14))

        # Calculate the range of questions for this figure
        start_idx = fig_num * 10
        end_idx = min((fig_num + 1) * 10, num_questions)

        # Create 2x5 subplot grid
        for subplot_idx, question in enumerate(
            questions["Question"][start_idx:end_idx], 1
        ):
            plt.subplot(2, 5, subplot_idx)

            q = gen_response_query(["Response"], question, 0, "Pre")
            q2 = gen_response_query(["Response"], question, 0, "Post")
            q_data = sql_context.execute(q).collect().cast(pl.Int64)
            q2_data = sql_context.execute(q2).collect().cast(pl.Int64)

            full_title = question_dict.get(question, question)
            if question.startswith("PK"):
                full_title = "PK: " + full_title
            elif any([x in question for x in ["PARTYID", "D"]]):
                full_title = "Affliation: " + full_title
            full_title = full_title.replace("$", r"\$")

            # Create histogram
            q_data = q_data.to_numpy().squeeze()
            q2_data = q2_data.to_numpy().squeeze()

            # Create histogram
            # Create histogram
            counts, bin_edges = np.histogram(np.concatenate([q_data, q2_data]), bins=10)

            # Plot histogram
            plt.hist(
                [q_data, q2_data],
                bins=bin_edges,
                label=["Pre", "Post"],
                alpha=0.7,
                density=True,
            )

            # Set x-ticks to the bin edges
            plt.xticks(bin_edges.astype(int), rotation=45)
            wrapped_title = textwrap.fill(full_title, width=40)
            plt.legend()

            # Add wrapped title
            plt.title(wrapped_title, ha="center")
            plt.tight_layout()
        plt.savefig(f"figures/{question}.png")

    # Show all figures
    plt.show()
    return (
        bin_edges,
        counts,
        end_idx,
        fig_num,
        full_title,
        gen_response_query,
        num_figures,
        num_questions,
        q,
        q2,
        q2_data,
        q_data,
        question,
        questions,
        questions_query,
        start_idx,
        subplot_idx,
        wrapped_title,
    )


@app.cell(hide_code=True)
def _(mo):
    mo.md(
        r"""
        From here we can make the following observations:

        - Political knowledge increased
        - Political knowledge did not increase on all questions evenly, e.g., the question in the number of undocumented immigrants shows larger change
        - After deliberation, people had more trust in the American system of democracy
        - Opinion shifted on many questions.

        Looking at the political affiliation of the voters, we note:

        - Before deliberation most people considered themselves an independent, with democrats as a second
        - After deliberation the number of republicans decreased, with both democrats and independents increasing
        - Looking at just democrats and republications, we see that most independents do lean more republican.
        - When it comes to the Liberal - conservative spectrum, most people consider themselves somewhat in the middle, with post deliberation people skewing more democrat
        - We can also see the rating of the democrats increase after deliberation, republicans also are rated higher but less so. For both parties, the number of 0's decreases drastically.
        """
    )
    return


@app.cell
def _(sql_context):
    def gen_voter_query(ids, cond):
        if cond != "":
            return f"""
                SELECT *
                FROM data_long
                WHERE ID in ({", ".join([str(id) for id in ids])}) AND {cond}
            """
        else:
            return f"""
                SELECT *
                FROM data_long
                WHERE ID in ({", ".join([str(id) for id in ids])})
            """

    def gen_group_voter_query(group, cond):
        if cond != "":
            return f"""
                SELECT *
                FROM data_long
                WHERE GROUP = '{group}' AND {cond}
            """
        else:
            return f"""
                SELECT *
                FROM data_long
                WHERE GROUP = '{group}'
            """

    def get_voters(ids=None, group=None, cond=""):
        assert ids is not None or group is not None
        if ids is not None:
            q = gen_voter_query(ids, cond)
            return sql_context.execute(q).collect()
        else:
            q = gen_group_voter_query(group, cond)
            return sql_context.execute(q).collect()

    def get_voter_preferences(ids=None, group=None, cond=""):
        """
        Retrieves voter data in a wide format.
        """
        voter_data = get_voters(ids=ids, group=group, cond=cond)

        # Pivot back to wide format
        df_wide = voter_data.pivot(
            values="Response",
            index=[
                "ID",
                "WEIGHT_CONTROL",
                "WEIGHT_DELEGATE",
                "POST",
                "GROUP",
                "CONDITION",
                "GENDER",
                "AGE",
                "AGE4",
                "RACETHNICITY",
                "EDUC4",
            ],  # Keep identifying columns as index
            on="Question",  # Spread responses back into separate columns
        )

        return df_wide

    print(get_voter_preferences(group=6, cond="Timepoint = 'Pre'").columns)
    return (
        gen_group_voter_query,
        gen_voter_query,
        get_voter_preferences,
        get_voters,
    )


@app.cell(hide_code=True)
def _(mo):
    mo.md("""We can now select individual voters, we will now setup a proceedure that generates "states of the world", meaning that we generate a set of possible outcomes, which the individuals will have preferences over, depending on the absolute distance from their current possition.""")
    return


@app.cell
def _(from_value, get_voter_preferences, np, pl, questions):
    def generate_states(n_states, variables):
        states = {}
        for state in range(n_states):
            state_opinion = np.random.randint(0, 11, len(variables))
            states[state] = state_opinion
        return states

    def voter_preference_over(states, voter):
        assert states[0].shape == voter.shape
        opinion_weights = [1 if from_value(op) == 0 else 1 for op in voter]
        return sorted(
            states.keys(),
            key=lambda k: sum(opinion_weights * abs(voter - states[k]) ** 2),
        )

    poll_questions = [q for q in questions["Question"] if "Q" in q]
    world_states = generate_states(10, poll_questions)
    voters = get_voter_preferences(group=6, cond="Timepoint = 'Pre'")[poll_questions]
    voters = voters.cast(pl.Int64)
    voter_preferences = np.apply_along_axis(
        lambda v: voter_preference_over(world_states, v), 1, voters.to_numpy()
    )
    return (
        generate_states,
        poll_questions,
        voter_preference_over,
        voter_preferences,
        voters,
        world_states,
    )


@app.cell
def _(get_voters):
    first_thousand = get_voters(ids=[*range(1000)])
    print([x  for x in first_thousand["Question"].unique()if "PK" in x])
    PK_correct_answers = {
        "PK1": 1,
        "PK2": 1,
        "PK3": 1,
        "PK4": 1,
        "PK6": 1,
        "PK7": 1,
        "PK8": 1,
        "PK9": 1,
    }
    return PK_correct_answers, first_thousand


if __name__ == "__main__":
    app.run()
