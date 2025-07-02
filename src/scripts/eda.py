import marimo

__generated_with = "0.12.0"
app = marimo.App(width="full")


@app.cell
def _():
    import marimo as mo
    import polars as pl
    import sqlite3
    import sklearn as sk
    import numpy as np
    import matplotlib.pyplot as plt
    from enum import Enum
    import seaborn as sns
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
            "font.family": "Charter",
        }
    )
    return Enum, mo, np, pl, plt, sk, sns, sqlite3, textwrap


@app.cell
def _(Enum):
    PK_correct_answers = {
        "code": [
            "PK1",
            "PK2",
            "PK3",
            "PK4",
            "PK6",
            "PK7",
        ],
        "answer": [
            2,  # Which political Party holds a majority in the sensate
            1,  # Which political Party holds a majority in the house
            1,  # About how many undocumented immigrants are in teh US
            4,  # Which of the follwoing countries is not part of the paris agreement
            2,  # What percentage is the highest tax rate for capital gains taxes
            2,  # Which of the following organizations dealing with trade has the most countries
        ],
    }

    class Response(Enum):
        VALID = "0"
        NO_OPINION = "77"
        REFUSED = "98"
        INVALID = "-1"
        MULTIPLE_REPONSE = "-8"

    def from_value(value: int):
        if type(value) != int:
            try:
                value = float(value)
            except:
                return None
        if value >= 0 and value <= 10:
            return int(value)
    return PK_correct_answers, Response, from_value


@app.cell
def _(PK_correct_answers, pl):
    questionnaire_questions = pl.read_csv(
        "data/questionnaire.csv",
        separator=";",
        has_header=False,
        new_columns=["code", "question"],
    )

    questions = questionnaire_questions.filter(
        pl.col("code").str.contains("Q") & ~pl.col("code").str.contains("T2")
    )
    pk = questionnaire_questions.filter(
        pl.col("code").str.contains("PK") & (~pl.col("code").str.contains("T2"))
    )
    pk = pk.join(pl.from_dict(PK_correct_answers), on="code")
    question_dict = {}
    for [q_code, text] in questionnaire_questions.iter_rows():
        question_dict[q_code] = text.replace("]", "").replace("[", "")

    data = pl.read_csv("data/copy of Stanford_A1R_Dataset_APSR.csv")
    data = data.with_columns(pl.int_range(data.shape[0]).alias("ID"))
    sql_context = pl.SQLContext()

    print(questions)
    print(pk)
    print(data)
    return (
        data,
        pk,
        q_code,
        question_dict,
        questionnaire_questions,
        questions,
        sql_context,
        text,
    )


@app.cell
def _(pk, questions):
    connection_string = "sqlite:///data/a1r.db"

    questions.write_database(
        table_name="questionnaire",
        connection=connection_string,
        if_table_exists="replace",
    )
    pk.write_database(
        table_name="political_knowledge",
        connection=connection_string,
        if_table_exists="replace",
    )
    return (connection_string,)


@app.cell
def _(connection_string, data, from_value, pk, pl):
    responses_pre = data.select(
        col for col in data.columns if col.startswith("Q") or col == "ID"
    )

    responses_pre = responses_pre.with_columns(
        [
            pl.col(col).map_elements(from_value, return_dtype=pl.Int16).alias(col)
            for col in responses_pre.columns
            if col != "ID"
        ]
    )

    responses_post = data.select(
        col for col in data.columns if col.startswith("T2Q") or col == "ID"
    )

    responses_post = responses_post.with_columns(
        [
            pl.col(col).map_elements(from_value, return_dtype=pl.Int16).alias(col)
            for col in responses_post.columns
            if col != "ID"
        ]
    )

    responses_post = responses_post.rename(
        {q: q.replace("T2", "") for q in responses_post.columns}
    )

    PK_past = data.select(
        col
        for col in data.columns
        if (col.startswith("PK") and "TIME" not in col) or col == "ID"
    )

    PK_past = PK_past.with_columns(
        [
            pl.col(col).map_elements(from_value, return_dtype=pl.Int16).alias(col)
            for col in PK_past.columns
            if col != "ID"
        ]
    )

    columns_to_process = [col for col in PK_past.columns if col != "ID"]

    PK_correct = PK_past.with_columns(
        [
            pl.col(col)
            .map_elements(
                lambda x: x
                == (pk.filter(pl.col("code") == col).get_column("answer")[0]),
                return_dtype=pl.Boolean,
            )
            .alias(col + "_correct")
            for col in columns_to_process
        ]
    )

    pk_correct_questions_labels = [
        col for col in PK_correct.columns if "correct" in col
    ]
    PK_correct = PK_correct.with_columns(
        pl.mean_horizontal(pk_correct_questions_labels).alias("score")
    )

    print(responses_pre.fill_null(strategy="mean"))

    responses_pre.write_database(
        table_name="response_pre",
        connection=connection_string,
        if_table_exists="replace",
    )
    responses_post.write_database(
        table_name="response_post",
        connection=connection_string,
        if_table_exists="replace",
    )
    PK_correct.write_database(
        table_name="response_PK",
        connection=connection_string,
        if_table_exists="replace",
    )
    return (
        PK_correct,
        PK_past,
        columns_to_process,
        pk_correct_questions_labels,
        responses_post,
        responses_pre,
    )


@app.cell
def _(connection_string, data):
    # voter information

    print(data.columns)
    info_columns = [
        "GROUP",
        "CONDITION",
        "GENDER",
        "AGE",
        "RACETHNICITY",
        "EDUC4",
        "ID",
        "LIBCONV",
        "D1",
        "D2D",
        "D2R",
        "D2I",
        "T2D1",
    ]

    voter_info = data.select(info_columns)
    voter_info.write_database(
        table_name="voter_info", connection=connection_string, if_table_exists="replace"
    )

    voter_info.head(100)
    return info_columns, voter_info


@app.cell
def _(sqlite3):
    cursor = sqlite3.connect("data/a1r.db").cursor()
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table'")
    tables = cursor.fetchall()
    for table in tables:
        table_name = table[0]
        print(f"\nSchema for table: {table_name}")
        cursor.execute(f"PRAGMA table_info({table_name})")
        columns = cursor.fetchall()
        for column in columns:
            print(column)
    return column, columns, cursor, table, table_name, tables


@app.cell(hide_code=True)
def _():
    # I    index=[
    #         "ID",
    #         "WEIGHT_CONTROL",
    #         "WEIGHT_DELEGATE",
    #         "POST",
    #         "GROUP",
    #         "CONDITION",
    #         "GENDER",
    #         "AGE",
    #         "AGE4",
    #         "RACETHNICITY",
    #         "EDUC4",
    #     ],  # Identify individuals
    #     on=[
    #         col
    #         for col in data.columns
    #         if col.startswith("Q")
    #         or col.startswith("T2Q")
    #         or (col.startswith("PK") and not col.endswith("TIME"))
    #         or (col.startswith("T2PK") and not col.endswith("TIME"))
    #         or "PARTYID3" in col
    #         or col.startswith("D")
    #         or col.startswith("T2D")
    #         or "LIBCONV" in col
    #     ],  # Select all question columns
    #     variable_name="Question",
    #     value_name="Response",
    # )
    # df_long = df_long.with_columns(
    #     pl.when(df_long["Question"].str.starts_with("T2"))
    #     .then(pl.lit("Post"))
    #     .otherwise(pl.lit("Pre"))
    #     .alias("Timepoint")
    # )

    # df_long = df_long.with_columns(
    #     df_long["Question"].str.replace("^T2", "").alias("Question")
    # )
    # df_long = df_long.with_columns(
    #     df_long["Response"]
    #     .map_elements(from_value, return_dtype=int)
    #     .alias("ResponseType")
    # )

    # sql_context.register("data_long", df_long)
    # print(df_long)
    return


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
def gen_response_query():
    def gen_response_query(cols, q, response_type, time):
        return f"""
            SELECT {" ".join(cols)}
            FROM data_long
            WHERE Question = '{q}' AND ResponseType = {response_type} AND Timepoint = '{time}' AND CONDITION = 1
        """

    # questions_query = """
    #     SELECT DISTINCT Question from data_long
    # """

    # questions = sql_context.execute(questions_query).collect()

    # num_questions = len(questions)
    # # Ceiling division to get number of figures
    # num_figures = (num_questions + 9) // 10

    # for fig_num in range(num_figures):
    #     # Create a new figure with increased size for better readability
    #     plt.figure(figsize=(35, 14))

    #     # Calculate the range of questions for this figure
    #     start_idx = fig_num * 10
    #     end_idx = min((fig_num + 1) * 10, num_questions)

    #     # Create 2x5 subplot grid
    #     for subplot_idx, question in enumerate(
    #         questions["Question"][start_idx:end_idx], 1
    #     ):
    #         plt.subplot(2, 5, subplot_idx)

    #         q = gen_response_query(["Response"], question, 0, "Pre")
    #         q2 = gen_response_query(["Response"], question, 0, "Post")
    #         q_data = sql_context.execute(q).collect().cast(pl.Int64)
    #         q2_data = sql_context.execute(q2).collect().cast(pl.Int64)

    #         full_title = question_dict.get(question, question)
    #         if question.startswith("PK"):
    #             full_title = "PK: " + full_title
    #         elif any([x in question for x in ["PARTYID", "D"]]):
    #             full_title = "Affliation: " + full_title
    #         full_title = full_title.replace("$", r"\$")

    #         # Create histogram
    #         q_data = q_data.to_numpy().squeeze()
    #         q2_data = q2_data.to_numpy().squeeze()

    #         # Create histogram
    #         # Create histogram
    #         counts, bin_edges = np.histogram(np.concatenate([q_data, q2_data]), bins=10)

    #         # Plot histogram
    #         plt.hist(
    #             [q_data, q2_data],
    #             bins=bin_edges,
    #             label=["Pre", "Post"],
    #             alpha=0.7,
    #             density=True,
    #         )

    #         # Set x-ticks to the bin edges
    #         plt.xticks(bin_edges.astype(int), rotation=45)
    #         wrapped_title = textwrap.fill(full_title, width=40)
    #         plt.legend()

    #         # Add wrapped title
    #         plt.title(wrapped_title, ha="center")
    #         plt.tight_layout()
    #     plt.savefig(f"figures/{question}.png")

    # # Show all figures
    # plt.show()
    return (gen_response_query,)


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


@app.cell(hide_code=True)
def _(mo):
    mo.md("""We can now select individual voters, we will now setup a proceedure that generates "states of the world", meaning that we generate a set of possible outcomes, which the individuals will have preferences over, depending on the absolute distance from their current possition.""")
    return


@app.cell
def _(pl, sqlite3):
    conn = sqlite3.connect("data/a1r.db")
    pre_deliberation_responses = pl.read_database(
        query="""
                     SELECT * 
                     FROM response_pre
                     """,
        connection=conn,
    ).drop_nulls()

    pre_affiliation = pl.read_database(
        query="""
                     SELECT ID, D1, T2D1 
                     FROM voter_info
                     """,
        connection=conn,
    ).drop_nulls()

    post_deliberation_responses = pl.read_database(
        query="""
                     SELECT * 
                     FROM response_post
                     """,
        connection=conn,
    ).drop_nulls()

    pre_deliberation_responses_affiliation = pre_deliberation_responses.join(
        pre_affiliation, how="inner", on="ID"
    )
    return (
        conn,
        post_deliberation_responses,
        pre_affiliation,
        pre_deliberation_responses,
        pre_deliberation_responses_affiliation,
    )


@app.cell
def _(conn, pl):
    groups = pl.read_database(
        query="""
                     SELECT "GROUP"
                     FROM voter_info
                     """,
        connection=conn,
    ).drop_nulls()
    print(len(groups.unique()))
    for group in groups.unique().iter_rows():
        group = group[0]
        print(f'group: {group} has {len(groups.filter(pl.col("GROUP") == group))} members')
    return group, groups


@app.cell
def _(conn, pl):
    voter_info_data_pre = pl.read_database(
        query = """SELECT voter_info.ID, response_PK.score, Q2A, Q2C, Q2E, Q2H, Q2I, Q3A, Q3B, Q3C, Q3D, Q3E, Q3H, Q4A, Q4B, Q4C,
         Q4F, Q4G, Q4H, Q4I, Q5A, Q5B, Q5C, Q5D, Q5H, Q6A, Q6F, Q6G 
        FROM response_pre 
        INNER JOIN voter_info ON response_pre.ID = voter_info.ID
        INNER JOIN response_PK ON response_pre.ID = response_PK.ID 
        WHERE voter_info.CONDITION = 1""",
        connection=conn
    ).drop_nulls()


    voter_info_data_post = pl.read_database(
        query = """SELECT voter_info.ID, response_PK.score, Q2A, Q2C, Q2E, Q2H, Q2I, Q3A, Q3B, Q3C, Q3D, Q3E, Q3H, Q4A, Q4B, Q4C,
         Q4F, Q4G, Q4H, Q4I, Q5A, Q5B, Q5C, Q5D, Q5H, Q6A, Q6F, Q6G 
        FROM response_post
        INNER JOIN voter_info ON response_post.ID = voter_info.ID
        INNER JOIN response_PK ON response_post.ID = response_PK.ID 
        WHERE voter_info.CONDITION = 1""",
        connection=conn
    ).drop_nulls()

    pre_ids = set(voter_info_data_pre["ID"])
    post_ids = set(voter_info_data_post["ID"])

    print(len(pre_ids & post_ids))
    return post_ids, pre_ids, voter_info_data_post, voter_info_data_pre


@app.cell
def _(conn, pl):
    pk_pbs_data = pl.read_database(
        query = """SELECT voter_info."GROUP", response_PK.score, Q2A, Q2C, Q2E, Q2H, Q2I, Q3A, Q3B, Q3C, Q3D, Q3E, Q3H, Q4A, Q4B, Q4C,
     Q4F, Q4G, Q4H, Q4I, Q5A, Q5B, Q5C, Q5D, Q5H, Q6A, Q6F, Q6G 
    FROM response_pre 
    INNER JOIN voter_info ON response_pre.ID = voter_info.ID
    INNER JOIN response_PK ON response_pre.ID = response_PK.ID 
    WHERE voter_info.CONDITION = 1
    """,
        connection=conn
    ).drop_nulls()
    score = pk_pbs_data["score"]
    groups_filter = pk_pbs_data["GROUP"]
    pbs = pk_pbs_data.drop(["score", "GROUP"]).mean_horizontal()
    return groups_filter, pbs, pk_pbs_data, score


@app.cell
def _(groups_filter, pk_pbs_data):
    print(groups_filter.unique())
    count = 0
    for group_f in groups_filter.unique():
        group_f = group_f
        if group_f != " ":
            count += sum(pk_pbs_data["GROUP"] == str(group_f))

    print(count/40)
    return count, group_f


@app.cell
def _(np, pbs):
    np.std(pbs.to_numpy())
    return


@app.cell
def _(pbs, score):
    import scipy 

    scipy.stats.pearsonr(pbs, score)
    return (scipy,)


@app.cell
def _(pbs, plt, score, sns):
    import pandas as pd
    # Your data
    df = pd.DataFrame({
        "Knowledge Score": score,
        "pbs": pbs
    })
    df = df.loc[df["Knowledge Score"] > 0]

    # Bin PBS values into integer bins [0–1), [1–2), ..., [9–10)
    df["pbs_bin"] = pd.cut(df["pbs"], bins=range(2, 10), right=False)

    # Prepare for ridge-style KDE plot
    pal = sns.color_palette("viridis", len(df["pbs_bin"].unique()))
    g = sns.FacetGrid(df, row="pbs_bin", hue="pbs_bin", aspect=5, height=2.0, palette=pal)

    g.map(sns.histplot, "Knowledge Score",
          kde=True, clip_on=False,
          alpha=0.8, linewidth=1.5)


    # Reference line at y=0
    g.refline(y=0, linewidth=2, linestyle="-", color=None, clip_on=False)

    # Label function for axes
    def label(x, color, label):
        ax = plt.gca()
        ax.text(-0.05, 0.2, str(label).strip("[)").replace(",", " -"), fontweight="bold", color=color,
                ha="left", va="center", transform=ax.transAxes)

    # Add the labels on the left of each plot
    g.map(label, "Knowledge Score")

    # Set the subplots to overlap
    g.figure.subplots_adjust(hspace=.05)

    # Remove axes details that don't play well with overlap
    g.set_titles("")
    g.set(yticks=[], ylabel="")
    g.despine(bottom=True, left=True)
    plt.savefig("figures/knowledge_pbs_dist.png", dpi=600)
    plt.show()
    return df, g, label, pal, pd


@app.cell
def _(plt):
    voters_pref = [[1,2,5,4,3],
                   [2,3,4,5,1],
                   [2,5,4,3,1],
                  ]

    y_labels = ["1\\textsuperscript{st}", "2\\textsuperscript{nd}", "3\\textsuperscript{rd}", "4\\textsuperscript{th}", "5\\textsuperscript{th}"]

    axis = [1, 2, 3, 4, 5]  # assumed to be the axis over which preferences are single-peaked

    plt.figure(figsize=(8, 4))



    colors = ["#008B72", "#613F99", "#D9027D"]
    for i, ranking in enumerate(voters_pref):
        # Convert ranking to a dictionary: candidate -> rank
        plt.plot(axis, voters_pref[i], "--o", label=f'Voter {i + 1}', color=colors[i], alpha=0.8)

    plt.xticks(axis, labels= ["$a$", "$b$", "$c$", "$d$", "$e$"])
    plt.yticks(ticks=[5, 4, 3, 2, 1], labels=y_labels)
    plt.legend(loc="upper right")
    plt.grid(True, linestyle=':')
    plt.tight_layout()
    plt.savefig("figures/single_peak_vis.png", dpi=600)
    plt.show()
    return axis, colors, i, ranking, voters_pref, y_labels


if __name__ == "__main__":
    app.run()
