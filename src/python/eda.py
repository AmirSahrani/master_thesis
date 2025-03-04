import marimo

__generated_with = "0.11.14"
app = marimo.App(width="full")


@app.cell
def _():
    import marimo as mo
    import polars as pl
    import matplotlib.pyplot as plt
    from enum import Enum
    import textwrap
    return Enum, mo, pl, plt, textwrap


@app.cell
def _(Enum):
    class Response(Enum):
        VALID = "0"
        NO_OPINION = "77"
        REFUSED = "98"
        INVALID= "-1"
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
    questionnaire_questions = pl.read_csv("data/questionnaire.csv", separator=';', has_header=False, new_columns=["code", "question"])

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
        index=['ID', 'WEIGHT_CONTROL', 'WEIGHT_DELEGATE', 'POST', 'GROUP', 'CONDITION', 'GENDER', 'AGE', 'AGE4', 'RACETHNICITY', 'EDUC4'],  # Identify individuals
        on=[col for col in data.columns if col.startswith("Q") or col.startswith("T2Q") or col.startswith("PK") or col.startswith("T2PK")],  # Select all question columns
        variable_name="Question",
        value_name="Response"
    )
    df_long = df_long.with_columns(
        pl.when(df_long["Question"].str.starts_with("T2"))
        .then(pl.lit("Post"))
        .otherwise(pl.lit("Pre"))
        .alias("Timepoint")
    )

    df_long = df_long.with_columns(df_long["Question"].str.replace("^T2", "").alias("Question"))
    df_long = df_long.with_columns(
        df_long["Response"].map_elements(from_value, return_dtype=int).alias("ResponseType")
    )

    sql_context.register("data_long", df_long)
    print(df_long)
    return (df_long,)


@app.cell
def _(mo):
    mo.md(
        r"""
        We now have a sql database containing all data points, which we can now query with asking for the pre- and post-deliberation questionnaire results.

        Now we focus on cleaning the data.
        """
    )
    return


@app.cell
def _(pl, plt, question_dict, sql_context, textwrap):
    def gen_response_query(cols, q, response_type, time):
        return f"""
            SELECT {" ".join(cols)}
            FROM data_long 
            WHERE Question = '{q}' AND ResponseType = {response_type} AND Timepoint = '{time}'
        """


    questions_query = """
        SELECT DISTINCT Question from data_long
    """

    questions = sql_context.execute(questions_query).collect()

    num_questions = len(questions)
    num_figures = (num_questions + 9) // 10  # Ceiling division to get number of figures

    for fig_num in range(num_figures):
        # Create a new figure with increased size for better readability
        plt.figure(figsize=(20, 10))
    
        # Calculate the range of questions for this figure
        start_idx = fig_num * 10
        end_idx = min((fig_num + 1) * 10, num_questions)
    
        # Create 2x5 subplot grid
        for subplot_idx, question in enumerate(questions["Question"][start_idx:end_idx], 1):
            plt.subplot(2, 5, subplot_idx)
        
            q = gen_response_query(["Response"], question, 0, "Pre")
            q_data = sql_context.execute(q).collect().cast(pl.Int64)

            full_title = question_dict.get(question, question)

            # Create histogram
            plt.hist(q_data, bins='auto')
            # Wrap the title (adjust width as needed)
            wrapped_title = textwrap.fill(full_title, width=40)
        
            # Add wrapped title
            plt.title(wrapped_title, fontsize=9, ha='center')
            plt.tight_layout()
    

    # Show all figures
    plt.show()
    return (
        end_idx,
        fig_num,
        full_title,
        gen_response_query,
        num_figures,
        num_questions,
        q,
        q_data,
        question,
        questions,
        questions_query,
        start_idx,
        subplot_idx,
        wrapped_title,
    )


@app.cell
def _():
    return


if __name__ == "__main__":
    app.run()
