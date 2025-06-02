open Sqlite3

let q6 = "Q6J, Q6I, Q6H, Q6G, Q6F, Q6E, Q6D, Q6C, Q6B, Q6A, Q5J"
let q5 = "Q5I, Q5H, Q5G, Q5F, Q5E, Q5D, Q5C, Q5B, Q5A"
let q4 = "Q4J, Q4I, Q4H, Q4G, Q4F, Q4E, Q4D, Q4C, Q4B, Q4A"
let q3 = "Q3H, Q3G, Q3F, Q3E, Q3D, Q3C, Q3B, Q3A"
let q2 = "Q2I, Q2H, Q2G, Q2F, Q2E, Q2D, Q2C, Q2B, Q2A"
let q1 = "Q1"

let knowledge_questions =
    "response_pk.PK1, response_pk.PK2, response_pk.PK3, response_pk.PK4, \
     response_pk.PK5, response_pk.PK6, response_pk.PK7"

let join_by_comma = String.concat ","
let pk_score = "voter_info.ID, response_pk.score"

(* let questions_with_pk = join_by_comma [ q6; q5; q4; q3; q2; q1; pk_score ] *)
let questions_without_pk =
    join_by_comma [ "voter_info.ID"; q6; q5; q4; q3; q2; q1 ]

let polarizing_questions =
    "voter_info.ID, Q2A, Q2C, Q2E, Q2H, Q2I, Q3A, Q3B, Q3C, Q3D, Q3E, Q3H, \
     Q4A, Q4B, Q4C, Q4F, Q4G, Q4H, Q4I, Q5A, Q5B, Q5C, Q5D, Q5H, Q6A, Q6F, Q6G"

let inner_join table1 table2 column =
    Printf.sprintf "INNER JOIN %s ON %s.%s = %s.%s" table1 table2 column table1
      column

let right_join table1 table2 column =
    Printf.sprintf "RIGHT JOIN %s ON %s.%s = %s.%s" table1 table2 column table1
      column

let condition_sub table column comp value =
    Printf.sprintf "%s.%s %s %s" table column comp value

let limit num = Printf.sprintf "LIMIT %d" num

let join_where conditions =
    if List.length conditions = 0 then ""
    else Printf.sprintf "WHERE %s " (String.concat " AND " conditions)

let comp = function
    | `Equal -> "="
    | `IsNot -> "is not"
    | `GE -> ">="
    | `LE -> "<="
    | `In -> "in"

let voter_info = function
    | `ID -> "ID"
    | `Group -> "\"GROUP\""
    | `Condition -> "CONDITION"
    | `Education -> "EDUC4"

let tables = function
    | `Response_pre -> "response_pre"
    | `Response_post -> "response_post"
    | `Response_pk -> "response_PK"
    | `Voter_info -> "voter_info"
    | `Political_knowledge -> "political_knowledge"
    | `Questionnaire -> "questionnaire"

let query_of columns table joins condition limit =
    let join = String.concat "\n" joins in
        Printf.sprintf "SELECT %s \nFROM %s \n%s \n%s %s;" columns table join
          condition limit

let open_db filename =
    let db = Sqlite3.db_open filename in
        db

let close_db db = Sqlite3.db_close db

let extract_query_data db query =
    let results = ref [] in
    let callback row =
        (* Convert row to a list of strings *)
        let row_data = Array.to_list row in
            results := row_data :: !results
    in
        match exec_no_headers db query ~cb:callback with
        | Rc.OK -> List.rev !results (* Reverse to maintain original order *)
        | error ->
            Printf.printf "Error executing query: %s\n"
              (Sqlite3.Rc.to_string error);
            []

let get_query columns table joins condition limit =
    query_of columns table joins condition limit

let get_voters_opinions db query =
    let response = extract_query_data db query in
        (* Printf.printf "Number of rows of data: %d\n" (List.length response); *)
        response
        |> List.filter (fun row -> List.for_all Option.is_some row)
           (* Remove rows containing None *)
        |> List.map (fun row ->
               row
               |> List.map (function
                    | Some x ->
                        if String.length (String.trim x) = 0 then -1.
                        else float_of_string x
                    | None -> failwith "Unexpected None")
               |> Array.of_list)
        |> Array.of_list
