open Sqlite3

let questions =
  "Q6J, Q6I, Q6H, Q6G, Q6F, Q6E, Q6D, Q6C, Q6B, Q6A, Q5J, Q5I, Q5H, Q5G, Q5F, \
   Q5E, Q5D, Q5C, Q5B, Q5A, Q4J, Q4I, Q4H, Q4G, Q4F, Q4E, Q4D, Q4C, Q4B, Q4A, \
   Q3H, Q3G, Q3F, Q3E, Q3D, Q3C, Q3B, Q3A, Q2I, Q2H, Q2G, Q2F, Q2E, Q2D, Q2C, \
   Q2B, Q2A, Q1"

let knowledge_questions = "PK1, PK2, PK3, PK4, PK5, PK6, PK7"
let knowledge_score = "score"

let inner_join table1 table2 column =
  Printf.sprintf "INNER JOIN %s ON %s.%s = %s.%s" table1 table2 column table1
    column

let right_join table1 table2 column =
  Printf.sprintf "RIGHT JOIN %s ON %s.%s = %s.%s" table1 table2 column table1
    column

let condition_sub table column comp value =
  Printf.sprintf "%s.%s %s %s" table column comp value

let join_where conditions =
  if List.length conditions = 0 then ""
  else Printf.sprintf "WHERE %s " (String.concat " AND " conditions)

let comp = function
  | `Equal -> "="
  | `IsNot -> "is not"
  | `GE -> ">="
  | `LE -> "<="

let voter_info = function
  | `ID -> "ID"
  | `Group -> "GROUP"
  | `Condition -> "CONDITION"
  | `Education -> "EDUC4"

let tables = function
  | `Response_pre -> "response_pre"
  | `Response_post -> "response_post"
  | `Response_pk -> "response_PK"
  | `Voter_info -> "voter_info"
  | `Political_knowledge -> "political_knowledge"
  | `Questionnaire -> "questionnaire"

let query_of columns table join condition =
  Printf.sprintf "SELECT %s \nFROM %s \n%s \n%s;" columns table join condition

let open_db filename =
  let db = Sqlite3.db_open filename in
  db

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
      Printf.printf "Error executing query: %s\n" (Sqlite3.Rc.to_string error);
      []

let get_voters_opinions db columns table join condition =
  let query = query_of columns table join condition in
  print_endline query;
  let response = extract_query_data db query in
  Printf.printf "Number of rows of data: %d\n" (List.length response);
  response
  |> List.filter (fun row -> List.for_all Option.is_some row)
     (* Remove rows containing None *)
  |> List.map (fun row ->
         row
         |> List.map (function
              | Some x -> float_of_string x
              | None -> failwith "Unexpected None")
         |> Array.of_list)
  |> Array.of_list
