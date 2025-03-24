open Sqlite3
open Utils

let open_db filename =
  let db = Sqlite3.db_open filename in
  db

let get_voters_opinions db id's condition =
  let _ = [ "ID"; "Q9_1"; "Q9_2" ] in
  let query =
    "JOIN voter_info response_pre on ID SELECT * from response_pre WHERE ID in "
    ^ string_of_list id's string_of_int
    ^ "AND voter.condition = " ^ string_of_int condition
  in
  match exec db query with
  | Rc.OK -> Printf.printf "Query executed successfully!\n"
  | error -> Printf.printf "Error: %s\n" (Rc.to_string error)
