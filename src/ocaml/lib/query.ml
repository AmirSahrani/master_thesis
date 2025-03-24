(* open Sqlite3

let open_db filename =
  let db = Sqlite3.db_open filename in
  db

let get_voters_opinions db id's condition =
  let query = "SELECT * FROM voters" in
  let callback (_ : string -> string option -> unit) row =
    match row with
    | `Row values ->
        (* Process each row; values is a list of columns in the row *)
        Printf.printf "Row: %s\n" (String.concat ", " values)
    | `Error msg -> Printf.printf "Error: %s\n" msg
    | `End -> Printf.printf "End of result set.\n"
  in
  (* Execute the query *)
  Sqlite3.exec db "voters" ~cb:callback *)
