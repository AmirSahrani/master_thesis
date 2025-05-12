open Deliberation_model.Experiments
open Deliberation_model.Utils

let main () =
    let experiment_type =
        Sys.argv.(1) |> String.lowercase_ascii |> experiments
    in
        print_string "Running the following experiment: ";
        print_endline Sys.argv.(1);
        match experiment_type with
        | `Rad -> rad_roy_bias_experiment ()
        | `DeGroot -> deGroot_experiment ()
        | `Sensitivity -> sensitivity_analysis ()
        | `Testing -> test ()

let () =
    Printexc.record_backtrace true;
    try
      main ();
      let _ = Sys.command "notify-send 'Simulations finished'" in
          ()
    with
    | Not_found ->
        Printexc.print_backtrace stderr;
        prerr_endline "Caught Not_found!";
        exit 1
    | e ->
        Printf.eprintf "Uncaught exception: %s\n%s\n" (Printexc.to_string e)
          (Printexc.get_backtrace ());
        exit 1
