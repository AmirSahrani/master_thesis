open Utils

let is_single_peaked profile =
  (* 
     Algorithm to determine is single peaked, runs in O(nm) adapted from the presentation by Elkind, who formulated it based on the algortihm by Doignon and Falmagne (1994), adding ideas from Escoffier et al. (2008)
     *)
  let rec stage_1 left right a' =
    if a' = [] || right <> [||] then (left, right, a')
    else
      let b = bottom profile a' in
      if List.length b > 2 then ([||], [||], a')
      else
        let a'' = List.filter (fun alt -> not (List.mem alt b)) a' in
        match b with
        | x :: y :: _ ->
            stage_1
              (Array.concat [ left; [| x |] ])
              (Array.concat [ [| y |]; right ])
              a''
        | x :: _ -> stage_1 (Array.concat [ left; [| x |] ]) right a''
        | _ -> failwith "Not bottom alternatives found"
  in
  let left, right, a' = stage_1 [||] [||] (List.hd profile) in
  if left = [||] && right = [||] then None
  else
    let rec part_2 left right a' =
      if List.length a' < 2 then
        Some (Array.to_list left @ a' @ Array.to_list right)
      else
        let l = left.(Array.length left - 1) in
        let r = right.(0) in
        let b = bottom profile a' in
        if List.length b > 2 then None
        else
          let l' =
            List.filter
              (fun m ->
                List.exists
                  (fun p -> between l m r p)
                  (List.filter (fun p -> List.hd p <> m) profile))
              b
          in
          let r' =
            List.filter
              (fun m ->
                List.exists
                  (fun p -> between r m l p)
                  (List.filter (fun p -> List.hd p <> m) profile))
              b
          in
          if List.length l' > 1 || List.length r' > 1 then None
          else if intersection l' r' <> [] then None
          else
            let rest =
              List.filter (fun elem -> not (List.mem elem (union l' r'))) b
            in
            (* assert (List.length rest <= 1); *)
            let l' = if List.length l' = 0 then rest @ l' else l' in
            let r' = if List.length r' = 0 then rest @ r' else r' in
            part_2
              (Array.append left (Array.of_list l'))
              (Array.append (Array.of_list r') right)
              (List.filter (fun elem -> not (List.mem elem b)) a')
    in
    part_2 left right a'

(* let proximity_to_single_peakedness profile = () *)
