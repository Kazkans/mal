let set env s v =
    Hashtbl.add (List.hd env) s v

let rec get env s = match env with
    | [] -> raise (Invalid_argument (Printf.sprintf "%s not found" s))
    | _ ->
        let v = Hashtbl.find_opt (List.hd env) s in
        (match v with
        | Some v -> v
        | None -> get (List.tl env) s)
