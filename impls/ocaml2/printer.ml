let remove_prefix s =
    let c = String.get s 0 in
    if Char.code c = 255 then
        String.sub s 1 ((String.length s) - 1)
    else
        s

let rec f k d acc =
    (remove_prefix k) ^ " " ^ (pr_str d) ^ " " ^ acc

and pr_str = function
    | Reader.Int i -> string_of_int i
    | Reader.Sym s -> s
    | Reader.Keyword s -> String.sub s 1 (String.length s - 1)
    | Reader.List l -> "(" ^ (String.concat " " (List.map pr_str l)) ^ ")"
    | Reader.Vector l -> "[" ^ (String.concat " " (List.map pr_str l)) ^ "]"
    | Reader.Map tbl -> "{" ^ (String.trim (Hashtbl.fold f tbl "")) ^ "}"

(*| Map of (string, mal_type) Hashtbl.t*)
