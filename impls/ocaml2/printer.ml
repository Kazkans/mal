let rec pr_str = function
    | Reader.Int i -> string_of_int i
    | Reader.Sym s -> s
    | Reader.List l -> "(" ^ (String.concat " " (List.map pr_str l)) ^ ")"
