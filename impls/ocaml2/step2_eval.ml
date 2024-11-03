let repl_env = Hashtbl.create 10

let add = function
    | [Reader.Int a; Reader.Int b] -> Reader.Int (a+b)
    | _ -> raise (Invalid_argument "Cannot invoke non-function")

let mul = function
    | [Reader.Int a; Reader.Int b] -> Reader.Int (a*b)
    | _ -> raise (Invalid_argument "Cannot invoke non-function")

let operation op = function
    | [Reader.Int a; Reader.Int b] -> Reader.Int (op a b)
    | _ -> raise (Invalid_argument "Cannot invoke non-function")

let () = Hashtbl.add repl_env "+" (Reader.Fn (operation (+)))
let () = Hashtbl.add repl_env "*" (Reader.Fn (operation ( * )))
let () = Hashtbl.add repl_env "-" (Reader.Fn (operation ( - )))
let () = Hashtbl.add repl_env "/" (Reader.Fn (operation ( / )))
;;

let read x = Reader.read_str x
let rec eval repl ast = match ast with
    | Reader.Sym s -> Hashtbl.find repl_env s
    | Reader.List l when List.length l <> 0 ->
            let evaluated = List.map (eval repl) l in
            (match (List.hd evaluated) with
            | Reader.Fn f -> f (List.tl evaluated)
            | _ -> raise (Invalid_argument "Cannot invoke non-function"))
    | Reader.Vector l -> Reader.Vector (List.map (eval repl) l)
    | (Reader.Map tbl) as map -> Hashtbl.filter_map_inplace (fun k v -> Some (eval repl v)) tbl; map
    | x -> x
let print x = Printer.pr_str x
let rep x = read x |> eval repl_env |> print

let rec main () =
        Printf.printf "user> ";
        try 
            let line = read_line () in
            line |> read |> print |> print_endline;
            line |> rep |> print_endline;
            main ()
        with 
            | End_of_file ->
                print_endline "EOF";
                main ()
            | Not_found ->
                print_endline "";
                main ()

let () = main ()
