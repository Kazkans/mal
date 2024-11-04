let repl_env = [Hashtbl.create 10]

let add = function
    | [Reader.Int a; Reader.Int b] -> Reader.Int (a+b)
    | _ -> raise (Invalid_argument "Cannot invoke non-function")

let mul = function
    | [Reader.Int a; Reader.Int b] -> Reader.Int (a*b)
    | _ -> raise (Invalid_argument "Cannot invoke non-function")

let operation op = function
    | [Reader.Int a; Reader.Int b] -> Reader.Int (op a b)
    | _ -> raise (Invalid_argument "Cannot invoke non-function")

let () = Env.set repl_env "+" (Reader.Fn (operation (+)))
let () = Env.set repl_env "*" (Reader.Fn (operation ( * )))
let () = Env.set repl_env "-" (Reader.Fn (operation (-)))
let () = Env.set repl_env "/" (Reader.Fn (operation (/)))
(*
let () = Hashtbl.add repl_env "*" (Reader.Fn (operation ( * )))
let () = Hashtbl.add repl_env "-" (Reader.Fn (operation ( - )))
let () = Hashtbl.add repl_env "/" (Reader.Fn (operation ( / )))*)
;;

let read x = Reader.read_str x
let rec eval env ast = match ast with
    | Reader.Sym s -> Env.get !env s
    | Reader.List l when List.length l <> 0 ->
            (match l with
            | Reader.Sym "def!" :: Reader.Sym s :: tail :: [] ->
                let value = (eval env tail) in
                Env.set !env s value; value
            | Reader.Sym "let*" :: Reader.List bindings :: body ->
                let env' = ref ((Hashtbl.create 10) :: !env) in
                let body = (List.nth l 2) in
                let rec bind_pairs = (function
                    | Reader.Sym s :: expr :: tail ->
                        Env.set !env s (eval env' expr);
                        bind_pairs tail
                    | [] -> ()
                    | _ -> raise (Invalid_argument "bad let*")) in
                bind_pairs bindings;
                eval env' body
            | _ ->
                let evaluated = List.map (eval env) l in
                (match (List.hd evaluated) with
                | Reader.Fn f -> f (List.tl evaluated)
                | _ -> raise (Invalid_argument "Cannot invoke non-function")))
    | Reader.Vector l -> Reader.Vector (List.map (eval env) l)
    | (Reader.Map tbl) as map -> Hashtbl.filter_map_inplace (fun k v -> Some (eval env v)) tbl; map
    | x -> x
let print x = Printer.pr_str x

let repl_env = ref repl_env
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
            | Invalid_argument s ->
                print_endline s;
                main ()
            | Not_found ->
                print_endline "";
                main ()

let () = main ()
