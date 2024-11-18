let repl_env = [Hashtbl.create 10]


let () = Core.init_env repl_env
(*
let () = Hashtbl.add repl_env "*" (Reader.Fn (operation ( * )))
let () = Hashtbl.add repl_env "-" (Reader.Fn (operation ( - )))
let () = Hashtbl.add repl_env "/" (Reader.Fn (operation ( / )))*)
;;

let read x = Reader.read_str x

let rec eval env (ast : Reader.mal_type) : Reader.mal_type = match ast with
    | Reader.Sym s -> Env.get !env s
    | Reader.List l when List.length l <> 0 ->
            (match l with
            | Reader.Sym "def!" :: Reader.Sym s :: tail :: [] ->
                let value = (eval env tail) in
                Env.set !env s value; value
            | Reader.Sym "let*" :: Reader.List bindings :: body :: [] ->
                eval_let env bindings body
            | Reader.Sym "let*" :: Reader.Vector bindings :: body :: [] ->
                eval_let env bindings body
            | Reader.Sym "do" :: body ->
                    (List.fold_left (fun _ x -> eval env x) Reader.Nil body)
            | Reader.Sym "if" :: cond :: if_true :: rest ->
                    let cond = eval env cond in
                    let if_false = (match rest with
                        | [] -> Reader.Nil
                        | [cond] -> cond
                        | _ -> raise (Invalid_argument "More than 2 to if")) in
                    (match cond with
                        | Reader.Bool false | Reader.Nil -> eval env if_false
                        | _ -> eval env if_true)
            | Reader.Sym "fn*" :: Reader.List arg_names :: expr :: [] ->
                create_fn arg_names expr env
            | Reader.Sym "fn*" :: Reader.Vector arg_names :: expr :: [] ->
                create_fn arg_names expr env
            | _ ->
                let evaluated = List.map (eval env) l in
                (match (List.hd evaluated) with
                | Reader.Fn f -> f (List.tl evaluated)
                | _ -> raise (Invalid_argument "Cannot invoke non-function")))
    | Reader.Vector l -> Reader.Vector (List.map (eval env) l)
    | (Reader.Map tbl) as map -> Hashtbl.filter_map_inplace (fun k v -> Some (eval env v)) tbl; map
    | x -> x

and eval_let env bindings body =
    let env' = ref ((Hashtbl.create 10) :: !env) in
    let rec bind_pairs = (function
        | Reader.Sym s :: expr :: tail ->
                Env.set !env' s (eval env' expr);
                        bind_pairs tail
        | [] -> ()
        | _ -> raise (Invalid_argument "bad let*")) in
    bind_pairs bindings;
    eval env' body

and create_fn arg_names expr env =
    Reader.Fn
     (function args ->
         let env' = ref ((Hashtbl.create 10) :: !env) in
         let rec bind_args = (function
             | Reader.Sym name :: names, arg :: args ->
                     Env.set !env' name arg;
                     bind_args (names, args);
             | [], [] -> ()
             | _ -> raise (Invalid_argument "Bad number of parameters in fn* call")) in
         bind_args (arg_names, args);
         eval env' expr)

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
