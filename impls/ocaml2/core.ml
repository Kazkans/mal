let operation op = function
    | [Reader.Int a; Reader.Int b] -> Reader.Int (op a b)
    | _ -> raise (Invalid_argument "Cannot invoke non-function")

let prn = function
    | [m] -> Printf.printf "%s\n" (Printer.pr_str m); Reader.Nil
    | _ -> raise (Invalid_argument "Cannot invoke non-function")

let list args = Reader.List args

let is_list = function
    | [Reader.List _] -> Reader.Bool true
    | _ -> Reader.Bool false

let is_empty = function
    | [Reader.List []] -> Reader.Bool true
    | _ -> Reader.Bool false

let is_empty = function
    | [Reader.List []] -> Reader.Bool true
    | _ -> Reader.Bool false

let count = function
    | [Reader.List l] -> Reader.Int (List.length l)
    | [Reader.Nil] -> Reader.Int 0
    | _ -> raise (Invalid_argument "Cannot invoke non-function")

let eq = function
    | [Reader.Int a; Reader.Int b] -> Reader.Bool (a = b)
    | [Reader.List a; Reader.List b] -> Reader.Bool (a = b)
    | [Reader.Sym a; Reader.Sym b] -> Reader.Bool (a = b)
    | [Reader.Bool a; Reader.Bool b] -> Reader.Bool (a = b)
    | [Reader.String a; Reader.String b] -> Reader.Bool (a = b)
    | [Reader.Nil; Reader.Nil] -> Reader.Bool true
    | _ -> Reader.Bool false

let cmp op = function
    | [Reader.Int a; Reader.Int b] -> Reader.Bool (op a b)
    | _ -> raise (Invalid_argument "Cannot invoke non-function")

let init_env env =
    Env.set env "+" (Reader.Fn (operation (+)));
    Env.set env "*" (Reader.Fn (operation ( * )));
    Env.set env "-" (Reader.Fn (operation (-)));
    Env.set env "/" (Reader.Fn (operation (/)));
    Env.set env "prn" (Reader.Fn prn);
    Env.set env "list" (Reader.Fn list);
    Env.set env "list?" (Reader.Fn is_list);
    Env.set env "empty?" (Reader.Fn is_empty);
    Env.set env "count" (Reader.Fn count);
    Env.set env "=" (Reader.Fn eq);
    Env.set env "<" (Reader.Fn (cmp (<)));
    Env.set env "<=" (Reader.Fn (cmp (<=)));
    Env.set env ">" (Reader.Fn (cmp (>)));

    Env.set env ">=" (Reader.Fn (cmp (>=)));
