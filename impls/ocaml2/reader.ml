type reader = Reader of string list
type sym = Sym of string

type mal_type =
    | Bool of bool
    | Nil
    | Int of int
    | Sym of string
    | String of string
    | Keyword of string
    | List of mal_type list
    | Vector of mal_type list
    | Map of (string, mal_type) Hashtbl.t
    | Fn of ((mal_type list) -> mal_type)

let next = function
    | Reader (x::xs) -> Some (x, Reader xs)
    | Reader [] -> None

let peek = function
    | Reader (x::_) -> Some x
    | Reader [] -> None

(* tokenize string -> string list*)

let (<<) f g x = f(g(x))

let starts_with s x =
    (String.get s 0) = x

let ends_with s x =
    (String.get s (String.length s - 1)) = x

let tokenize s =
    let pattern = Re.Pcre.regexp {|[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)|} in
    let gs = Re.all pattern s in
    List.map (fun g -> Re.Group.get g 1) gs
    |> List.filter (not << (String.starts_with ~prefix:";"))

let read_atom t =
    match t.[0] with
        | '-' when (String.length t > 1) -> Int (int_of_string t)
        | '0'..'9' -> Int (int_of_string t)
        | ':' -> Keyword (String.make 1 (Char.chr 0xFF) ^ t)
        | _ -> (match t with
            | "true" -> Bool true
            | "false" -> Bool false
            | "nil" -> Nil    
            | s when starts_with s '"' && ends_with s '"' -> String (String.sub s 1 (String.length s - 2))
            | _ -> Sym t)

let get_string = function
    | Sym s -> s 
    | Keyword s -> s
    | _ -> raise End_of_file

let create_map l =
    let n = List.length l in
    if n mod 2 = 1 then
        raise End_of_file
    else
        let tbl = Hashtbl.create n in
        let rec aux = (function
            | k :: v :: tail ->
                Hashtbl.add tbl (get_string k) v;
                aux tail
            | _ -> ()) in
        aux l;
        tbl
;;

let rec read_str s =
    let r = Reader (tokenize s) in
    let x, _ = read_form r in
    x

and read_form r = 
    match (next r) with
    | Some (x, r') ->
        (match x with
        | "(" -> let x, r' = read_list r' ")" in
                (List x, r')
        | "[" -> let x, r' = read_list r' "]" in
                (Vector x, r')
        | "{" -> let x, r' = read_list r' "}" in
                (Map (create_map x), r')
        | t -> (read_atom t, r'))
    | None -> raise End_of_file

and read_list r cl_char =
    match (next r) with
    | Some (x, r') ->
        (match x with
        | x when x = cl_char -> ([], r')
        | _ ->
                let x, r = read_form r in
                let y, r = read_list r cl_char in
                (x::y, r))
    | None -> raise End_of_file
