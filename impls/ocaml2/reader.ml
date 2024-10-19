type reader = Reader of string list
type sym = Sym of string

type mal_type =
    | Int of int
    | Sym of string
    | List of mal_type list

let next = function
    | Reader (x::xs) -> Some (x, Reader xs)
    | Reader [] -> None

let peek = function
    | Reader (x::_) -> Some x
    | Reader [] -> None

(* tokenize string -> string list*)
let tokenize s =
    let pattern = Re.Pcre.regexp {|[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)|} in
    let gs = Re.all pattern s in
    List.map (fun g -> Re.Group.get g 1) gs

let read_atom t =
    match t.[0] with
        | '0'..'9' -> Int (int_of_string t)
        | _ -> Sym t

let rec read_str s =
    let r = Reader (tokenize s) in
    let x, _ = read_form r in
    x

and read_form r = 
    match (next r) with
    | Some (x, r') ->
        (match x with
        | "(" -> let x, r' = read_list r' in
                (List x, r')
        | t -> (read_atom t, r'))
    | None -> raise End_of_file

and read_list r =
    match (next r) with
    | Some (x, r') ->
        (match x with
        | ")" -> ([], r')
        | _ ->
                let x, r = read_form r in
                let y, r = read_list r in
                (x::y, r))
    | None -> raise End_of_file
