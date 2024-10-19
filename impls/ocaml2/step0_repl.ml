let read x = x
let eval x = x
let print x = x
let rep x = read x |> eval |> print

let rec main () =
        Printf.printf "user> ";
        try 
                read_line () |> rep |> print_endline;
                main ()
        with End_of_file -> ()

let () = main ()
