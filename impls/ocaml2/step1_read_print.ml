let read x = Reader.read_str x
let eval x = x
let print x = Printer.pr_str x
let rep x = read x |> eval |> print

let rec main () =
        Printf.printf "user> ";
        try 
                read_line () |> rep |> print_endline;
                main ()
        with End_of_file ->
            print_endline "EOF";
            main ()

let () = main ()
