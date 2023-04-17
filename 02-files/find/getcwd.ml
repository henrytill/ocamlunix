let main () = print_endline (Exercise2.getcwd ())
let () = Unix.handle_unix_error main ()
