let main () =
  print_endline (Exercise2.getcwd ());;

Unix.handle_unix_error main ()
