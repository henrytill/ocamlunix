let main () =
  let num = ref 10 in
  let file = ref "" in
  let usage_string = ("Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]...") in
  let opt_list = [
    "-n", Arg.Int ((:=) num), "output the last NUM lines, instead of the last 10;";
  ] in
  Arg.parse opt_list (fun f -> file := f) usage_string;
  let xs = Tail.tail !file !num in
  Array.iter (fun line -> print_string line) xs
;;

Unix.handle_unix_error main ()
