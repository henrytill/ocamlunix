open Unix

let read_passwd message =
  match
    try
      let default = tcgetattr stdin in
      let silent =
        { default with
          c_echo = false;
          c_echoe = false;
          c_echok = false;
          c_echonl = false;
        } in
      Some (default, silent)
    with _ ->
      None
  with
  | None -> input_line Stdlib.stdin
  | Some (default, silent) ->
    print_string message;
    flush Stdlib.stdout;
    tcsetattr stdin TCSANOW silent;
    try
      let s = input_line Stdlib.stdin in
      tcsetattr stdin TCSANOW default; s
    with exn ->
      tcsetattr stdin TCSANOW default; raise exn

let main () =
  let passwd = read_passwd "Enter password: " in
  print_endline ("\nYou entered: " ^ passwd)

;;

handle_unix_error main ()
