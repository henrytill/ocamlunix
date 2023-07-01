let client () =
  if Array.length Sys.argv < 3 then (
    prerr_endline "Usage: client <host> <port>";
    exit 2);
  let server_name = Sys.argv.(1) and port_number = int_of_string Sys.argv.(2) in
  let server_addr =
    try Unix.(gethostbyname server_name).h_addr_list.(0)
    with Not_found ->
      prerr_endline (server_name ^ ": Host not found");
      exit 2
  in
  let sock = Unix.(socket PF_INET SOCK_STREAM 0) in
  Unix.connect sock (ADDR_INET (server_addr, port_number));
  match Unix.fork () with
  | 0 ->
      Misc.retransmit Unix.stdin sock;
      Unix.shutdown sock SHUTDOWN_SEND;
      exit 0
  | _ ->
      Misc.retransmit sock Unix.stdout;
      Unix.(close stdout);
      Unix.wait ()

let () = ignore (Unix.handle_unix_error client ())
