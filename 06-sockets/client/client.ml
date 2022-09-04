open Unix

let client () =
  if Array.length Sys.argv < 3
  then (
    prerr_endline "Usage: client <host> <port>";
    exit 2);
  let server_name = Sys.argv.(1)
  and port_number = int_of_string Sys.argv.(2) in
  let server_addr =
    try (gethostbyname server_name).h_addr_list.(0) with
    | Not_found ->
      prerr_endline (server_name ^ ": Host not found");
      exit 2
  in
  let sock = socket PF_INET SOCK_STREAM 0 in
  connect sock (ADDR_INET (server_addr, port_number));
  match fork () with
  | 0 ->
    Misc.retransmit stdin sock;
    shutdown sock SHUTDOWN_SEND;
    exit 0
  | _ ->
    Misc.retransmit sock stdout;
    close stdout;
    wait ()
;;

handle_unix_error client ()
