let server () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "Usage: client <port> <command> [arg1 ... argn]";
    exit 2);
  let port = int_of_string Sys.argv.(1) in
  let args = Array.sub Sys.argv 2 (Array.length Sys.argv - 2) in
  let host = Unix.(gethostbyname (gethostname ())).h_addr_list.(0) in
  let addr = Unix.ADDR_INET (host, port) in
  let treat sock ((_client_sock, client_addr) as client) =
    (* log information *)
    Unix.(
      match client_addr with
      | ADDR_INET (caller, _) -> prerr_endline ("Connection from " ^ string_of_inet_addr caller)
      | ADDR_UNIX _ -> prerr_endline "Connection from the Unix domain (???)");
    (* connection treatment *)
    let service (s, _) =
      Unix.(dup2 s stdin);
      Unix.(dup2 s stdout);
      Unix.(dup2 s stderr);
      Unix.close s;
      Unix.execvp args.(0) args
    in
    Misc.double_fork_treatment sock service client
  in
  Misc.tcp_server treat addr

let () = Unix.handle_unix_error server ()
