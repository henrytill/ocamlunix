open Unix
open Url

let get_regexp =
  { regexp = Str.regexp "^[Gg][Ee][Tt][ \t]+\\(.*[^ \t]\\)[ \t]*\r"
  ; fields = [ 1, None ]
  }
;;

let parse_request line =
  match regexp_match get_regexp line with
  | Some (url :: _) -> url
  | _ -> error line "Ill-formed request"
;;

let proxy_service (client_sock, _) =
  let service () =
    try
      let in_chan = in_channel_of_descr client_sock in
      let line = input_line in_chan in
      let url = parse_request line in
      get_url None url client_sock
    with
    | End_of_file -> error "Ill-formed request" "End_of_file encountered"
  in
  Misc.try_finalize (handle_error service) () close client_sock
;;

let proxy () =
  let http_port =
    if Array.length Sys.argv > 1
    then (
      try int_of_string Sys.argv.(1) with
      | Failure _ -> error Sys.argv.(1) "Incorrect port")
    else (
      try (getservbyname "http" "tcp").s_port with
      | Not_found -> error "http" "Unknown service")
  in
  let treat_connection s = Misc.double_fork_treatment s proxy_service in
  let addr = ADDR_INET (inet_addr_any, http_port) in
  Misc.tcp_server treat_connection addr
;;

let () = handle_unix_error (handle_error proxy) ()
