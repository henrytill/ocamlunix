let try_finalize f x finally y =
  let res =
    try f x with
    | exn ->
      finally y;
      raise exn
  in
  finally y;
  res
;;

let iter_dir f dirname =
  let d = Unix.opendir dirname in
  try
    while true do
      f (Unix.readdir d)
    done
  with
  | End_of_file -> Unix.closedir d
;;

let rec restart_on_EINTR f x =
  try f x with
  | Unix.Unix_error (EINTR, _, _) -> restart_on_EINTR f x
;;

let free_children _ =
  try
    while fst Unix.(waitpid [ WNOHANG ] (-1)) > 0 do
      ()
    done
  with
  | Unix.Unix_error (ECHILD, _, _) -> ()
;;

let retransmit fdin fdout =
  let buffer_size = 4096 in
  let buffer = Bytes.create buffer_size in
  let rec copy () =
    match Unix.read fdin buffer 0 buffer_size with
    | 0 -> ()
    | n ->
      ignore (Unix.write fdout buffer 0 n);
      copy ()
  in
  copy ()
;;

let install_tcp_server_socket addr =
  let s = Unix.(socket PF_INET SOCK_STREAM 0) in
  try
    Unix.bind s addr;
    Unix.listen s 10;
    s
  with
  | z ->
    Unix.close s;
    raise z
;;

let tcp_server treat_connection addr =
  let open Sys in
  ignore (signal sigpipe Signal_ignore);
  let server_sock = install_tcp_server_socket addr in
  while true do
    let client = restart_on_EINTR Unix.accept server_sock in
    treat_connection server_sock client
  done
;;

let sequential_treatment _ service client = service client

let fork_treatment server service ((client_sock, _) as client) =
  let treat () =
    match Unix.fork () with
    | 0 ->
      Unix.close server;
      service client;
      exit 0
    | _ -> ()
  in
  try_finalize treat () Unix.close client_sock
;;

let double_fork_treatment server service ((client_descr, _) as client) =
  let treat () =
    match Unix.fork () with
    | 0 ->
      if Unix.fork () <> 0 then exit 0;
      Unix.close server;
      service client;
      exit 0
    | k -> ignore (restart_on_EINTR (Unix.waitpid []) k)
  in
  try_finalize treat () Unix.close client_descr
;;

let co_treatment _ service ((client_descr, _) as client) =
  try ignore (Thread.create service client) with
  | exn ->
    Unix.close client_descr;
    raise exn
;;

let run_with_lock l f x =
  Mutex.lock l;
  try_finalize f x Mutex.unlock l
;;

let tcp_farm_server n treat_connection addr =
  let server_sock = install_tcp_server_socket addr in
  let mutex = Mutex.create () in
  let rec serve () =
    let client = run_with_lock mutex (restart_on_EINTR Unix.accept) server_sock in
    treat_connection server_sock client;
    serve ()
  in
  for _ = 1 to n - 1 do
    ignore (Thread.create serve ())
  done;
  serve ()
;;
