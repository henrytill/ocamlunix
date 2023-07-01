let compose () =
  let n = Array.length Sys.argv - 1 in
  for i = 1 to n - 1 do
    let pipe_out, pipe_in = Unix.pipe () in
    match Unix.fork () with
    | 0 ->
        Unix.(dup2 pipe_in stdout);
        Unix.close pipe_in;
        Unix.close pipe_out;
        Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; Sys.argv.(i) |]
    | _ ->
        Unix.(dup2 pipe_out stdin);
        Unix.close pipe_in;
        Unix.close pipe_out
  done;
  match Unix.fork () with
  | 0 -> Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; Sys.argv.(n) |]
  | _ ->
      let rec wait_for_children retcode =
        try
          match Unix.wait () with
          | _, WEXITED n -> wait_for_children (retcode lor n)
          | _, _ -> wait_for_children 127
        with Unix.Unix_error (ECHILD, _, _) -> retcode
      in
      exit (wait_for_children 0)

let () = Unix.handle_unix_error compose ()
