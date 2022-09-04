open Unix

let compose () =
  let n = Array.length Sys.argv - 1 in
  for i = 1 to n - 1 do
    let (pipe_out, pipe_in) = pipe () in
    match fork () with
    | 0 ->
        dup2 pipe_in stdout;
        close pipe_in;
        close pipe_out;
        execv "/bin/sh" [| "/bin/sh"; "-c"; Sys.argv.(i) |]
    | _ ->
        dup2 pipe_out stdin;
        close pipe_in;
        close pipe_out
  done;
  match fork () with
  | 0 ->
      execv "/bin/sh" [| "/bin/sh"; "-c"; Sys.argv.(n) |]
  | _ ->
      let rec wait_for_children retcode =
        try
          match wait () with
          | (_, WEXITED n) -> wait_for_children (retcode lor n)
          | (_, _)         -> wait_for_children 127
        with Unix_error(ECHILD, _, _) ->
          retcode
      in
      exit (wait_for_children 0)

let () =
  handle_unix_error compose ()
