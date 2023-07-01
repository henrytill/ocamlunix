let () =
  let argv = Sys.argv in
  ignore Sys.(signal sighup Signal_ignore);
  Unix.execvp argv.(1) (Array.sub argv 1 (Array.length argv - 1))
