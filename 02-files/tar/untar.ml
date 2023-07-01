let untar () =
  let nargs = Array.length Sys.argv in
  if nargs = 2 then
    Tarlib.extract Sys.argv.(1)
  else
    prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " <tarfile>")
;;

Unix.handle_unix_error untar ()
