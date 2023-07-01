let main () =
  let action p infos =
    let open Unix in
    let b = not (infos.st_kind = S_DIR || Filename.basename p = "CVS") in
    if b then print_endline p;
    b
  in
  let errors = ref false in
  let error (e, _, b) =
    errors := true;
    prerr_endline (b ^ ": " ^ Unix.error_message e)
  in
  Findlib.find error action false max_int [ "." ]
;;

Unix.handle_unix_error main ()
