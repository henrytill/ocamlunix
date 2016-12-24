let find () =
  let follow = ref false in
  let maxdepth = ref max_int in
  let roots = ref [] in
  let usage_string =
    ("Usage: " ^ Sys.argv.(0) ^ " [files...] [options...]") in
  let opt_list = [
    "-maxdepth", Arg.Int ((:=) maxdepth), "max depth search";
    "-follow", Arg.Set follow, "follow symbolic links";
  ] in
  Arg.parse opt_list (fun f -> roots := f :: !roots) usage_string;
  let action p infos = print_endline p; true in
  let errors = ref false in
  let on_error (e, b, c) = errors := true; prerr_endline (c ^ ": " ^ Unix.error_message e) in
  Findlib.find on_error action !follow !maxdepth
    (if !roots = [] then [ Filename.current_dir_name ]
     else List.rev !roots);
  if !errors then exit 1;;

Unix.handle_unix_error find ();;
