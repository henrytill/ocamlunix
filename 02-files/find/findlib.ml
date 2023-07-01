exception Hidden of exn

let hide_exn f x = try f x with exn -> raise (Hidden exn)
let reveal_exn f x = try f x with Hidden exn -> raise exn

let find on_error on_path follow depth roots =
  let rec find_rec depth visiting filename =
    try
      let infos = Unix.(if follow then stat else lstat) filename in
      let continue = hide_exn (on_path filename) infos in
      let id = (infos.st_dev, infos.st_ino) in
      if
        infos.st_kind = S_DIR
        && depth > 0
        && continue
        && ((not follow) || not (List.mem id visiting))
      then
        let process_child child =
          if child <> Filename.current_dir_name && child <> Filename.parent_dir_name then
            let child_name = Filename.concat filename child in
            let visiting = if follow then id :: visiting else visiting in
            find_rec (depth - 1) visiting child_name
        in
        Misc.iter_dir process_child filename
    with Unix.Unix_error (e, b, c) -> hide_exn on_error (e, b, c)
  in
  reveal_exn (List.iter (find_rec depth [])) roots
