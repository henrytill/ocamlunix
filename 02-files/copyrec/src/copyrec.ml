open Unix

let set_infos filename infos =
  utimes filename infos.st_atime infos.st_mtime;
  chmod filename infos.st_perm;
  try
    chown filename infos.st_uid infos.st_gid
  with Unix_error(EPERM, _, _) -> ()

let copied_files = (Hashtbl.create 53 : ((int * int), string) Hashtbl.t)

let rec copy_rec source dest =
  let infos = lstat source in
  match infos.st_kind with
  | S_REG ->
      if infos.st_nlink > 1 then
        begin
          try
            let dest' = Hashtbl.find copied_files (infos.st_dev, infos.st_ino) in
            link dest' dest
          with Not_found ->
            Hashtbl.add copied_files (infos.st_dev, infos.st_ino) dest;
            File_copy.file_copy false source dest;
            set_infos dest infos
        end
      else
        begin
          File_copy.file_copy false source dest;
          set_infos dest infos
        end
  | S_LNK ->
      let link = readlink source in
      symlink link dest
  | S_DIR ->
      mkdir dest 0o200;
      Misc.iter_dir
        (fun file ->
           if file <> Filename.current_dir_name && file <> Filename.parent_dir_name then
             copy_rec (Filename.concat source file) (Filename.concat dest file))
        source;
      set_infos dest infos
  | _ ->
      prerr_endline ("Can't cope with special file " ^ source)

let main () =
  if Array.length Sys.argv <> 3 then
    begin
      prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " <source> <destination>");
      exit 2
    end
  else
    begin
      copy_rec Sys.argv.(1) Sys.argv.(2);
      exit 0
    end

;;

handle_unix_error main ()
