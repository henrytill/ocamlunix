(* Exercise 9 *)

type archive = {
  regfiles : (int * int, string) Hashtbl.t;
  dirfiles : (int * int, bool) Hashtbl.t;
  fd : Unix.file_descr;
  st : Unix.stats;
  mutable size : int;
}

let warning path message = prerr_endline (path ^ ": " ^ message)

let write_header_to_buffer source infos kind =
  let open Unix in
  let size = if kind = Tarlib.REG then infos.st_size else 0 in
  Tarlib.(Bytes.fill buffer 0 block_size '\000');
  let put len string offset =
    String.blit string 0 Tarlib.buffer offset (min (String.length string) len)
  in
  let put_int8 x = put 7 (Printf.sprintf "%07o" x) in
  let put_int12 x = put 11 (Printf.sprintf "%011o" x) in
  let put_char c offset = Bytes.set Tarlib.buffer offset c in
  let put_path s offset =
    if String.length s <= 99 then
      put 99 s offset
    else
      raise (Tarlib.Error ("path too long", s))
  in
  put_path (if kind = DIR then source ^ "/" else source) 0;
  put_int8 infos.st_perm 100;
  put_int8 infos.st_uid 108;
  put_int8 infos.st_gid 116;
  put_int12 size 124;
  put_int12 (int_of_float infos.st_mtime) 136;
  put 7 "ustar " 257;
  put 31 Unix.(getpwuid infos.st_uid).pw_name 265;
  put 31 Unix.(getgrgid infos.st_gid).gr_name 297;
  (* Files dev and rdev are only used for special files, which we omit *)
  begin
    match kind with
    | REG -> put_char '0' 156
    | LINK s ->
        put_path s 157;
        put_char '1' 156
    | LNK s ->
        put_path s 157;
        put_char '2' 156
    | DIR -> put_char '5' 156
    | _ -> failwith "Special files not implemented"
  end;
  let rec sum s i =
    if i < 0 then
      s
    else
      sum (s + Char.code (Bytes.get Tarlib.buffer i)) (pred i)
  in
  let checksum = sum (Char.code ' ' * 8) (Tarlib.block_size - 1) in
  put 8 (Printf.sprintf "%06o\000 " checksum) 148

let write_file len source fdout =
  let fdin = Unix.(openfile source [ O_RDONLY ] 0) in
  let error () = raise (Tarlib.Error ("File changed size", source)) in
  let rec copy_loop len =
    match Tarlib.(Unix.read fdin buffer 0 buffer_size) with
    | 0 ->
        Unix.close fdin;
        if len > 0 then error ()
    | r ->
        let len = len - r in
        if len < 0 then (
          Unix.close fdin;
          error ());
        ignore (Unix.write fdout Tarlib.buffer 0 r);
        copy_loop len
  in
  copy_loop len

let padding fd len = if len > 0 then ignore (Unix.write fd (Bytes.make len '\000') 0 len)

let try_new_dir archive dir =
  try Hashtbl.find archive.dirfiles dir
  with Not_found ->
    Hashtbl.add archive.dirfiles dir false;
    true

let verbose = ref true

let write_from archive file =
  if not (Filename.is_relative file) then
    raise (Tarlib.Error ("absolute path", file));
  let rec write_rec archive file =
    let source = if Filename.basename file = "" then Filename.dirname file else file in
    if !verbose then prerr_endline source;
    let st = Unix.lstat source in
    if st.st_ino = archive.st.st_ino && st.st_dev = archive.st.st_dev then
      warning source "Skipping archive itself!"
    else
      let write_header kind =
        write_header_to_buffer source st kind;
        ignore Tarlib.(Unix.write archive.fd buffer 0 block_size)
      in
      match st.st_kind with
      | S_REG -> (
          try
            if st.st_nlink = 1 then raise Not_found;
            let path = Hashtbl.find archive.regfiles (st.st_ino, st.st_dev) in
            write_header (LINK path)
          with Not_found ->
            if st.st_nlink > 1 then
              Hashtbl.add archive.regfiles (st.st_ino, st.st_dev) source;
            write_header REG;
            write_file st.st_size source archive.fd;
            let t = Tarlib.((block_size - 1 + st.st_size) / block_size * block_size) in
            padding archive.fd (t - st.st_size);
            archive.size <- archive.size + t + Tarlib.block_size)
      | S_LNK -> write_header (LNK (Unix.readlink source))
      | S_DIR when try_new_dir archive (st.st_ino, st.st_dev) ->
          write_header DIR;
          Misc.iter_dir
            (fun file ->
              if file = Filename.current_dir_name then
                ()
              else if file = Filename.parent_dir_name then
                ()
              else
                write_rec archive (source ^ "/" ^ file))
            source
      | S_DIR -> warning source "Ignoring directory already in archive"
      | _ -> prerr_endline ("Can't cope with specia file " ^ source)
  in
  write_rec archive file

let min_archive_size = 20 * Tarlib.block_size

let build tarfile files =
  let open Unix in
  let fd, remove =
    if tarfile = "-" then
      (stdout, ignore)
    else
      (openfile tarfile [ O_WRONLY; O_CREAT; O_TRUNC ] 0o666, unlink)
  in
  try
    let arch =
      { regfiles = Hashtbl.create 13; dirfiles = Hashtbl.create 13; st = fstat fd; fd; size = 0 }
    in
    Array.iter (write_from arch) files;
    padding fd (min_archive_size - arch.size);
    close fd
  with z ->
    remove tarfile;
    close fd;
    raise z

let usage () =
  prerr_endline "Usage: tar -cvf tarfile file1 [ file 2 ... ]";
  exit 2

let tar () =
  let argn = Array.length Sys.argv in
  if argn > 3 && Sys.argv.(1) = "-cvf" then
    build Sys.argv.(2) (Array.sub Sys.argv 3 (argn - 3))
  else
    usage ()

let _ =
  try Unix.handle_unix_error tar ()
  with Tarlib.Error (mes, s) ->
    prerr_endline ("Error: " ^ mes ^ ": " ^ s);
    exit 1
