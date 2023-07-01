type kind =
  | REG
  | LNK of string
  | LINK of string
  | CHR of int * int
  | BLK of int * int
  | DIR
  | FIFO
  | CONT

type header = {
  name : string;
  perm : int;
  uid : int;
  gid : int;
  size : int;
  mtime : int;
  kind : kind;
  user : string;
  group : string;
}

type record = {
  header : header;
  offset : int;
  descr : Unix.file_descr;
}

exception Error of string * string

let error err mes = raise (Error (err, mes))

let handle_error f s =
  try f s
  with Error (err, mes) ->
    Printf.eprintf "Error: %s: %s" err mes;
    exit 2

let block_size = 512

let substring s offset len : string =
  let max_length = min (offset + len + 1) (String.length s) in
  let rec real_length j =
    if j < max_length && s.[j] <> '\000' then
      real_length (succ j)
    else
      j - offset
  in
  String.sub s offset (real_length offset)

let int_of_octal nbytes s offset : int =
  let i = int_of_string ("0o" ^ substring s offset nbytes) in
  if i < 0 then error "Corrupted archive" "integer too large" else i

let kind s i : kind =
  match s.[i] with
  | '\000' | '0' -> REG
  | '1' -> LINK (substring s (succ i) 99)
  | '2' -> LNK (substring s (succ i) 99)
  | '3' -> CHR (int_of_octal 8 s 329, int_of_octal 8 s 329)
  | '4' -> BLK (int_of_octal 8 s 329, int_of_octal 8 s 337)
  | '5' -> DIR
  | '6' -> FIFO
  | '7' -> CONT
  | _ -> error "Corrupted archive" "kind"

let header_of_string s : header =
  {
    name = substring s 0 99;
    perm = int_of_octal 8 s 100;
    uid = int_of_octal 8 s 108;
    gid = int_of_octal 8 s 116;
    size = int_of_octal 12 s 124;
    mtime = int_of_octal 12 s 136;
    kind = kind s 156;
    user = substring s 265 32;
    group = substring s 297 32;
  }

let total_size size = block_size + ((block_size - 1 + size) / block_size * block_size)

let rec really_read fd buffer start length =
  if length <= 0 then
    ()
  else
    match Unix.read fd buffer start length with
    | 0 -> raise End_of_file
    | r -> really_read fd buffer (start + r) (length + r)

let end_of_file_error () = error "Corrupted archive" "unexpected end of file"
let without_end_of_file f x = try f x with End_of_file -> end_of_file_error ()
let buffer_size = block_size
let buffer = Bytes.create buffer_size

let read_header fd =
  let len = Unix.read fd buffer 0 buffer_size in
  if len = 0 || Bytes.get buffer 0 = '\000' then
    None
  else (
    if len < buffer_size then
      without_end_of_file (really_read fd buffer len) (buffer_size - len);
    Some (header_of_string (String.of_bytes buffer)))

let fold f initial fd =
  let rec fold_aux offset accu =
    ignore Unix.(without_end_of_file (lseek fd offset) SEEK_SET);
    match without_end_of_file read_header fd with
    | None -> accu
    | Some h ->
        let r = { header = h; offset = offset + block_size; descr = fd } in
        fold_aux (offset + total_size h.size) (f r accu)
  in
  fold_aux 0 initial

let list tarfile =
  let fd = Unix.(openfile tarfile [ O_RDONLY ] 0o0) in
  let add r () =
    print_string r.header.name;
    print_newline ()
  in
  fold add () fd;
  Unix.close fd

let rec find_regular r list =
  match r.header.kind with
  | REG | CONT -> r
  | LINK name -> find_file name list
  | _ -> error r.header.name "Not a regular file"

and find_file name list =
  match list with
  | r :: rest -> if r.header.name = name then find_regular r rest else find_file name rest
  | [] -> error name "Link not found (corrupted archive)"

let copy_file file output =
  ignore Unix.(lseek file.descr file.offset SEEK_SET);
  let rec copy_loop len =
    if len > 0 then
      match Unix.read file.descr buffer 0 (min buffer_size len) with
      | 0 -> end_of_file_error ()
      | r ->
          ignore (Unix.write output buffer 0 r);
          copy_loop (len - r)
  in
  copy_loop file.header.size

exception Done

let find_and_copy_v1 tarfile filename =
  let fd = Unix.(openfile tarfile [ O_RDONLY ] 0o0) in
  let found_or_collect r accu =
    if r.header.name = filename then (
      copy_file (find_regular r accu) Unix.stdout;
      raise Done)
    else
      r :: accu
  in
  try
    ignore (fold found_or_collect [] fd);
    error "File not found" filename
  with Done -> Unix.close fd

let readtar_v1 () =
  let nargs = Array.length Sys.argv in
  if nargs = 2 then
    list Sys.argv.(1)
  else if nargs = 3 then
    find_and_copy_v1 Sys.argv.(1) Sys.argv.(2)
  else
    prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " <tarfile> [ <source> ]")

(* Exercise 7 *)

type info =
  | File
  | Link of string list
  | Dir of (string * inode) list

and inode = {
  mutable record : record option;
  mutable info : info;
}

let root () =
  let rec i = { record = None; info = Dir [ (Filename.current_dir_name, i) ] } in
  i

let link inode name nod =
  match inode.info with
  | File | Link _ -> error name "Not a directory"
  | Dir list -> (
      try
        let _ = List.assoc name list in
        error name "Already exists"
      with Not_found -> inode.info <- Dir ((name, nod) :: list))

let mkfile inode name r =
  let f = { record = r; info = File } in
  link inode name f;
  f

let symlink inode name r path =
  let s = { record = r; info = Link path } in
  link inode name s;
  s

let mkdir inode name r =
  let d = mkfile inode name r in
  d.info <- Dir [ (Filename.current_dir_name, d); (Filename.parent_dir_name, inode) ];
  d

let rec find link inode path =
  match (inode.info, path) with
  | _, [] -> inode
  | Dir list, name :: rest ->
      let subnode = List.assoc name list in
      let subnode =
        match subnode.info with
        | Link _ when link && rest = [] -> subnode
        | Link q -> find false inode q
        | _ -> subnode
      in
      find link subnode rest
  | _, _ -> raise Not_found

let rec mkpath inode path =
  match (inode.info, path) with
  | _, [] -> inode
  | Dir list, name :: rest ->
      let subnode = try List.assoc name list with Not_found -> mkdir inode name None in
      mkpath subnode rest
  | _, _ -> raise Not_found

let explode f =
  let rec dec f p =
    if f = Filename.current_dir_name then
      p
    else
      dec (Filename.dirname f) (Filename.basename f :: p)
  in
  dec (if Filename.basename f = "" then Filename.dirname f else f) []

let add archive r =
  match r.header.kind with
  | CHR (_, _) | BLK (_, _) | FIFO -> ()
  | kind -> (
      match List.rev (explode r.header.name) with
      | [] -> ()
      | name :: parent_rev -> (
          let inode = mkpath archive (List.rev parent_rev) in
          match kind with
          | DIR -> ignore (mkdir inode name (Some r))
          | REG | CONT -> ignore (mkfile inode name (Some r))
          | LNK f -> ignore (symlink inode name (Some r) (explode f))
          | LINK f -> link inode name (find true archive (explode f))
          | _ -> assert false))

let find_and_copy_v2 tarfile filename =
  let fd = Unix.(openfile tarfile [ O_RDONLY ] 0) in
  let records = List.rev (fold (fun x y -> x :: y) [] fd) in
  let archive = root () in
  List.iter (add archive) records;
  let inode =
    try find false archive (explode filename) with Not_found -> error filename "File not found"
  in
  (match inode.record with
  | Some ({ header = { kind = REG | CONT; _ }; _ } as r) -> copy_file r Unix.stdout
  | Some _ -> error filename "Not a regular file"
  | None -> error filename "Not found");
  Unix.close fd

let readtar_v2 () =
  let nargs = Array.length Sys.argv in
  if nargs = 2 then
    list Sys.argv.(1)
  else if nargs = 3 then
    find_and_copy_v1 Sys.argv.(1) Sys.argv.(2)
  else
    prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " <tarfile> [ <source> ]")

(* Exercise 8 *)

let warning mes =
  prerr_string mes;
  prerr_newline ()

let mkpath p perm =
  let open Filename in
  let normal_path = if basename p = "" then dirname p else p in
  let path_to_dir = dirname normal_path in
  let rec make p =
    try ignore (Unix.stat p)
    with Unix.Unix_error (ENOENT, _, _) ->
      if p = current_dir_name then
        ()
      else if p = parent_dir_name then
        warning "Ill-formed archive: path contains \"..\""
      else (
        make (dirname p);
        Unix.mkdir p perm)
  in
  make path_to_dir

let set_infos header =
  Unix.chmod header.name header.perm;
  let mtime = float header.mtime in
  Unix.utimes header.name mtime mtime;
  (match header.kind with
  | LNK _ -> ()
  | _ -> Unix.chmod header.name header.perm);
  try Unix.chown header.name header.uid header.gid with Unix.Unix_error (EPERM, _, _) -> ()

let verbose = ref true
let default_dir_perm = 0o777
let default_file_perm = 0o666

let protect f x g y =
  try
    f x;
    g y
  with z ->
    g y;
    raise z

let file_exists f =
  try
    ignore (Unix.stat f);
    true
  with _ -> false

let untar_file_collect_dirs file dirs =
  let fh = file.header in
  if !verbose then (
    print_string fh.name;
    print_newline ());
  match fh.kind with
  | CHR (_, _) | BLK (_, _) | FIFO ->
      warning (fh.name ^ "Ignoring special files");
      dirs
  | DIR ->
      mkpath fh.name default_dir_perm;
      if file_exists fh.name then
        dirs
      else (
        Unix.mkdir fh.name default_dir_perm;
        fh :: dirs)
  | x ->
      mkpath fh.name default_dir_perm;
      (match x with
      | REG | CONT ->
          let flags = Unix.[ O_WRONLY; O_TRUNC; O_CREAT ] in
          let out = Unix.openfile fh.name flags default_file_perm in
          protect (copy_file file) out Unix.close out
      | LNK f -> Unix.symlink f fh.name
      | LINK f ->
          (try if Unix.(stat fh.name).st_kind = S_REG then Unix.unlink fh.name
           with Unix.Unix_error (_, _, _) -> ());
          Unix.link f fh.name
      | _ -> assert false);
      set_infos fh;
      dirs

let extract tarfile =
  let fd = Unix.(openfile tarfile [ O_RDONLY ] 0) in
  let new_directories = fold untar_file_collect_dirs [] fd in
  List.iter set_infos new_directories;
  Unix.close fd
