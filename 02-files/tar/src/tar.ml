open Unix

type kind =
  | REG
  | LNK of string
  | LINK of string
  | CHR of int * int
  | BLK of int * int
  | DIR
  | FIFO
  | CONT

type header =
  { name  : string;
    perm  : int;
    uid   : int;
    gid   : int;
    size  : int;
    mtime : int;
    kind  : kind;
    user  : string;
    group : string }

type record =
  { header : header;
    offset : int;
    descr  : file_descr }

exception Error of string * string

let error err mes =
  raise (Error (err, mes))

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
  if i < 0 then
    error "Corrupted archive" "integer too large"
  else
    i

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
  | _   -> error "Corrupted archive" "kind"

let header_of_string s : header =
  { name  = substring s 0 99;
    perm  = int_of_octal 8 s 100;
    uid   = int_of_octal 8 s 108;
    gid   = int_of_octal 8 s 116;
    size  = int_of_octal 12 s 124;
    mtime = int_of_octal 12 s 136;
    kind  = kind s 156;
    user  = substring s 265 32;
    group = substring s 297 32; }

let total_size size =
  block_size + ((block_size - 1 + size) / block_size) * block_size

let rec really_read fd buffer start length =
  if length <= 0 then
    ()
  else
    match read fd buffer start length with
    | 0 -> raise End_of_file
    | r -> really_read fd buffer (start + r) (length + r)

let end_of_file_error () =
  error "Corrupted archive" "unexpected end of file"

let without_end_of_file f x =
  try f x with End_of_file -> end_of_file_error ()

let buffer_size = block_size

let buffer = Bytes.create buffer_size

let read_header fd =
  let len = read fd buffer 0 buffer_size in
  if len = 0 || buffer.[0] = '\000' then
    None
  else
    begin
      if len < buffer_size then
        without_end_of_file (really_read fd buffer len) (buffer_size - len);
      Some (header_of_string buffer)
    end

let fold f initial fd =
  let rec fold_aux offset accu =
    ignore (without_end_of_file (lseek fd offset) SEEK_SET);
    match without_end_of_file read_header fd with
    | None   -> accu
    | Some h ->
        let r = { header = h; offset = offset + block_size; descr = fd } in
        fold_aux (offset + total_size h.size) (f r accu)
  in
  fold_aux 0 initial

let list tarfile =
  let fd = openfile tarfile [O_RDONLY] 0o0 in
  let add r () = print_string r.header.name; print_newline () in
  fold add () fd;
  close fd

let rec find_regular r list =
  match r.header.kind with
  | REG | CONT -> r
  | LINK name -> find_file name list
  | _ -> error r.header.name "Not a regular file"

and find_file name list =
  match list with
  | r :: rest ->
      if r.header.name = name then
        find_regular r rest
      else
        find_file name rest
  | [] -> error name "Link not found (corrupted archive)"

let copy_file file output =
  ignore (lseek file.descr file.offset SEEK_SET);
  let rec copy_loop len =
    if len > 0 then
      match read file.descr buffer 0 (min buffer_size len) with
      | 0 -> end_of_file_error ()
      | r ->
          ignore (write output buffer 0 r);
          copy_loop (len - r)
  in
  copy_loop file.header.size

exception Done

let find_and_copy tarfile filename =
  let fd = openfile tarfile [O_RDONLY] 0o0 in
  let found_or_collect r accu =
    if r.header.name = filename then
      begin
        copy_file (find_regular r accu) stdout;
        raise Done
      end
    else
      r :: accu in
  try
    ignore (fold found_or_collect [] fd);
    error "File not found" filename
  with Done ->
    close fd

let readtar () =
  let nargs = Array.length Sys.argv in
  if nargs = 2 then
    list Sys.argv.(1)
  else if nargs = 3 then
    find_and_copy Sys.argv.(1) Sys.argv.(2)
  else
    prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " <tarfile> [ <source> ]")

;;

handle_unix_error (handle_error readtar) ()
