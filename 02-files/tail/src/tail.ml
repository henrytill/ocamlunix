open Unix

let lines bytes : bytes list =
  let open Bytes in
  let rec loop start_pos acc =
    try
      let end_pos = index_from bytes start_pos '\n' + 1 in
      let offset = end_pos - start_pos in
      loop end_pos (acc @ [(sub bytes start_pos offset)])
    with Not_found ->
      let end_pos = length bytes in
      let offset = end_pos - start_pos in
      if offset > 0 then
        acc @ [(sub bytes start_pos offset)]
      else
        acc
  in
  loop 0 []

let lines_alt bytes : bytes list =
  let open Bytes in
  let rec loop end_pos acc =
    try
      let start_pos = rindex_from bytes end_pos '\n' in
      let offset = (end_pos + 2) - (start_pos + 1) in
      loop (start_pos - 1) ((sub bytes (start_pos + 1) offset) :: acc)
    with Not_found ->
      if end_pos > 0 then
        (sub bytes 0 (end_pos + 2)) :: acc
      else
        acc
  in
  loop (Bytes.length bytes - 2) []

let rec drop n xs =
  match xs with
  | [] -> []
  | _ :: tl -> if n = 0 then xs else drop (n - 1) tl

let main filename n =
  let fd = openfile filename [O_RDONLY] 0 in
  let file_size = lseek fd 0 SEEK_END in
  let max_size = 4096 in
  let rec loop pos acc =
    let chunk_size = if pos <= max_size then pos else max_size in
    let start_pos = pos - chunk_size in
    let b = Bytes.create chunk_size in
    ignore (lseek fd start_pos SEEK_SET);
    ignore (read fd b 0 chunk_size);
    let acc_bytes = (Bytes.cat b acc) in
    let xs = lines_alt acc_bytes in
    let num_lines = List.length xs in
    if num_lines >= n then
      drop (num_lines - n) xs
    else
      loop start_pos acc_bytes
  in
  Misc.try_finalize
    (fun initial_pos -> loop initial_pos Bytes.empty) file_size
    (fun () -> close fd) ()
