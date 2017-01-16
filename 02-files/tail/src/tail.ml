open Unix

let lines bytes: bytes list =
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

let lines_alt bytes: bytes list =
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
  if n > List.length xs then
    []
  else if n = 0 then
    xs
  else
    drop (n - 1) (List.tl xs)

let main filename n =
  let fd = openfile filename [O_RDONLY] 0 in
  let file_size = lseek fd 0 SEEK_END in
  let max_size = 4096 in
  let rec loop buff =
    match buff with
    | Some b ->
      let current_size = Bytes.length b in
      let remaining_size = file_size - current_size in
      let new_size = if remaining_size < max_size then remaining_size else max_size in
      let b = Bytes.extend b new_size 0 in
      ignore (lseek fd (-(new_size + current_size)) SEEK_END);
      ignore (read fd b 0 new_size);
      let xs = lines b in
      let count = List.length xs in
      if count >= n then
        drop (count - n) xs
      else
        loop (Some b)
    | None ->
      let new_size = if file_size < max_size then file_size else max_size in
      let b = Bytes.create new_size in
      ignore (lseek fd (-new_size) SEEK_END);
      ignore (read fd b 0 new_size);
      let xs = lines b in
      let count = List.length xs in
      if count >= n then
        drop (count - n) xs
      else
        loop (Some b)
  in
  Misc.try_finalize
    loop None
    (fun () -> close fd) ()
