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

let new_lines bytes : int list =
  let rec loop start acc =
    try
      let next = Bytes.rindex_from bytes start '\n' in
      if start != next then
        loop (next - 1) (next :: acc)
      else
        loop (next - 1) acc
    with Not_found ->
      0 :: acc
  in
  let last_byte = Bytes.length bytes - 1 in
  loop last_byte [last_byte]

let new_lines_count bytes n : int list =
  let rec loop start counter acc =
    if counter = 0 then
      acc
    else
      try
        let next = Bytes.rindex_from bytes start '\n' in
        loop (next - 1) (counter - 1) (next :: acc)
      with Not_found ->
        0 :: acc
  in
  let last_byte = Bytes.length bytes - 1 in
  loop last_byte n [last_byte]

let operate_on_bytes_at_indices source indices k : unit =
  let rec loop remaining count =
    match remaining with
    | []                     -> ()
    | right :: []            -> ()
    | 0     :: right :: rest ->
      k (Bytes.sub source 0 (right + 1)) count;
      loop (right :: rest) (count + 1)
    | left  :: right :: rest ->
      k (Bytes.sub source (left + 1) (right - left)) count;
      loop (right :: rest) (count + 1)
  in
  loop indices 0

let get_bytes_at_indices bytes indices : bytes array =
  let target_bytes = Array.make ((List.length indices) - 1) Bytes.empty in
  operate_on_bytes_at_indices bytes indices (fun src cnt -> Array.set target_bytes cnt src);
  target_bytes

let print_bytes_at_indices bytes indices : unit =
  operate_on_bytes_at_indices bytes indices (fun src _ -> print_bytes src)

let new_tail bytes count =
  let indices = new_lines_count bytes count in
  get_bytes_at_indices bytes indices

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
