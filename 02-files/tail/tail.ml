open Unix

let get_indices bytes : int list =
  let rec loop start acc =
    try
      let next = Bytes.rindex_from bytes start '\n' in
      loop (next - 1) (next :: acc)
    with
    | Not_found -> 0 :: acc
  in
  let last_byte = Bytes.length bytes - 1 in
  let last_newline = Bytes.rindex_from bytes last_byte '\n' in
  loop last_byte (if last_byte != last_newline then [ last_byte ] else [])
;;

let get_n_indices bytes n : int list =
  let rec loop start counter acc =
    if counter = 0
    then acc
    else (
      try
        let next = Bytes.rindex_from bytes start '\n' in
        loop (next - 1) (counter - 1) (next :: acc)
      with
      | Not_found -> 0 :: acc)
  in
  let last_byte = Bytes.length bytes - 1 in
  let last_newline = Bytes.rindex_from bytes last_byte '\n' in
  loop last_byte n (if last_byte != last_newline then [ last_byte ] else [])
;;

let operate_on_bytes_at_indices source indices k : unit =
  let rec loop remaining count =
    match remaining with
    | [] -> ()
    | _ :: [] -> ()
    | 0 :: right :: rest ->
      k (Bytes.sub source 0 (right + 1)) count;
      loop (right :: rest) (count + 1)
    | left :: right :: rest ->
      k (Bytes.sub source (left + 1) (right - left)) count;
      loop (right :: rest) (count + 1)
  in
  loop indices 0
;;

let get_bytes_at_indices bytes indices : bytes array =
  let target_bytes = Array.make (List.length indices - 1) Bytes.empty in
  operate_on_bytes_at_indices bytes indices (fun src cnt ->
    Array.set target_bytes cnt src);
  target_bytes
;;

let print_bytes_at_indices bytes indices : unit =
  operate_on_bytes_at_indices bytes indices (fun src _ -> print_bytes src)
;;

let lines bytes =
  let indices = get_indices bytes in
  get_bytes_at_indices bytes indices
;;

let rec drop n xs =
  match xs with
  | [] -> []
  | _ :: tl -> if n = 0 then xs else drop (n - 1) tl
;;

let tail filename n =
  let fd = openfile filename [ O_RDONLY ] 0 in
  let file_size = lseek fd 0 SEEK_END in
  let max_size = 4096 in
  let rec loop pos acc =
    let chunk_size = min pos max_size in
    let acc_bytes = Bytes.extend acc chunk_size 0 in
    let start_pos = pos - chunk_size in
    ignore (lseek fd start_pos SEEK_SET);
    ignore (read fd acc_bytes 0 chunk_size);
    let xs = lines acc_bytes in
    let num_lines = Array.length xs in
    if num_lines >= n then Array.sub xs (num_lines - n) n else loop start_pos acc_bytes
  in
  Misc.try_finalize (loop file_size) Bytes.empty close fd
;;
