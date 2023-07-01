let rec really_read fd buff start length =
  if length <= 0 then
    ()
  else
    match Unix.read fd buff start length with
    | 0 -> raise End_of_file
    | n -> really_read fd buff (start + n) (length - n)

let buffer = Bytes.create 258

let multiplex channel inputs outputs =
  let input_fds = channel :: Array.to_list inputs in
  try
    while true do
      let ready_fds, _, _ = Unix.select input_fds [] [] (-1.0) in
      for i = 0 to Array.length inputs - 1 do
        if List.mem inputs.(i) ready_fds then (
          let n = Unix.read inputs.(i) buffer 2 255 in
          Bytes.set buffer 0 (char_of_int i);
          Bytes.set buffer 1 (char_of_int n);
          ignore (Unix.write channel buffer 0 (n + 2)))
      done;
      if List.mem channel ready_fds then (
        really_read channel buffer 0 2;
        let i = int_of_char (Bytes.get buffer 0) and n = int_of_char (Bytes.get buffer 1) in
        if n = 0 then
          Unix.close outputs.(i)
        else (
          really_read channel buffer 0 n;
          ignore (Unix.write outputs.(i) buffer 0 n)))
    done
  with End_of_file -> ()
