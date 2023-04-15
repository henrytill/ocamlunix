let buffer_size = 8192
let buffer = Bytes.create buffer_size

let file_copy append input_name output_name =
  let output_flags =
    if append
    then Unix.[ O_WRONLY; O_CREAT; O_APPEND ]
    else Unix.[ O_WRONLY; O_CREAT; O_TRUNC ]
  in
  let fd_in = Unix.(openfile input_name [ O_RDONLY ] 0) in
  let fd_out = Unix.openfile output_name output_flags 0o666 in
  let rec go () =
    match Unix.read fd_in buffer 0 buffer_size with
    | 0 -> ()
    | r ->
      ignore (Unix.write fd_out buffer 0 r);
      go ()
  in
  go ();
  Unix.close fd_in;
  Unix.close fd_out
;;
