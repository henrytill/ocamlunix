open Unix

let buffer_size = 8192
let buffer = Bytes.create buffer_size

let file_copy append input_name output_name =
  let output_flags =
    if append then [ O_WRONLY; O_CREAT; O_APPEND ] else [ O_WRONLY; O_CREAT; O_TRUNC ]
  in
  let fd_in = openfile input_name [ O_RDONLY ] 0 in
  let fd_out = openfile output_name output_flags 0o666 in
  let rec copy_loop () =
    match read fd_in buffer 0 buffer_size with
    | 0 -> ()
    | r ->
      ignore (write fd_out buffer 0 r);
      copy_loop ()
  in
  copy_loop ();
  close fd_in;
  close fd_out
;;
