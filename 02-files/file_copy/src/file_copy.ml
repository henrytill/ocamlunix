open Unix
open Common

let buffer_size = 8192
let buffer = Bytes.create buffer_size

let file_copy append input_name output_name =
  let fd_in = openfile input_name [O_RDONLY] 0 in
  let flags =
    if append
    then [O_WRONLY; O_CREAT; O_APPEND]
    else [O_WRONLY; O_CREAT; O_TRUNC] in
  let fd_out = openfile output_name flags 0o666 in
  let rec copy_loop () =
    match read fd_in buffer 0 buffer_size with
    | 0 -> ()
    | r -> ignore (write fd_out buffer 0 r); copy_loop ()
  in
  copy_loop ();
  close fd_in;
  close fd_out

let copy () =
  let append = ref false in
  let file_count = ref 0 in
  let files = Array.make 2 "" in
  let opt_list = [
    "-a", Arg.Set append, "Append to output file";
  ] in
  let process_arg x =
    Array.set files !file_count x;
    file_count := !file_count + 1
  in
  let usage_string = ("Usage: " ^ Sys.argv.(0) ^ " <input_file> <output_file>") in
  Arg.parse opt_list process_arg usage_string;
  if Array.for_all (fun a -> String.length a > 0) files
  then begin
    file_copy !append files.(0) files.(1);
    exit 0
  end
  else begin
    Arg.usage opt_list usage_string;
    exit 1
  end;;

handle_unix_error copy ()
