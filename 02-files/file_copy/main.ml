module Array = Ext_array

let copy () =
  let append = ref false in
  let file_count = ref 0 in
  let files = Array.make 2 "" in
  let opt_list = [ ("-a", Arg.Set append, "Append to output file") ] in
  let process_arg x =
    Array.set files !file_count x;
    file_count := !file_count + 1
  in
  let usage_string = "Usage: " ^ Sys.argv.(0) ^ " [options...] <input_file> <output_file>" in
  try
    Arg.parse opt_list process_arg usage_string;
    if Array.for_all (fun a -> String.length a > 0) files then (
      File_copy.file_copy !append files.(0) files.(1);
      exit 0)
    else
      raise (Invalid_argument "Invalid file names")
  with _ ->
    Arg.usage opt_list usage_string;
    exit 1

let () = Unix.handle_unix_error copy ()
