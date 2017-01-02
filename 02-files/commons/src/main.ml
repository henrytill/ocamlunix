let print () =
  let chan = Commons.open_in Sys.argv.(1) in
  try
    while true do
      print_char (Commons.input_char chan);
    done
  with Commons.End_of_file ->
    Commons.close_in chan
;;

Unix.handle_unix_error print ()
