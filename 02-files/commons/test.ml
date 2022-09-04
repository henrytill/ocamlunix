let read_file_as_string file =
  let actual = ref "" in
  let chan = Commons.open_in file in
  (try
     while true do
       actual := !actual ^ String.make 1 (Commons.input_char chan)
     done
   with
   | Commons.End_of_file -> Commons.close_in chan);
  !actual
;;

let round_trip_string expected =
  let temp_file = Filename.temp_file "commons." ".txt" in
  let temp_file_channel = Commons.open_out temp_file in
  let output_thunk () = Commons.output_string temp_file_channel expected in
  Misc.try_finalize output_thunk () Commons.close_out temp_file_channel;
  read_file_as_string temp_file
;;

let round_trip_string_test () =
  let expected = "This is a test" in
  Alcotest.(check string) "same string" expected (round_trip_string expected)
;;

let test_set = [ "round trip a string to a file", `Quick, round_trip_string_test ]
let () = Alcotest.run "Commons" [ "test_set", test_set ]
