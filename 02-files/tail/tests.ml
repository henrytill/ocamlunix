module type Tests = sig
  val test_set : (string * [> `Quick ] * (unit -> unit)) list
end

module Lines_tests : Tests = struct
  let test_bytes : bytes =
    Bytes.of_string "this is the first line\nthis is the second line\nthis is the third line\n"

  let expected_test_bytes : bytes array =
    Array.map Bytes.of_string
      [| "this is the first line\n"; "this is the second line\n"; "this is the third line\n" |]

  let test_bytes_no_trailing_newline : bytes =
    Bytes.of_string
      "this is the first line\n\
       this is the second line\n\
       this is the third line\n\
       this is the fourth line"

  let expected_test_bytes_no_trailing_newline : bytes array =
    Array.map Bytes.of_string
      [|
        "this is the first line\n";
        "this is the second line\n";
        "this is the third line\n";
        "this is the fourth line";
      |]

  let test_bytes_test () =
    Alcotest.(check (array bytes)) "same elements" expected_test_bytes (Tail.lines test_bytes)

  let test_bytes_no_trailing_newline_test () =
    Alcotest.(check (array bytes))
      "same elements" expected_test_bytes_no_trailing_newline
      (Tail.lines test_bytes_no_trailing_newline)

  let test_set =
    [
      ("test_bytes_test", `Quick, test_bytes_test);
      ("test_bytes_no_trailing_newline_test", `Quick, test_bytes_no_trailing_newline_test);
    ]
end

module Tail_tests : Tests = struct
  let load_file f =
    let channel = open_in f in
    Misc.try_finalize
      (fun () ->
        let file_length = in_channel_length channel in
        let bytes = Bytes.create file_length in
        really_input channel bytes 0 file_length;
        bytes)
      ()
      (fun () -> close_in channel)
      ()

  let file_bytes_test filename () =
    let file_bytes = load_file filename in
    let expected = Tail.lines file_bytes in
    let file_length = Array.length expected in
    Alcotest.(check (array bytes)) "same element" expected (Tail.tail filename file_length)

  let test_set = [ ("file_bytes_test tests.ml", `Quick, file_bytes_test "tests.ml") ]
end

let lines () =
  try Alcotest.run ~and_exit:false "lines" [ ("Lines_tests.test_set", Lines_tests.test_set) ]
  with Alcotest.Test_error -> Printf.printf "Continue!!\n%!"

let tail () = Alcotest.run ~and_exit:true "tail" [ ("Tail_tests.test_set", Tail_tests.test_set) ]

let () =
  lines ();
  tail ()
