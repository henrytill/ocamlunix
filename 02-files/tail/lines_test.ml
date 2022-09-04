module type Lines_impl = sig
  val f : bytes -> bytes array
end

module Make (M : Lines_impl) : Common.Tests = struct
  let test_bytes : string =
    "this is the first line\nthis is the second line\nthis is the third line\n"
  ;;

  let expected_test_bytes : string array =
    [| "this is the first line\n"
     ; "this is the second line\n"
     ; "this is the third line\n"
    |]
  ;;

  let test_bytes_no_trailing_newline : string =
    "this is the first line\n\
     this is the second line\n\
     this is the third line\n\
     this is the fourth line"
  ;;

  let expected_test_bytes_no_trailing_newline : string array =
    [| "this is the first line\n"
     ; "this is the second line\n"
     ; "this is the third line\n"
     ; "this is the fourth line"
    |]
  ;;

  let test_bytes_test () =
    Alcotest.(check (array bytes))
      "same elements"
      (Array.map Bytes.of_string expected_test_bytes)
      (M.f (Bytes.of_string test_bytes))
  ;;

  let test_bytes_no_trailing_newline_test () =
    Alcotest.(check (array bytes))
      "same elements"
      (Array.map Bytes.of_string expected_test_bytes_no_trailing_newline)
      (M.f (Bytes.of_string test_bytes_no_trailing_newline))
  ;;

  let test_set =
    [ "test_bytes_test", `Quick, test_bytes_test
    ; "test_bytes_no_trailing_newline_test", `Quick, test_bytes_no_trailing_newline_test
    ]
  ;;
end

module New_lines = Make (struct
  let f = Tail.lines
end)
