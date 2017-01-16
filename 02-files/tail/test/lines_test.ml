module type Lines_impl = sig

  val f : bytes -> bytes list

end

module Make (M : Lines_impl) : Common.Tests = struct

  let test_bytes : bytes =
    "this is the first line\nthis is the second line\nthis is the third line\n"

  let expected_test_bytes : bytes list = [
    "this is the first line\n";
    "this is the second line\n";
    "this is the third line\n";
  ]

  let test_bytes_no_trailing_newline : bytes =
    "this is the first line\nthis is the second line\nthis is the third line\nthis is the fourth line"

  let expected_test_bytes_no_trailing_newline : bytes list = [
    "this is the first line\n";
    "this is the second line\n";
    "this is the third line\n";
    "this is the fourth line";
  ]

  let test_bytes_test () =
    Alcotest.(check (list string))
      "same elements"
      expected_test_bytes
      (M.f test_bytes)

  let test_bytes_no_trailing_newline_test () =
    Alcotest.(check (list string))
      "same elements"
      expected_test_bytes_no_trailing_newline
      (M.f test_bytes_no_trailing_newline)

  let test_set = [
    "test_bytes_test",                     `Quick, test_bytes_test;
    "test_bytes_no_trailing_newline_test", `Quick, test_bytes_no_trailing_newline_test;
  ]

end

module Lines     = Make (struct let f = Tail.lines     end)
module Lines_alt = Make (struct let f = Tail.lines_alt end)
