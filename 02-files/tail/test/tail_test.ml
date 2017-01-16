module Tail : Common.Tests = struct

  let load_file f =
    let channel = open_in f in
    Misc.try_finalize
      (fun () ->
         let file_length = in_channel_length channel in
         let bytes = Bytes.create file_length in
         really_input channel bytes 0 file_length;
         bytes) ()
      (fun () -> close_in channel) ()

  let bytes_list_t =
    let module M = struct
      type t = bytes list
      let equal xs ys = xs = ys
      let pp ppf xs = Format.pp_print_string ppf (String.concat "" (List.map Bytes.of_string xs))
    end in
    (module M : Alcotest.TESTABLE with type t = M.t)

  let file_bytes_test filename () =
    let file_bytes = load_file filename in
    let expected = Tail.lines file_bytes in
    let file_length = List.length expected in
    Alcotest.(check bytes_list_t)
      "same element"
      expected
      (Tail.main filename file_length)

  let test_set = [
    "file_bytes_test Makefile",        `Quick, (file_bytes_test "Makefile");
    "file_bytes_test /var/log/syslog", `Quick, (file_bytes_test "/var/log/syslog");
  ]

end
