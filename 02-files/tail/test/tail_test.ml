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

  let file_bytes_test filename () =
    let file_bytes = load_file filename in
    let expected = Tail.lines file_bytes in
    let file_length = Array.length expected in
    Alcotest.(check (array string))
      "same element"
      expected
      (Tail.tail filename file_length)

  let test_set = [
    "file_bytes_test build",           `Quick, (file_bytes_test "build");
    "file_bytes_test /var/log/syslog", `Quick, (file_bytes_test "/var/log/syslog");
  ]

end
