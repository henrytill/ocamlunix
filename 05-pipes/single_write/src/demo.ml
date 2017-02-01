let rec really_write fd buffer offset len =
  let n = Misc.restart_on_EINTR (Single_write.single_write fd buffer offset) len in
  if n < len then really_write fd buffer (offset + n) (len - n)

let () =
  let main str = really_write Unix.stdout str 0 (Bytes.length str) in
  let output = "Hello, world!" in
  Unix.handle_unix_error main output
