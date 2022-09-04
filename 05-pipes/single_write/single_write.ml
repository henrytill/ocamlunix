external unsafe_single_write
  :  Unix.file_descr
  -> string
  -> int
  -> int
  -> int
  = "caml_single_write"

let single_write fd buf ofs len =
  if ofs < 0 || len < 0 || ofs > String.length buf - len
  then invalid_arg "Unix.write"
  else unsafe_single_write fd buf ofs len
;;
