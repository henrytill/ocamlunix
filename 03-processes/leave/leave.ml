let leave () =
  let hh = int_of_string (String.sub Sys.argv.(1) 0 2)
  and mm = int_of_string (String.sub Sys.argv.(1) 2 2) in
  let now = Unix.(localtime (time ())) in
  let delay = ((hh - now.tm_hour) * 3600) + ((mm - now.tm_min) * 60) in
  if delay <= 0
  then (
    print_endline "Hey! That time has already passed!";
    exit 0);
  if Unix.fork () <> 0 then exit 0;
  Unix.sleep delay;
  print_endline "\007\007\007Time to leave!";
  exit 0
;;

let () = Unix.handle_unix_error leave ()
