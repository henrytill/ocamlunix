let sleep s =
  let old_alarm = Sys.(signal sigalrm (Signal_handle (fun _ -> ()))) in
  let old_mask = Unix.(sigprocmask SIG_UNBLOCK [ Sys.sigalrm ]) in
  let _ = Unix.alarm s in
  let new_mask = List.filter (fun x -> x <> Sys.sigalrm) old_mask in
  Unix.sigsuspend new_mask;
  let _ = Unix.alarm 0 in
  ignore Sys.(signal sigalrm old_alarm);
  ignore Unix.(sigprocmask SIG_SETMASK old_mask)
;;

let () =
  let argn = Array.length Sys.argv in
  if argn = 2
  then Unix.handle_unix_error sleep (int_of_string Sys.argv.(1))
  else (
    prerr_endline "Usage: sleep <time>";
    exit 2)
;;
