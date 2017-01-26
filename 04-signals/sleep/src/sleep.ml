open Sys
open Unix

let sleep s =
  let old_alarm = signal sigalrm (Signal_handle (fun s -> ())) in
  let old_mask = sigprocmask SIG_UNBLOCK [ sigalrm ] in
  let _ = alarm s in
  let new_mask = List.filter (fun x -> x <> sigalrm) old_mask in
  sigsuspend new_mask;
  let _ = alarm 0 in
  ignore (signal sigalrm old_alarm);
  ignore (sigprocmask SIG_SETMASK old_mask)

let () =
  let argn = Array.length Sys.argv in
  if argn = 2 then
    handle_unix_error sleep (int_of_string Sys.argv.(1))
  else
    begin
      prerr_endline "Usage: sleep <time>";
      exit 2
    end
