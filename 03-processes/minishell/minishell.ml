open Unix
open Printf

let split_words s =
  let rec skip_blanks i =
    if i < String.length s && s.[i] = ' ' then skip_blanks (i + 1) else i
  in
  let rec split start i =
    if i >= String.length s
    then [ String.sub s start (i - start) ]
    else if s.[i] = ' '
    then (
      let j = skip_blanks i in
      String.sub s start (i - start) :: split j j)
    else split start (i + 1)
  in
  Array.of_list (split 0 0)
;;

let exec_command cmd =
  try execvp cmd.(0) cmd with
  | Unix_error (err, _, _) ->
    printf "Cannot execute %s : %s\n%!" cmd.(0) (error_message err);
    exit 255
;;

let print_status program status =
  match status with
  | WEXITED 255 -> ()
  | WEXITED status -> printf "%s exited with code %d\n%!" program status
  | WSIGNALED signal -> printf "%s killed by signal %d\n%!" program signal
  | WSTOPPED _ -> printf "%s stopped (???)\n%!" program
;;

let parse_command_line s =
  let new_s, ampersand =
    try
      let len = String.length s in
      if String.index s '&' = len - 1 then String.sub s 0 (len - 2), true else s, false
    with
    | Not_found -> s, false
  in
  let words = split_words new_s in
  words, ampersand
;;

let minishell () =
  try
    while true do
      let cmd = input_line Stdlib.stdin in
      let words, ampersand = parse_command_line cmd in
      match fork () with
      | 0 -> exec_command words
      | pid_son ->
        if ampersand
        then ()
        else (
          let rec wait_for_son () =
            let pid, status = wait () in
            if pid = pid_son
            then print_status "Program" status
            else (
              let p = "Background program " ^ string_of_int pid in
              print_status p status;
              wait_for_son ())
          in
          wait_for_son ())
    done
  with
  | End_of_file -> ()
;;

let () = handle_unix_error minishell ()
