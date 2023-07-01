exception Found

let simple_search cond v =
  try
    for i = 0 to Array.length v - 1 do
      if cond v.(i) then raise Found
    done;
    false
  with Found -> true

let fork_search cond v =
  let n = Array.length v in
  match Unix.fork () with
  | 0 ->
      (* child process *)
      let found = simple_search cond (Array.sub v (n / 2) (n - (n / 2))) in
      exit (if found then 0 else 1)
  | _ -> (
      (* parent process *)
      let found = simple_search cond (Array.sub v 0 (n / 2)) in
      match Unix.wait () with
      | _, WEXITED retcode -> found || retcode = 0
      | _, _ -> failwith "fork_search")

let () =
  fork_search (fun x -> x = 4_999_999) (Array.init 5_000_000 (fun x -> x))
  |> string_of_bool
  |> print_endline
