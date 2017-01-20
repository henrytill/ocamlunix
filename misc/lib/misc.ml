let try_finalize f x finally y =
  let res = try f x with exn -> finally y; raise exn in
  finally y;
  res

(*
 * A better alternative?
 *
 * http://stackoverflow.com/questions/11276985/emulating-try-with-finally-in-ocaml
 * http://caml.inria.fr/pub/ml-archives/caml-list/2003/07/5ff669a9d2be35ec585b536e2e0fc7ca.en.html
 *
 *)
let protect ~f ~(finally: unit -> unit) =
  let result = ref None in
  try
    result := Some (f ());
    raise Exit
  with
  | Exit as e ->
    finally ();
    (match !result with Some x -> x | None -> raise e)
  | e ->
    finally (); raise e

let iter_dir f dirname =
  let open Unix in
  let d = opendir dirname in
  try while true do f (readdir d) done
  with End_of_file -> closedir d
