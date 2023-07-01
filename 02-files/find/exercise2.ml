let get_exn = function
  | Some x -> x
  | None -> raise (Invalid_argument "get_exn None")

let file_id file =
  let open Unix in
  let stats = lstat file in
  (stats.st_dev, stats.st_ino)

let is_root_dir dir =
  let current_id = file_id dir in
  let parent_id = file_id (dir ^ "/..") in
  current_id = parent_id

let dir_name dir =
  let name = ref None in
  let dir_id = file_id dir in
  let find_name candidate =
    if dir_id = file_id (dir ^ "/../" ^ candidate) then name := Some candidate
  in
  Misc.iter_dir find_name (dir ^ "/..");
  !name

let getcwd () : string =
  let rec getcwd_rec curr_dir accum =
    if is_root_dir curr_dir then
      accum
    else
      getcwd_rec (curr_dir ^ "/..") ("/" ^ get_exn (dir_name curr_dir) ^ accum)
  in
  getcwd_rec "." ""
