open Unix

type t =
  { timer    : interval_timer;
    interval : float;
    value    : float; }

let new_timer k f : t =
  let { it_interval = interval; it_value = value } = getitimer k in
  f ();
  { timer = k; interval; value }

let get_timer { timer = _; interval; value } : interval_timer_status =
  { it_interval = interval; it_value = value }

let set_timer { timer; interval; value } _status : interval_timer_status =
  setitimer timer { it_interval = interval; it_value = value }

let to_string { timer = _; interval = i; value = v; } : string =
  Printf.sprintf "{ interval = %f; value = %f; }" i v
