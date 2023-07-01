type t = {
  timer : Unix.interval_timer;
  interval : float;
  value : float;
}

let new_timer k f : t =
  let { Unix.it_interval = interval; it_value = value } = Unix.getitimer k in
  f ();
  { timer = k; interval; value }

let get_timer { timer = _; interval; value } : Unix.interval_timer_status =
  { it_interval = interval; it_value = value }

let set_timer { timer; interval; value } _ : Unix.interval_timer_status =
  Unix.setitimer timer { it_interval = interval; it_value = value }

let to_string { timer = _; interval = i; value = v } : string =
  Printf.sprintf "{ interval = %f; value = %f; }" i v
