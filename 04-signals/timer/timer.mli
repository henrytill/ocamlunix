type t = {
  timer : Unix.interval_timer;
  interval : float;
  value : float;
}

val new_timer : Unix.interval_timer -> (unit -> unit) -> t
val get_timer : t -> Unix.interval_timer_status
val set_timer : t -> Unix.interval_timer_status -> Unix.interval_timer_status
val to_string : t -> string
