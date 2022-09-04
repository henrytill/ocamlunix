open Unix

type t =
  { timer : interval_timer
  ; interval : float
  ; value : float
  }

val new_timer : interval_timer -> (unit -> unit) -> t

val get_timer : t -> interval_timer_status

val set_timer : t -> interval_timer_status -> interval_timer_status

val to_string : t -> string
