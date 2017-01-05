(* Inspired by: https://groups.google.com/d/msg/fa.caml/GP9p9wTo-Cs/NVTl3idPSLYJ *)
type 'a t =
  { mutable read_pos : int;
    mutable write_pos : int;
    mutable element_count : int;
    data : 'a array;
    size : int }

val create : int -> 'a -> 'a t

val push : 'a t -> 'a -> unit

val pop : 'a t -> 'a
