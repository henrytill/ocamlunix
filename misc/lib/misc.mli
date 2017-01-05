val try_finalize : ('a -> 'b) -> 'a -> ('c -> 'd) -> 'c -> 'b

val protect : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a
