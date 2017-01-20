val try_finalize : ('a -> 'b) -> 'a -> ('c -> 'd) -> 'c -> 'b

val protect : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a

val iter_dir : (bytes -> 'a) -> bytes -> unit
