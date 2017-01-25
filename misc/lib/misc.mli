(** Miscellaneous functions from
    {{: https://ocaml.github.io/ocamlunix/ocamlunix.html }
     {i Unix system programming in OCaml }}. *)

val try_finalize : ('a -> 'b) -> 'a -> ('c -> 'd) -> 'c -> 'b

val iter_dir : (bytes -> 'a) -> bytes -> unit

val restart_on_EINTR : ('a -> 'b) -> 'a -> 'b
(** Repeat a system call when it is interrupted by a signal, i.e. when the
    [EINTR] exception is raised *)

val free_children : 'a -> unit
(** Executes [waitpid] in non-blocking mode (option [WNOHANG]) to recover any
    dead children and repeats until either there are only live children (zero is
    returned instead of a child id) or there are no children ([ECHILD]
    exception) *)
