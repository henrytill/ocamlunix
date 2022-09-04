(** Miscellaneous functions from {{:https://ocaml.github.io/ocamlunix/ocamlunix.html}
    {i Unix system programming in OCaml}}. *)

val try_finalize : ('a -> 'b) -> 'a -> ('c -> unit) -> 'c -> 'b
val iter_dir : (string -> unit) -> string -> unit

(** Repeat a system call when it is interrupted by a signal, i.e. when the [EINTR] exception is
    raised *)
val restart_on_EINTR : ('a -> 'b) -> 'a -> 'b

(** Executes [waitpid] in non-blocking mode (option [WNOHANG]) to recover any dead children and
    repeats until either there are only live children (zero is returned instead of a child id) or
    there are no children ([ECHILD] exception) *)
val free_children : 'a -> unit

(** Reads data on the descriptor [fdin] and writes it on [fdout]. It terminates, without closing the
    descriptors, when the end of file is reached on the input descriptor. Note that [retransmit] may
    be interrupted by a signal. *)
val retransmit : Unix.file_descr -> Unix.file_descr -> unit

(** Creates a socket of type stream in the Internet domain with the default protocol and prepares it
    to accept new connection requests on the address [addr] with [bind] and [listen]. We close the
    socket in case of an error. *)
val install_tcp_server_socket : Unix.sockaddr -> Unix.file_descr

(** Creates a socket with [install_tcp_server_socket] and enters an infinite loop. At each iteration
    of the loop it waits for a connection request with [accept] and treats it with the function
    [treat_connection]. We restart the [accept] call if it is interrupted. We also ignore the signal
    [sigpipe] so that an unexpected disconnection raise an [EPIPE] exception that can be caught by
    [treat_connection] rather than killing the server. Note that in any case it is
    [treat_connection]'s duty to close the [client] descriptor at the end of the connection.

    The function [treat_connection] is also given the descriptor of the server so that if it [fork]s
    or [double_fork]s it can be closed by the child. *)
val tcp_server
  :  (Unix.file_descr -> Unix.file_descr * Unix.sockaddr -> unit)
  -> Unix.sockaddr
  -> unit

val sequential_treatment : 'a -> ('b -> 'c) -> 'b -> 'c

(** Delegates a service to a child process. The child process handles the connection and the parent
    process immediately retries to [accept].

    Note that it is essential that the parent closes [client_sock], otherwise the close made by the
    child will not terminate the connection (besides the parent would also quickly run out of
    descriptors). The descriptor is also closed if the fork fails, for the server may eventually
    decide the error is not fatal and continue to operate.

    Similarly, the child immediately closes the [server] descriptor on which the connection request
    was received. First, it does not need it. Second, the server may stop accepting new connections
    before the child has terminated. The call to [exit 0] is important since it ensures that the
    child terminates after the execution of the service and that it does not start to execute the
    server loop. *)
val fork_treatment
  :  Unix.file_descr
  -> (Unix.file_descr * 'a -> unit)
  -> Unix.file_descr * 'a
  -> unit

(** Double forks a service so that children can recovered *)
val double_fork_treatment
  :  Unix.file_descr
  -> (Unix.file_descr * 'a -> unit)
  -> Unix.file_descr * 'a
  -> unit

val co_treatment
  :  Unix.file_descr
  -> (Unix.file_descr * 'a -> 'b)
  -> Unix.file_descr * 'a
  -> unit

(** Hold a lock temporarily during a function call. *)
val run_with_lock : Mutex.t -> ('a -> 'b) -> 'a -> 'b

(** The [tcp_farm_server] function behaves like tcp_server but takes an additional argument which is
    the number of threads to start, each of which will become a server at the same address. The
    advantage of a pool of threads is to reduce the time to handle each connection by eliminating
    the cost of creating a thread for it, since they are created once and for all. *)
val tcp_farm_server
  :  int
  -> (Unix.file_descr -> Unix.file_descr * Unix.sockaddr -> unit)
  -> Unix.sockaddr
  -> unit
