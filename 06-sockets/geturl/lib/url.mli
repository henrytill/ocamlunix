exception Error of string

val error : string -> string -> 'a

val handle_error : ('a -> 'b) -> 'a -> 'b

type regexp = { regexp : Str.regexp;
                fields : (int * string option) list; }

val regexp_match : regexp -> string -> string list option

val parse_host : string -> string * int

val parse_url : string -> (string * int) * string

val send_get : string -> Unix.file_descr -> unit

val get_url : string option -> string -> Unix.file_descr -> unit
