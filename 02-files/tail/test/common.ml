(* Signature for test modules  *)

module type Tests = sig

  val test_set : (string * [> `Quick ] * (unit -> unit)) list

end
