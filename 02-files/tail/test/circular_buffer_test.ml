(* Signature for test modules  *)

module type N_element_buffer = sig

  type t

  val buffer : t Circular_buffer.t

  val test_set : (string * [> `Quick ] * (unit -> unit)) list

end

let check_char = Alcotest.(check char)

(* Two-element buffer *)

module Two_element_buffer : N_element_buffer = struct

  type t = char

  let buffer = Circular_buffer.create 2 '0' (*     *)

  let push_push_push_pop () =
    let open Circular_buffer in
    push buffer '1';                        (* 1   *)
    push buffer '2';                        (* 1 2 *)
    push buffer '3';                        (* 2 3 *)
    check_char "same char" '2' (pop buffer) (* 3   *)

  let push_pop () =
    let open Circular_buffer in
    push buffer '4';                        (* 3 4 *)
    check_char "same char" '3' (pop buffer) (* 4   *)

  let pop () =
    let open Circular_buffer in
    check_char "same char" '4' (pop buffer) (*     *)

  let pop_empty_01 () =
    Alcotest.check_raises
      "empty buffer"
      Not_found
      (fun () -> ignore (Circular_buffer.pop buffer))

  let pop_empty_02 () =
    Alcotest.check_raises
      "empty buffer"
      Not_found
      (fun () -> ignore (Circular_buffer.pop buffer))

  let test_set = [
    "push_push_push_pop", `Quick, push_push_push_pop;
    "push_pop",           `Quick, push_pop;
    "pop",                `Quick, pop;
    "pop_empty_01",       `Quick, pop_empty_01;
    "pop_empty_02",       `Quick, pop_empty_02;
  ]

end

(* Five-element buffer *)

module Five_element_buffer : N_element_buffer = struct

  type t = char

  let buffer = Circular_buffer.create 5 '0'  (*           *)

  let push_push_push_push_push_push_pop () =
    let open Circular_buffer in
    push buffer '1';                         (* 1         *)
    push buffer '2';                         (* 1 2       *)
    push buffer '3';                         (* 1 2 3     *)
    push buffer '4';                         (* 1 2 3 4   *)
    push buffer '5';                         (* 1 2 3 4 5 *)
    push buffer '6';                         (* 2 3 4 5 6 *)
    check_char "same char" '2' (pop buffer)  (* 3 4 5 6   *)

  let pop_01 () =
    let open Circular_buffer in
    check_char "same char" '3' (pop buffer)  (* 4 5 6     *)

  let push_push_push_pop () =
    let open Circular_buffer in
    push buffer '7';                         (* 4 5 6 7   *)
    push buffer '8';                         (* 4 5 6 7 8 *)
    push buffer '9';                         (* 5 6 7 8 9 *)
    check_char "same char" '5' (pop buffer)  (* 6 7 8 9   *)

  let pop_pop () =
    let open Circular_buffer in
    check_char "same char" '6' (pop buffer); (* 7 8 9     *)
    check_char "same char" '7' (pop buffer)  (* 8 9       *)

  let push_push_push_push_pop () =
    let open Circular_buffer in
    push buffer '1';                         (* 8 9 1     *)
    push buffer '2';                         (* 8 9 1 2   *)
    push buffer '3';                         (* 8 9 1 2 3 *)
    check_char "same char" '8' (pop buffer)  (* 9 1 2 3   *)

  let pop_02 () =
    let open Circular_buffer in
    check_char "same char" '9' (pop buffer)  (* 1 2 3     *)

  let push_pop () =
    let open Circular_buffer in
    push buffer '4';                         (* 1 2 3 4   *)
    check_char "same char" '1' (pop buffer)  (* 2 3 4     *)

  let pop_pop_pop () =
    let open Circular_buffer in
    check_char "same char" '2' (pop buffer); (* 3 4       *)
    check_char "same char" '3' (pop buffer); (* 4         *)
    check_char "same char" '4' (pop buffer)  (*           *)

  let pop_empty_01 () =
    Alcotest.check_raises
      "empty buffer"
      Not_found
      (fun () -> ignore (Circular_buffer.pop buffer))

  let pop_empty_02 () =
    Alcotest.check_raises
      "empty buffer"
      Not_found
      (fun () -> ignore (Circular_buffer.pop buffer))

  let test_set = [
    "push_push_push_push_push_push_pop", `Quick, push_push_push_push_push_push_pop;
    "pop_01",                            `Quick, pop_01;
    "push_push_push_pop",                `Quick, push_push_push_pop;
    "pop_pop",                           `Quick, pop_pop;
    "push_push_push_push_pop",           `Quick, push_push_push_push_pop;
    "pop_02",                            `Quick, pop_02;
    "push_pop",                          `Quick, push_pop;
    "pop_pop_pop",                       `Quick, pop_pop_pop;
    "pop_empty_01",                      `Quick, pop_empty_01;
    "pop_empty_02",                      `Quick, pop_empty_02;
  ]

end
