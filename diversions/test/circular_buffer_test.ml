let check_char = Alcotest.(check char)

(* Two-element buffer *)

module Two_element_buffer : Common.Tests = struct
  let buffer = Circular_buffer.create 2 '0'

  let push_push_push_pop () =
    let open Circular_buffer in
    push buffer '1';
    push buffer '2';
    push buffer '3';
    check_char "same char" '2' (pop buffer)

  let push_pop () =
    let open Circular_buffer in
    push buffer '4';
    check_char "same char" '3' (pop buffer)

  let pop () =
    let open Circular_buffer in
    check_char "same char" '4' (pop buffer)

  let pop_empty_01 () =
    Alcotest.check_raises "empty buffer" Not_found (fun () -> ignore (Circular_buffer.pop buffer))

  let pop_empty_02 () =
    Alcotest.check_raises "empty buffer" Not_found (fun () -> ignore (Circular_buffer.pop buffer))

  let test_set =
    [ ("push_push_push_pop", `Quick, push_push_push_pop)
    ; ("push_pop", `Quick, push_pop)
    ; ("pop", `Quick, pop)
    ; ("pop_empty_01", `Quick, pop_empty_01)
    ; ("pop_empty_02", `Quick, pop_empty_02)
    ]
end

(* Five-element buffer *)

module Five_element_buffer : Common.Tests = struct
  let buffer = Circular_buffer.create 5 '0'

  let push_push_push_push_push_push_pop () =
    let open Circular_buffer in
    push buffer '1';
    push buffer '2';
    push buffer '3';
    push buffer '4';
    push buffer '5';
    push buffer '6';
    check_char "same char" '2' (pop buffer)

  let pop_01 () =
    let open Circular_buffer in
    check_char "same char" '3' (pop buffer)

  let push_push_push_pop () =
    let open Circular_buffer in
    push buffer '7';
    push buffer '8';
    push buffer '9';
    check_char "same char" '5' (pop buffer)

  let pop_pop () =
    let open Circular_buffer in
    check_char "same char" '6' (pop buffer);
    check_char "same char" '7' (pop buffer)

  let push_push_push_push_pop () =
    let open Circular_buffer in
    push buffer '1';
    push buffer '2';
    push buffer '3';
    check_char "same char" '8' (pop buffer)

  let pop_02 () =
    let open Circular_buffer in
    check_char "same char" '9' (pop buffer)

  let push_pop () =
    let open Circular_buffer in
    push buffer '4';
    check_char "same char" '1' (pop buffer)

  let pop_pop_pop () =
    let open Circular_buffer in
    check_char "same char" '2' (pop buffer);
    check_char "same char" '3' (pop buffer);
    check_char "same char" '4' (pop buffer)

  let pop_empty_01 () =
    Alcotest.check_raises "empty buffer" Not_found (fun () -> ignore (Circular_buffer.pop buffer))

  let pop_empty_02 () =
    Alcotest.check_raises "empty buffer" Not_found (fun () -> ignore (Circular_buffer.pop buffer))

  let test_set =
    [ ("push_push_push_push_push_push_pop", `Quick, push_push_push_push_push_push_pop)
    ; ("pop_01", `Quick, pop_01)
    ; ("push_push_push_pop", `Quick, push_push_push_pop)
    ; ("pop_pop", `Quick, pop_pop)
    ; ("push_push_push_push_pop", `Quick, push_push_push_push_pop)
    ; ("pop_02", `Quick, pop_02)
    ; ("push_pop", `Quick, push_pop)
    ; ("pop_pop_pop", `Quick, pop_pop_pop)
    ; ("pop_empty_01", `Quick, pop_empty_01)
    ; ("pop_empty_02", `Quick, pop_empty_02)
    ]
end
