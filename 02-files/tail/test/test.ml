let buf = Circular_buffer.create 2 '0'

(* test *)
let one_two_three () =
  (*     *)
  Circular_buffer.push buf '1';
  (* 1   *)
  Circular_buffer.push buf '2';
  (* 1 2 *)
  Circular_buffer.push buf '3';
  (* 2 3 *)
  Alcotest.(check char) "same char" '2' (Circular_buffer.pop buf)

let three() =
  (* 3   *)
  Circular_buffer.push buf '4';
  (* 3 4 *)
  Alcotest.(check char) "same char" '3' (Circular_buffer.pop buf)

let four () =
  (* 4   *)
  Alcotest.(check char) "same char" '4' (Circular_buffer.pop buf)

(* let exceptor () = *)
(*   (\*         *\) *)
(*   assert (try ignore (Circular_buffer.pop buf); false with Not_found -> true); *)
(*   assert (try ignore (Circular_buffer.pop buf); false with Not_found -> true); *)

let test_set = [
  "one two three" , `Quick, one_two_three;
  "three", `Quick, three ;
  "four", `Quick, four ;
]

(* Run it *)
let () =
  Alcotest.run "Circular_buffer" ["test_set", test_set;]
