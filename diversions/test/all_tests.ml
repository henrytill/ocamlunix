let circular_buffer () =
  Alcotest.run
    "Circular_buffer"
    ["Two_element_buffer.test_set",  Circular_buffer_test.Two_element_buffer.test_set;
     "Five_element_buffer.test_set", Circular_buffer_test.Five_element_buffer.test_set;]

let () =
  circular_buffer ()
