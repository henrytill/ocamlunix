let circular_buffer () =
  try
    Alcotest.run
      ~and_exit:false
      "Circular_buffer"
      [ "Two_element_buffer.test_set",  Circular_buffer_test.Two_element_buffer.test_set;
        "Five_element_buffer.test_set", Circular_buffer_test.Five_element_buffer.test_set; ]
  with
    Alcotest.Test_error -> Printf.printf "Continue!!\n%!"

let tail () =
  Alcotest.run
    ~and_exit:true
    "lines"
    [ "Lines.test_set",     Lines_test.Lines.test_set;
      "Lines_alt.test_set", Lines_test.Lines_alt.test_set; ]

let () =
  circular_buffer ();
  tail ()
