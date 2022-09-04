let lines () =
  try
    Alcotest.run ~and_exit:false "lines" [ ("New_lines.test_set", Lines_test.New_lines.test_set) ]
  with
  | Alcotest.Test_error -> Printf.printf "Continue!!\n%!"

let tail () = Alcotest.run ~and_exit:true "tail" [ ("Tail.test_set", Tail_test.Tail.test_set) ]

let () =
  lines ();
  tail ()
