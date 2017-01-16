let () =
  let xs = Tail.main Sys.argv.(1) (int_of_string Sys.argv.(2)) in
  List.iter (fun line -> print_string line) xs
