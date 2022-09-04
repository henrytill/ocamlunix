let swap arr i j =
  let tmp = Array.unsafe_get arr i in
  Array.unsafe_set arr i (Array.unsafe_get arr j);
  Array.unsafe_set arr j tmp

let qsort cmp arr =
  let rec qsort lo hi =
    if hi - lo > 0
    then begin
      let mid = (lo + hi) lsr 1 in
      if cmp arr.(mid) arr.(lo) then swap arr mid lo;
      if cmp arr.(hi) arr.(mid)
      then begin
        swap arr mid hi;
        if cmp arr.(mid) arr.(lo) then swap arr mid lo
      end;
      let pivot = arr.(mid) in
      let i = ref (lo + 1)
      and j = ref (hi - 1) in
      while !i < !j do
        while not (cmp pivot arr.(!i)) do
          incr i
        done;
        while not (cmp arr.(!j) pivot) do
          decr j
        done;
        if !i < !j then swap arr !i !j
      done;
      let u = Thread.create (qsort lo) (!i - 1) in
      let v = Thread.create (qsort (!i + 1)) hi in
      Thread.join u;
      Thread.join v
    end
  in
  qsort 0 (Array.length arr - 1)

let () =
  let arr = [| 5; 1; 9; 4; 8; 2; 7; 3; 6 |] in
  qsort ( < ) arr;
  Array.iter (fun x -> print_string (string_of_int x ^ " ")) arr;
  print_newline ()
