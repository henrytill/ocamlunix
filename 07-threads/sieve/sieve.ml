let input_int = input_binary_int
let output_int = output_binary_int

let generate k output =
  let rec gen m =
    output_int output m;
    if m < k then gen (m + 1)
  in
  gen 2
;;

let print_prime n =
  print_int n;
  print_char '\n'
;;

let read_first_primes input count =
  let rec read_primes first_primes count =
    if count <= 0
    then first_primes
    else (
      let n = input_int input in
      if List.exists (fun m -> n mod m = 0) first_primes
      then read_primes first_primes count
      else (
        print_prime n;
        read_primes (n :: first_primes) (count - 1)))
  in
  Misc.try_finalize (read_primes []) count flush Stdlib.stdout
;;

let rec filter input =
  try
    let first_primes = read_first_primes input 1000 in
    let pipe_out, pipe_in = Unix.pipe () in
    let p = Thread.create filter (Unix.in_channel_of_descr pipe_out) in
    let output = Unix.out_channel_of_descr pipe_in in
    try
      while true do
        let n = input_int input in
        if List.exists (fun m -> n mod m = 0) first_primes
        then ()
        else output_int output n
      done
    with
    | End_of_file ->
      close_out output;
      Thread.join p
  with
  | End_of_file -> ()
;;

let sieve () =
  let len =
    try int_of_string Sys.argv.(1) with
    | _ -> max_int
  in
  let pipe_out, pipe_in = Unix.pipe () in
  let k = Thread.create filter (Unix.in_channel_of_descr pipe_out) in
  let output = Unix.out_channel_of_descr pipe_in in
  generate len output;
  close_out output;
  Thread.join k
;;

let () = Unix.handle_unix_error sieve ()
