open Unix

let input_int = input_binary_int
let output_int = output_binary_int

let generate k output =
  let rec gen m =
    output_int output m;
    if m < k then gen (m + 1)
  in
  gen 2

let print_prime n =
  print_int n;
  print_char '\n'

let read_first_primes input count =
  let rec read_primes first_primes count =
    if count <= 0 then
      first_primes
    else
      let n = input_int input in
      if List.exists (fun m -> n mod m = 0) first_primes then
        read_primes first_primes count
      else
        begin
          print_prime n;
          read_primes (n :: first_primes) (count - 1)
        end
  in
  Misc.try_finalize
    (read_primes []) count
    flush Stdlib.stdout

let rec filter input =
  try
    let first_primes = read_first_primes input 1000 in
    let (pipe_out, pipe_in) = pipe () in
    match fork () with
    | 0 ->
        close pipe_in;
        filter (in_channel_of_descr pipe_out)
    | p ->
        close pipe_out;
        let output = out_channel_of_descr pipe_in in
        try
          while true do
            let n = input_int input in
            if List.exists (fun m -> n mod m = 0) first_primes then
              ()
            else
              output_int output n
          done
        with End_of_file ->
          close_out output;
          ignore (waitpid [] p)
  with End_of_file -> ()

let sieve () =
  let len = try int_of_string Sys.argv.(1) with _ -> max_int in
  let (pipe_out, pipe_in) = pipe () in
  match fork () with
  | 0 ->
      close pipe_in;
      filter (in_channel_of_descr pipe_out)
  | p ->
      close pipe_out;
      let output = out_channel_of_descr pipe_in in
      generate len output;
      close_out output;
      ignore (waitpid [] p)

let () =
  handle_unix_error sieve ()
