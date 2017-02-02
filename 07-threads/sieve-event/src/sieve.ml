type 'a buffered =
  { c         : 'a Queue.t Event.channel;
    mutable q : 'a Queue.t;
    size      : int;
  }

let pipe () =
  let c = Event.new_channel () in
  c, c

let size = 1024

let out_channel_of_descr chan =
  { c    = chan;
    q    = Queue.create ();
    size = size;
  }

let in_channel_of_descr =
  out_channel_of_descr

let input_int chan =
  if Queue.length chan.q = 0 then
    begin
      let q = Event.sync (Event.receive chan.c) in
      if Queue.length q > 0 then chan.q <- q else raise End_of_file
    end;
  Queue.take chan.q

let flush_out chan =
  if Queue.length chan.q > 0 then Event.sync (Event.send chan.c chan.q);
  chan.q <- Queue.create ()

let output_int chan x =
  if Queue.length chan.q = size then flush_out chan;
  Queue.add x chan.q

let close_out chan =
  flush_out chan;
  Event.sync (Event.send chan.c chan.q)

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
    flush Pervasives.stdout

let rec filter input =
  try
    let first_primes = read_first_primes input 1000 in
    let (pipe_out, pipe_in) = pipe () in
    let p = Thread.create filter (in_channel_of_descr pipe_out) in
    let output = out_channel_of_descr pipe_in in
    try
      while true do
        let n = input_int input in
        if List.exists (fun m -> n mod m = 0) first_primes then
          ()
        else
          output_int output n
      done;
    with End_of_file ->
      close_out output;
      Thread.join p
  with End_of_file -> ()

let sieve () =
  let len = try int_of_string Sys.argv.(1) with _ -> max_int in
  let (pipe_out, pipe_in) = pipe () in
  let k = Thread.create filter (in_channel_of_descr pipe_out) in
  let output = out_channel_of_descr pipe_in in
  generate len output;
  close_out output;
  Thread.join k

let () =
  Unix.handle_unix_error sieve ()
