let interval_timer_status_t =
  let module M = struct
    type t = Unix.interval_timer_status

    let equal x y = x = y

    let pp ppf x =
      Format.pp_print_string
        ppf
        (let open Unix in
         let { it_interval = i; it_value = v } = x in
         Printf.sprintf "{ it_interval = %f; it_value = %f }" i v)
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let get_timer () =
  let timer = Unix.ITIMER_REAL in
  Alcotest.check
    interval_timer_status_t
    "same timer"
    (Unix.getitimer timer)
    (Timer.get_timer (Timer.new_timer timer (fun () -> ())))

let test_set = [ ("Timer.get_timer", `Quick, get_timer) ]
let () = Alcotest.run "Timer" [ ("test_set", test_set) ]
