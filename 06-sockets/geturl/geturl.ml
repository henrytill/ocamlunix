let geturl () =
  let len = Array.length Sys.argv in
  if len < 2 then
    Url.error "Usage: " (Sys.argv.(0) ^ " [ proxy [:<port>] ] <url>")
  else
    let proxy, url = if len > 2 then (Some Sys.argv.(1), Sys.argv.(2)) else (None, Sys.argv.(1)) in
    Url.get_url proxy url Unix.stdout

let () = Unix.handle_unix_error (Url.handle_error geturl) ()
