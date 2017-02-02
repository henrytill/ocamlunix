open Unix
open Url

let geturl () =
  let len = Array.length Sys.argv in
  if len < 2 then
    error "Usage: " (Sys.argv.(0) ^ " [ proxy [:<port>] ] <url>")
  else
    let proxy, url =
      if len > 2 then
        Some Sys.argv.(1), Sys.argv.(2)
      else
        None, Sys.argv.(1)
    in
    get_url proxy url stdout

let () =
  handle_unix_error (handle_error geturl) ()
