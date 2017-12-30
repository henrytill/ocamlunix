open Ocamlbuild_plugin

(*
 * https://stackoverflow.com/questions/19644451/how-to-make-ocamlbuild-compile-and-link-a-c-file-into-an-ocaml-project
 * https://ocaml.org/learn/tutorials/ocamlbuild/Ocamlbuild_example_with_C_stubs.html
 *)

let headers = ["lib/ex.h"]
let static  = true

let () =
  dispatch begin
    function
    | After_rules ->
        flag ["link"; "library"; "ocaml"; "byte";   "use_ex"] (S [A "-dllib"; A "-lex"; A "-cclib"; A "-lex"]);
        flag ["link"; "library"; "ocaml"; "native"; "use_ex"] (S [A "-cclib"; A "-lex"]);
        if static then flag ["link"; "ocaml"; "byte"] (A "-custom");
        dep ["link"; "ocaml"; "byte";   "use_ex"] ["lib/dllex.so"; "lib/libex.a"];
        dep ["link"; "ocaml"; "native"; "use_ex"] ["lib/libex.a"];
        dep ["compile"; "c"] headers;
    | _ ->  ()
  end
