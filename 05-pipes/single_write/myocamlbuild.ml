open Ocamlbuild_plugin;;

(*
 * http://stackoverflow.com/questions/19644451/how-to-make-ocamlbuild-compile-and-link-a-c-file-into-an-ocaml-project#19662145
 * http://l-lang.blogspot.de/2012/08/incorporating-c-code-in-ocaml-project.html
 *)

dispatch (function
    | After_rules -> pdep ["link"] "linkdep" (fun param -> [param])
    | _           -> ())
