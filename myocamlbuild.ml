open Ocamlbuild_plugin;;

dispatch begin function
  | After_rules ->
    ocaml_lib "cats"
  | _ -> ()
end


