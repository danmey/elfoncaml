open Ocamlbuild_plugin;;

dispatch begin function
  | After_rules ->
    ocaml_lib "src/elf"
  | _ -> ()
end;;

rule "gcc: %.c -> %.o"
  ~tags:["gcc"]
  ~prod:"%.o"
  ~dep:"%.c"
  begin fun env _ ->
    Cmd(S[Sh "gcc"; Sh "-Wall"; A"-g";A"-lasmrun"; A"-c"; P(env "%.c"); A"-o";P (env "%.o");])
  end;;

let link opts = List.flatten (List.map (fun opt -> [A"-cclib"; A opt]) opts);;

flag ["library";"ocaml"] (S(link ["-lelf";"-lbsd";"src/bindings.o"]@[A"-cc"; A"-g";A"-g";]));;
dep ["library";"ocaml"] ["src/bindings.o"]


