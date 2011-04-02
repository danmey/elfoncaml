open Ocamlbuild_plugin;;

rule "gcc: %.c -> %.o"
  ~tags:["gcc"]
  ~prod:"%.o"
  ~dep:"%.c"
  begin fun env _ ->
    Cmd(S[Sh "gcc"; Sh "-Wall"; A"-c"; P(env "%.c"); A"-o";P (env "%.o");])
  end;;

let link opts = List.flatten (List.map (fun opt -> [A"-cclib"; A opt]) opts);;

flag ["link"; "ocaml"] (S(link ["-lelf";"-lbsd";]));;
dep ["link"; "ocaml"; "gcc"] ["src/bindings.o";]
