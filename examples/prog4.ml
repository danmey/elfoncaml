let _ =
  if Array.length Sys.argv != 2 then
    Printf.printf "usage: %s <file-name>\n" Sys.argv.(0)
  else
    match Elf.version `CURRENT with
      | `NONE -> print_endline "ELF library initialization failed"
      | _ -> 
        begin 
          let fd = Unix.openfile Sys.argv.(1) [Unix.O_RDONLY] 0 in
          let elf = Elf.begins fd Elf.C_READ in
          let kind = 
            match Elf.kind elf with
              | Elf.K_NONE -> "none"
              | Elf.K_AR -> "ar"
              | Elf.K_COFF -> "coff"
              | Elf.K_ELF -> "elf"
              | Elf.K_NUM -> "num" in
          print_endline kind
        end
            
