let _ =
  if Array.length Sys.argv != 2 then
    Printf.printf "usage: %s <file-name>\n" Sys.argv.(0)
  else
    match Elf.version `CURRENT with
      | `NONE -> print_endline "ELF library initialization failed"
      | _ -> 
        begin 
          let fd = Unix.openfile Sys.argv.(1) [Unix.O_RDONLY] 0 in
          let elf = Elf.begins fd Elf.C_READ None in
          let kind = Elf.kind elf in
          let ss = Elf.str_section elf in
          let sections = Elf.sections elf in
          List.iter 
            (fun sec -> print_endline (Elf.section_name elf sec ss)) sections
        end

            
