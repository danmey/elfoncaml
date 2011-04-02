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
          (* let kind = Elf.kind elf in *)
          let str_section = Elf.str_section elf in
          let sections = Elf.sections elf in
          List.iter 
            (fun sec -> 
              Printf.printf "Section %-4.4d %s\n" 
                (Elf.section_index sec) 
                (Elf.section_name elf sec str_section)) sections;
            (* Printf.printf ".shstrab: size = %d\n" (Elf.section_size str_section); *)
            let data = Elf.section_data str_section in
            for i = 0 to (Elf.SectionData.dim data) - 1 do
              Printf.printf "%c" (char_of_int (Elf.SectionData.get data i));
              print_char (if i mod 16 = 0 then '\n' else ' ');
            done
        end

            
