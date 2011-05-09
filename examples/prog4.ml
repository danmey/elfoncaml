let _ =
  if Array.length Sys.argv != 2 then
    Printf.printf "usage: %s <file-name>\n" Sys.argv.(0)
  else
    match Elf.version `CURRENT with
      | `NONE -> print_endline "ELF library initialization failed"
      | _ -> 
        begin 
          let fd = Unix.openfile Sys.argv.(1) [Unix.O_RDONLY] 0 in
          let Some elf = Elf.begins fd Elf.C_READ None in
          let kind = Elf.kind elf in
          match kind with
            | Elf.K_ELF ->
              let ehdr = Elf.Elf32Header.get (Elf.elf32_header elf) in
              print_endline (Elf.Elf32Header.to_string ehdr);
              let str_section = Elf.str_section elf in
              (* let sections = Elf.sections elf in *)
              let sections = [] in
              List.iter 
                (fun sec -> 
                  Printf.printf "Section %-4.4d %s\n" 
                    (Elf.section_index sec) 
                    (Elf.section_name elf sec str_section)) sections;
            (* Printf.printf ".shstrab: size = %d\n" (Elf.section_size str_section); *)
              let data = Elf.section_data str_section in
              for i = 0 to (Elf.SectionBuffer.dim data) - 1 do
                Printf.printf "%c" (char_of_int (Elf.SectionBuffer.get data i));
                print_char (if i mod 16 = 0 then '\n' else ' ');
              done
            | _ -> failwith (Printf.sprintf "%s is not an ELF object." Sys.argv.(1))
        end

            
