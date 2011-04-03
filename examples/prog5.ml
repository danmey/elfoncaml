let _ =
  if Array.length Sys.argv != 2 then
    Printf.printf "usage: %s <file-name>\n" Sys.argv.(0)
  else
    match Elf.version `CURRENT with
      | `NONE -> print_endline "ELF library initialization failed"
      | _ -> 
        begin 
          let open Elf in
              let fd = Unix.openfile Sys.argv.(1) [Unix.O_WRONLY;Unix.O_CREAT;] 0777 in
              let elf = begins fd C_WRITE None in
              let ehdr = Elf32Header.create elf in
              let ehdr =
                { ehdr with
                  e_ident =
                    { ehdr.e_ident with adata = Elf.ELFDATA2MSB };
                  e_machine = `PPC;
                  e_type = ET_EXEC; }
              in
              Elf32Header.update ehdr;
              let phdr = ProgramHeader.create elf in
              let scn = create_section elf in
              let data = SectionData.create scn in
              SectionData.update data;
              
          ()
          (* match kind with *)
          (*   | Elf.K_ELF -> *)
          (*     let str_section = Elf.str_section elf in *)
          (*     let sections = Elf.sections elf in *)
          (*     List.iter  *)
          (*       (fun sec ->  *)
          (*         Printf.printf "Section %-4.4d %s\n"  *)
          (*           (Elf.section_index sec)  *)
          (*           (Elf.section_name elf sec str_section)) sections; *)
          (*   (\* Printf.printf ".shstrab: size = %d\n" (Elf.section_size str_section); *\) *)
          (*     let data = Elf.section_data str_section in *)
          (*     for i = 0 to (Elf.SectionData.dim data) - 1 do *)
          (*       Printf.printf "%c" (char_of_int (Elf.SectionData.get data i)); *)
          (*       print_char (if i mod 16 = 0 then '\n' else ' '); *)
          (*     done *)
          (*   | _ -> failwith (Printf.sprintf "%s is not an ELF object." Sys.argv.(1)) *)
        end
