
let _ =
  if Array.length Sys.argv != 2 then
    Printf.printf "usage: %s <file-name>\n" Sys.argv.(0)
  else
    match Elf.version `CURRENT with
      | `NONE -> print_endline "ELF library initialization failed"
      | _ -> 
        begin 
          let open Elf in
              let fd = Unix.openfile Sys.argv.(1) [Unix.O_WRONLY;Unix.O_CREAT;] 0o777 in
              let elf = begins fd C_WRITE None in
              let ehdr = Elf32Header.create elf in
              Elf32Header.update { ehdr with
                e_ident =
                  { ehdr.e_ident with adata = Elf.ELFDATA2MSB };
                e_machine = `PPC;
                e_type = ET_EXEC; };
              let phdr = ProgramHeader.create elf in
              let scn = create_section elf in
              let data = SectionData.create scn in
              SectionData.update { data with
                d_align = 4L;
                d_off = 0L;
                (* d_buf = Some (Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout [|0x01234567l;0x89abcdefl;0xdeadc0del|]); *)
                d_type = T_WORD;
                d_size = 12L;
                d_version = EV_CURRENT;
              };
              let shdr = SectionHeader.from_section scn in
              SectionHeader.update { shdr with
                sh_name = 1l;
                sh_type = SHT_HASH;
                sh_flags = [SHF_ALLOC];
                sh_entsize = 0l;
              };
              
              let scn = create_section elf in
              let data = SectionData.create scn in
              let c = int_of_char in
              SectionData.update { data with
                d_align = 1L;
                d_buf = Some (Bigarray.Array1.of_array Bigarray.int8_unsigned Bigarray.c_layout
                                (Array.map c [|
                                  char_of_int 0;
                                  '.'; 'f'; 'o'; 'o'; char_of_int 0;
                                  '.'; 's'; 'h'; 's';'t'; char_of_int 0;
                                  'r'; 't'; 'a'; 'b'; char_of_int 0;
                                |]));
                d_off = 0L;
                d_size = 17L;
                d_type = T_BYTE;
                d_version = EV_CURRENT;
              };
              SectionHeader.update { (SectionHeader.from_section scn) with
                sh_name = 6l;
                sh_type = SHT_STRTAB;
                sh_flags = [SHF_STRINGS;SHF_ALLOC];
                sh_entsize = 0l;
              };
              set_str_section_index elf (section_index scn);
              update elf C_NULL;
              let ehdr = Elf32Header.get ehdr.ehdr in
              ProgramHeader.update { phdr with
                p_type = PT_PHDR;
                p_offset = ehdr.e_phoff;
                p_filesz = fsize T_PHDR 1l EV_CURRENT;
              };
              program_header_flags elf C_SET F_DIRTY;
              update elf C_WRITE;
              ends elf;
              Unix.close fd;
              exit 0
        end
