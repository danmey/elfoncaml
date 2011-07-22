
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
              match begins fd C_WRITE None with
                | None -> failwith "begins"
                | Some elf ->
                  let ehdr = newehdr elf in
                  let ehdr = Elf32_Ehdr.create ehdr in
                  Elf32_Ehdr.update { ehdr with
                    e_ident =
                      { ehdr.e_ident with adata = Elf.ELFDATA2MSB };
                    e_machine = `PPC;
                    e_type = ET_EXEC; };
                  let phdr = newphdr elf 1 in
                  let phdr = Elf32_Phdr.create phdr in
                  Elf32_Phdr.update phdr;
                  match newscn elf with
                    | None -> failwith "newscn"
                    | Some scn ->
                      let data = Elf_Data.create (newdata scn) in
                      Elf_Data.update { data with
                        d_align = 4L;
                        d_off = 0L;
                    (* d_buf = Some (Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout [|0x01234567l;0x89abcdefl;0xdeadc0del|]); *)
                        d_type = T_WORD;
                        d_size = 12L;
                        d_version = EV_CURRENT;
                      };
                      let Some shdr = getshdr scn in
                      let shdr = Elf32_Shdr.create shdr in
                      Elf32_Shdr.update { shdr with
                        sh_name = 1l;
                        sh_type = SHT_HASH;
                        sh_flags = [SHF_ALLOC];
                        sh_entsize = 0l;
                      };
                      
                  match newscn elf with
                    | None -> failwith "newscn"
                    | Some scn ->
                      let data = Elf_Data.create (newdata scn) in
                      let c = int_of_char in
                      Elf_Data.update { data with
                        d_align = 1L;
                    (* d_buf = Some (Bigarray.Array1.of_array Bigarray.int8_unsigned Bigarray.c_layout *)
                    (*                 (Array.map c [| *)
                    (*                   char_of_int 0; *)
                    (*                   '.'; 'f'; 'o'; 'o'; char_of_int 0; *)
                    (*                   '.'; 's'; 'h'; 's';'t'; char_of_int 0; *)
                    (*                   'r'; 't'; 'a'; 'b'; char_of_int 0; *)
                    (*                 |])); *)
                        d_off = 0L;
                        d_size = 17L;
                        d_type = T_BYTE;
                        d_version = EV_CURRENT;
                      };
                      let Some shdr = getshdr scn in
                      let shdr = Elf32_Shdr.create shdr in
                      Elf32_Shdr.update { shdr with
                        sh_name = 6l;
                        sh_type = SHT_STRTAB;
                        (* sh_flags = [SHF_STRINGS;SHF_ALLOC]; *)
                        sh_entsize = 0l;
                      };
                      update_shstrndx elf (ndxscn scn);
                      ignore (update elf C_NULL);
                      Elf32_Phdr.update { phdr with
                        p_type = PT_PHDR;
                        p_offset = ehdr.e_phoff;
                        p_filesz = fsize T_PHDR 1l EV_CURRENT;
                      };
                      flagphdr elf C_SET F_DIRTY;
                      ignore (update elf C_WRITE);
                      ends elf;
                      Unix.close fd;
                      exit 0
        end
