open Elf
open Elf32
let _ =
  if Array.length Sys.argv != 2 then
    Printf.printf "usage: %s <file-name>\n" Sys.argv.(0)
  else
    match Elf.version `CURRENT with
      | `NONE -> print_endline "ELF library initialization failed"
      | _ -> 
        begin 
              let fd = Unix.openfile Sys.argv.(1) [Unix.O_WRONLY;Unix.O_CREAT;] 0o777 in
              match begins fd C_WRITE None with
                | None -> failwith "begins"
                | Some elf ->
                  let ehdr = Elf32.newehdr elf in
                  let ehdr = Elf32.Ehdr.create ehdr in
                  Ehdr.update { ehdr with
                    Ehdr.e_ident =
                      { ehdr.Ehdr.e_ident with adata = Elf.ELFDATA2MSB };
                    Ehdr.e_machine = `PPC;
                    Ehdr.e_type = ET_EXEC; };
                  let phdr = newphdr elf 1 in
                  let phdr = Phdr.create phdr in
                  Phdr.update phdr;
                  match newscn elf with
                    | None -> failwith "newscn"
                    | Some scn ->
                      let data = Data.create (newdata scn) in
                      Data.update { data with
                        Data.d_align = 4L;
                        Data.d_off = 0L;
                    (* d_buf = Some (Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout [|0x01234567l;0x89abcdefl;0xdeadc0del|]); *)
                        Data.d_type = T_WORD;
                        Data.d_size = 12L;
                        Data.d_version = EV_CURRENT;
                      };
                      let Some shdr = getshdr scn in
                      let shdr = Shdr.create shdr in
                      Shdr.update { shdr with
                        Shdr.sh_name = 1l;
                        Shdr.sh_type = SHT_HASH;
                        Shdr.sh_flags = [SHF_ALLOC];
                        Shdr.sh_entsize = 0l;
                      };
                      
                  match newscn elf with
                    | None -> failwith "newscn"
                    | Some scn ->
                      let data = Data.create (newdata scn) in
                      let c = int_of_char in
                      Data.update { data with
                        Data.d_align = 1L;
                    (* d_buf = Some (Bigarray.Array1.of_array Bigarray.int8_unsigned Bigarray.c_layout *)
                    (*                 (Array.map c [| *)
                    (*                   char_of_int 0; *)
                    (*                   '.'; 'f'; 'o'; 'o'; char_of_int 0; *)
                    (*                   '.'; 's'; 'h'; 's';'t'; char_of_int 0; *)
                    (*                   'r'; 't'; 'a'; 'b'; char_of_int 0; *)
                    (*                 |])); *)
                        Data.d_off = 0L;
                        Data.d_size = 17L;
                        Data.d_type = T_BYTE;
                        Data.d_version = EV_CURRENT;
                      };
                      let Some shdr = getshdr scn in
                      let shdr = Shdr.create shdr in
                      Shdr.update { shdr with
                        Shdr.sh_name = 6l;
                        Shdr.sh_type = SHT_STRTAB;
                        (* sh_flags = [SHF_STRINGS;SHF_ALLOC]; *)
                        Shdr.sh_entsize = 0l;
                      };
                      update_shstrndx elf (ndxscn scn);
                      ignore (update elf C_NULL);
                      Phdr.update { phdr with
                        Phdr.p_type = PT_PHDR;
                        Phdr.p_offset = ehdr.Ehdr.e_phoff;
                        Phdr.p_filesz = fsize T_PHDR 1l EV_CURRENT;
                      };
                      flagphdr elf C_SET F_DIRTY;
                      ignore (update elf C_WRITE);
                      ends elf;
                      Unix.close fd;
                      exit 0
        end
