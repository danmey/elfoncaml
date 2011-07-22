open Elf

let err f x = failwith (Printf.sprintf f x)
let errx fmt_hdr fmt_rst a  = failwith (Printf.sprintf (fmt_hdr ^^ fmt_rst) a)

let _ =
  begin if Array.length Sys.argv != 2 then
    err "usage: %s file-name" Sys.argv.(0) end;
  
  begin if version `CURRENT == `NONE then
    errx "ELF library initialization " "failed: %s" (errmsg (-1)) end;

  let fd = Unix.openfile Sys.argv.(1) [Unix.O_RDONLY] 0 in

  match begins fd C_READ None with
    | None -> err "elf_begin() failed: %s." (errmsg (-1))
    | Some e -> begin
      if kind e != K_ELF then
        err "\"%s\" is not an ELF object." Sys.argv.(0);
      match getehdr e with
        | None -> err "getehdr() failed: %s." (errmsg (-1))
        | Some ehdr -> begin

          let bits = match getclass e with
            | ELFCLASSNONE -> err "getclass() failed: %s." (errmsg(-1))
            | ELFCLASS32 -> 32
            | _ -> 64 in
          Printf.printf "%s: %d-bit ELF object\n" Sys.argv.(1) bits;
          match getident e with
            | None -> err "getident() failed: %s." (errmsg (-1))
            | Some id -> begin
              Printf.printf "%3s e_ident[0..%1d] %7s" " " ei_abiversion " ";
              for i = 0 to ei_abiversion - 2  do
                let bytes = vis id.[i] 0 in
                Printf.printf " ['%s' %X]" bytes (int_of_char id.[i])
              done;
              print_endline "";
              let ehdr = Elf32_Ehdr.create ehdr in
              let pf v = Printf.printf "    %-20s 0x%x\n" v in
              let pfL v = Printf.printf "    %-20s 0x%Lx\n" v in
              let pfl v = Printf.printf "    %-20s 0x%lx\n" v in
              pf "e_type" (Obj.magic ehdr.e_type);
              pf "e_machine" (Obj.magic ehdr.e_machine);
              pf "e_version" (Obj.magic ehdr.e_version);
              pfL "e_entry" ehdr.e_entry;
              pfL "e_phoff" ehdr.e_phoff;
              pfL "e_shoff" ehdr.e_shoff;
              pfl "e_flags" ehdr.e_flags;
              pf "e_ehsize" ehdr.e_ehsize;
              pf "e_phentsize" ehdr.e_phentsize;
              pf "e_shentsize" ehdr.e_shentsize;

              match getshdrnum e with
                | -1 -> err "getshdrnum() failed: %s." (errmsg(-1))
                | n ->  begin 
                  pf "(shnum)" n;
                  match getshdrstrndx e with
                    | -1 -> err "getshdrstrndx() failed: %s." (errmsg(-1));
                    | n -> begin pf "(shstrndx)" n;
                        match getphdrnum e with
                          | -1 -> err "getphdrnum() failed: %s." (errmsg(-1));
                          | n -> begin pf "(phnum)" n;
                            ends e;
                            Unix.close fd
                          end
                    end
                end
            end
        end
    end

