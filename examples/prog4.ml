open Elf
open Elf32

let err x = failwith (Printf.sprintf x (Elf.errmsg (-1)))
let _ =
  if Array.length Sys.argv != 2 then
    Printf.printf "usage: %s <file-name>\n" Sys.argv.(0)
  else
    match Elf.version `CURRENT with
      | `NONE -> err "elf_version() failed: %s."
      | _ -> 
        begin 
          let fd = Unix.openfile Sys.argv.(1) [Unix.O_RDONLY] 0 in
          match begins fd C_READ None with
            | None -> err "elf_begin() failed: %s."
            | Some elf -> begin
              let kind = kind elf in
              match kind with
                | Elf.K_ELF -> begin
                  let ehdr = Ehdr.create (newehdr elf) in
                  print_endline (Ehdr.to_string ehdr);
                  let Some idx = getshdrstrndx elf in
                  match getscn elf idx with
                    | None -> err "elf_getscn() failed: %s."
                    | Some str_section -> begin
                      let sections = sections elf in 
                      List.iter 
                        (fun sec -> 
                          let shdr = getshdr sec in
                          match shdr with
                            | None -> err "gelf_getshdr() failed: %s."
                            | Some shdr ->
                              let str_sec_idx = ndxscn str_section in
                              let idx = ndxscn sec in
                              let name = strptr elf str_sec_idx idx in
                              match name with
                                | None -> err "elf_strptr() failed: %s."
                                | Some name -> Printf.printf "Section %-4.4d %s\n"  idx name) sections;
                      (* Printf.printf ".shstrab: size = %d\n" (section_size str_section); *)
                      (* let data = section_data str_section in *)
                      (* for i = 0 to (SectionBuffer.dim data) - 1 do *)
                      (*   Printf.printf "%c" (char_of_int (SectionBuffer.get data i)); *)
                      (*   print_char (if i mod 16 = 0 then '\n' else ' '); *)
                      (* done *)
                    end
                end
                | _ -> print_endline "Uknown elf kind"
            end
        end


            
