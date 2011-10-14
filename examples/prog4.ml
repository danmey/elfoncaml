open Elf
open Elf.Exceptions
open Elf32
open Elf32.Exceptions

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
          let elf = begins fd C_READ None in
          let kind = kind elf in
          match kind with
            | Elf.K_ELF -> begin
              let ehdr = Ehdr.create (newehdr elf) in
              print_endline (Ehdr.to_string ehdr);
              let idx = getshdrstrndx elf in
              let str_section = getscn elf idx in
              let sections = sections elf in 
              List.iter 
                (fun sec -> 
                  let shdr = getshdr sec in
                  let str_sec_idx = ndxscn str_section in
                  let idx = ndxscn sec in
                  let name = strptr elf str_sec_idx idx in
                  Printf.printf "Section %-4.4d %s\n"  idx name) sections
            end
            | _ -> print_endline "Uknown elf kind"
        end


            
