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
              let shstrndx = getshdrstrndx elf in
              let sections = sections elf in
              List.iter
                (fun sec ->
                  let shdr = getshdr sec in
                  let shdr = Shdr.create shdr in
                  let name = strptr elf shstrndx (Int32.to_int shdr.Shdr.sh_name) in
                  match getdata sec with
                  | Some data ->
                      let data = Data.create data in
                      Array.iter print_int data.Data.d_buf;
                      Printf.printf "Section %-4.4d %s\n"  (ndxscn sec) name
                  | None -> err "getdata failed: %s"
                ) sections
            end
            | _ -> print_endline "Uknown elf kind"
        end
