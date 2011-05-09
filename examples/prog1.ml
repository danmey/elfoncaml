open Elf

let err = Printf.fprintf stderr
let errx fmt_hdr fmt_rst  = Printf.fprintf stderr (fmt_hdr ^^ fmt_rst)

let _ =
  if Array.length Sys.argv != 2 then
    err "usage: %s file-name" Sys.argv.(0);
      
  if version `CURRENT == `NONE then
    errx "ELF library initialization " "failed: %s" (errmsg (-1));
    
  let fd = Unix.openfile Sys.argv.(1) [Unix.O_RDONLY] 0 in

  match begins fd C_READ None with
    | None -> err "elf_begin() failed: %s." (errmsg(-1))
    | Some e -> 
      let k =
        match kind e with
          | K_AR ->   "ar(1) archive"
          | K_ELF ->  "elf object";
          | K_NONE -> "data";
          | _ -> "unrecognized"
      in
      Printf.printf "%s: %s\n" Sys.argv.(1) k;
      ends e;
      Unix.close fd;
      exit 0
