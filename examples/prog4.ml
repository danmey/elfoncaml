let _ =
  if Array.length Sys.argv != 2 then
    Printf.printf "usage: %s file-name" Sys.argv.(0)
  else
    match Elf.version `CURRENT with
      | `NONE -> print_endline "ELF library initialization failed"
      | _ -> 
        begin 
          let fd = Unix.openfile Sys.argv.(1) [Unix.O_RDONLY] 0 in
          Elf.begins fd Elf.READ;
          ()
        end
            
