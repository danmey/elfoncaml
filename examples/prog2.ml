(* open Elf *)

(* let err = Printf.fprintf stderr *)
(* let errx fmt_hdr fmt_rst  = Printf.fprintf stderr (fmt_hdr ^^ fmt_rst) *)

(* let _ = *)
(*   if Array.length Sys.argv != 2 then begin *)
(*     err "usage: %s file-name" Sys.argv.(0); *)
(*     exit 1 end; *)
      
(*   if version `CURRENT == `NONE then *)
(*     errx "ELF library initialization " "failed: %s" (errmsg (-1)); *)

(*   let fd = Unix.openfile Sys.argv.(1) [Unix.O_RDONLY] 0 in *)

(*   match begins fd C_READ None with *)
(*     | None -> err "elf_begin() failed: %s." (errmsg (-1)) *)
(*     | Some e -> begin *)
(*       if kind e != K_ELF then *)
(*         err "\"%s\" is not an ELF object." Sys.argv.(0); *)
      
            

(*     if (gelf_getehdr(e, &ehdr) == NULL) @\co{3}@ *)
(*         errx(EX_SOFTWARE, "getehdr() failed: %s.", *)
(*             elf_errmsg(-1)); *)

(*     if ((i = gelf_getclass(e)) == ELFCLASSNONE) @\co{4}@ *)
(*         errx(EX_SOFTWARE, "getclass() failed: %s.", *)
(*             elf_errmsg(-1)); *)

(*     (void) printf("%s: %d-bit ELF object\n", argv[1], *)
(*         i == ELFCLASS32 ? 32 : 64); *)

(*     if ((id = elf_getident(e, NULL)) == NULL) @\co{5}@ *)
(*         errx(EX_SOFTWARE, "getident() failed: %s.", *)
(*             elf_errmsg(-1)); *)

(*     (void) printf("%3s e_ident[0..%1d] %7s", " ", *)
(*         EI_ABIVERSION, " "); *)

(*     for (i = 0; i <= EI_ABIVERSION; i++) { *)
(*         (void) vis(bytes, id[i], VIS_WHITE, 0); *)
(*         (void) printf(" ['%s' %X]", bytes, id[i]); *)
(*     } *)

(*     (void) printf("\n"); *)

(* #define        PRINT_FMT        "    %-20s 0x%jx\n" *)
(* #define        PRINT_FIELD(N) do { \ *)
(*         (void) printf(PRINT_FMT, #N, (uintmax_t) ehdr.N); \ *)
(*     } while (0) *)

(*     PRINT_FIELD(e_type); @\co{6}@ *)
(*     PRINT_FIELD(e_machine); *)
(*     PRINT_FIELD(e_version); *)
(*     PRINT_FIELD(e_entry); *)
(*     PRINT_FIELD(e_phoff); *)
(*     PRINT_FIELD(e_shoff); *)
(*     PRINT_FIELD(e_flags); *)
(*     PRINT_FIELD(e_ehsize); *)
(*     PRINT_FIELD(e_phentsize); *)
(*     PRINT_FIELD(e_shentsize); *)

(*     if (elf_getshdrnum(e, &n) != 0) @\co{7}@ *)
(*         errx(EX_SOFTWARE, "getshdrnum() failed: %s.", *)
(*             elf_errmsg(-1)); *)
(*     (void) printf(PRINT_FMT, "(shnum)", (uintmax_t) n); *)

(*     if (elf_getshdrstrndx(e, &n) != 0) @\co{8}@ *)
(*         errx(EX_SOFTWARE, "getshdrstrndx() failed: %s.", *)
(*             elf_errmsg(-1)); *)
(*     (void) printf(PRINT_FMT, "(shstrndx)", (uintmax_t) n); *)

(*     if (elf_getphdrnum(e, &n) != 0) @\co{9}@ *)
(*         errx(EX_SOFTWARE, "getphdrnum() failed: %s.", *)
(*             elf_errmsg(-1)); *)
(*     (void) printf(PRINT_FMT, "(phnum)", (uintmax_t) n); *)

(*     (void) elf_end(e); *)
(*     (void) close(fd); *)
(*     exit(EX_OK); *)
(* } *)
