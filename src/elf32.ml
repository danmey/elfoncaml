open Elf

module H = Elf_headers.Make (struct type t = int32 let to_string = Int32.to_string end)
include H

external newehdr : t -> Ehdr.native_t = "caml_elf32_newehdr"
external newphdr : t -> int -> Phdr.native_t = "caml_elf32_newphdr"
external fsize : dtype -> int32 -> version -> int = "caml_elf32_fsize"
external getehdr : t -> Ehdr.native_t option = "caml_elf32_getehdr"
external getshdr : scn -> Shdr.native_t option = "caml_elf32_getshdr"
