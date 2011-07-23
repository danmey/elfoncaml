open Elf

module H = Elf_headers.Make (struct
  type word = int32
  type off = int32
  type addr = int32
  type xword = int32
  type half = int
  type todo = int32
  let string_of_addr = Int32.to_string
  let string_of_off = Int32.to_string
  let string_of_word = Int32.to_string
  let string_of_half = string_of_int
end)
include H

external newehdr : t -> Ehdr.native_t = "caml_elf32_newehdr"
external newphdr : t -> int -> Phdr.native_t = "caml_elf32_newphdr"
external fsize : dtype -> int32 -> version -> int = "caml_elf32_fsize"
external getehdr : t -> Ehdr.native_t option = "caml_elf32_getehdr"
external getshdr : scn -> Shdr.native_t option = "caml_elf32_getshdr"
