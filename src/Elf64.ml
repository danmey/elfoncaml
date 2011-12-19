(*----------------------------------------------------------------------------
  elf64.ml - All the 64 bit libelf functionality.
  Copyright (C) 2011 Wojciech Meyer

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
  --------------------------------------------------------------------------*)


open Elf

module H = Elf_headers.Make (struct

  type word = int64
  type off = int64
  type addr = int64
  type xword = int64
  type todo = int64
  type half = int
  let string_of_addr = Int64.to_string
  let string_of_off = Int64.to_string
  let string_of_word = Int64.to_string
  let string_of_half = string_of_int

end)
include H


external newehdr : t -> Ehdr.native_t option = "caml_elf64_newehdr"
external newphdr : t -> int -> Phdr.native_t = "caml_elf64_newphdr"
external fsize : dtype -> int64 -> version -> int = "caml_elf64_fsize"
external getehdr : t -> Ehdr.native_t option = "caml_elf64_getehdr"
external getshdr : scn -> Shdr.native_t option = "caml_elf64_getshdr"
external getphdr : t -> Phdr.native_t option = "caml_elf64_getphdr"


module Exceptions = struct

  let l1 str f a =
    match f a with
      | Some i -> i
      | None -> raise (Elf_error str)
    
  let newehdr = l1 "newehdr" newehdr
  let getehdr = l1 "getehdr" getehdr
  let getshdr = l1 "getshdr" getshdr

end
