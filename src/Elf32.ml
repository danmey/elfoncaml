(*----------------------------------------------------------------------------
  elf32.ml - All the 32 bit libelf functionality
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

  type word = int32
  type off = int32
  type addr = int32
  type xword = int32
  type todo = int32
  type half = int
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


module Exceptions = struct

  let l1 str f a =
    match f a with
      | Some i -> i
      | None -> raise (Elf_error str)
    
  let getehdr = l1 "newehdr" getehdr
  let getshdr = l1 "newshdr" getshdr

end
