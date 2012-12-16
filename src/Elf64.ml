(*---------------------------------------------------------------------------
  Copyright (c) 2012 Wojciech Meyer
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the
     distribution.

  3. Neither the name of Wojciech Meyer nor the names of
     contributors may be used to endorse or promote products derived
     from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)


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
