(*----------------------------------------------------------------------------
  Elf_headers.ml - Make elf headers parametrized by the architecture sizes.
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

module type TYPES = sig
  type word
  type off
  type addr
  type xword
  type half
  type todo
  val string_of_addr : addr -> string
  val string_of_off : off -> string
  val string_of_word : word -> string
  val string_of_half : half -> string
      
end

module Make(T : TYPES) = struct
  open Elf

  module Ehdr = struct

    type native_t

    type t = {
      e_ident     : ident;
      e_type      : type_;
      e_machine   : machine;
      e_version   : version;
      e_entry     : T.addr;
      e_phoff     : T.off;
      e_shoff     : T.off;
      e_flags     : T.word;
      e_ehsize    : T.half;
      e_phentsize : T.half;
      e_phnum     : T.half;
      e_shentsize : T.half;
      e_shnum     : T.half;
      e_shstrndx  : T.half;
      ehdr: native_t;
    }

    external create : native_t -> t = "caml_Elf32_Ehdr_create"
    external update : t -> unit = "caml_Elf32_Ehdr_update"

    let string_of_eclass = function
      | ELFCLASSNONE -> "ELFCLASSNONE"
      | ELFCLASS32 -> "ELFCLASS32"
      | ELFCLASS64 -> "ELFCLASS64"
      | ELFCLASSNUM -> "ELFCLASSNUM"

    let string_of_adata = function
      | ELFDATANONE -> "ELFDATANONE"
      | ELFDATA2LSB -> "ELFDATA2LSB"
      | ELFDATA2MSB -> "ELFDATA2MSB"
      | ELFDATANUM -> "ELFDATANUM"

    let string_of_version = function
      | EV_NONE -> "EV_NONE"
      | EV_CURRENT -> "EV_CURRENT"
      | EV_NUM -> "EV_NUM"

    let string_of_osabi = function
      | ELFOSABI_NONE -> "ELFOSABI_NONE"
      | ELFOSABI_SYSV -> "ELFOSABI_SYSV"
      | ELFOSABI_HPUX -> "ELFOSABI_HPUX"
      | ELFOSABI_NETBSD -> "ELFOSABI_NETBSD"
      | ELFOSABI_LINUX -> "ELFOSABI_LINUX"
      | ELFOSABI_SOLARIS -> "ELFOSABI_SOLARIS"
      | ELFOSABI_AIX -> "ELFOSABI_AIX"
      | ELFOSABI_IRIX -> "ELFOSABI_IRIX"
      | ELFOSABI_FREEBSD -> "ELFOSABI_FREEBSD"
      | ELFOSABI_TRU64 -> "ELFOSABI_TRU64"
      | ELFOSABI_MODESTO -> "ELFOSABI_MODESTO"
      | ELFOSABI_OPENBSD -> "ELFOSABI_OPENBSD"
      | ELFOSABI_OPENVMS -> "ELFOSABI_OPENVMS"
      | ELFOSABI_NSK -> "ELFOSABI_NSK"
      | ELFOSABI_AROS -> "ELFOSABI_AROS"
      | ELFOSABI_ARM -> "ELFOSABI_ARM"
      | ELFOSABI_STANDALONE -> "ELFOSABI_STANDALONE"

    let string_of_type = function
      | ET_NONE -> "ET_NONE"
      | ET_REL -> "ET_REL"
      | ET_EXEC -> "ET_EXEC"
      | ET_DYN -> "ET_DYN"
      | ET_CORE -> "ET_CORE"
      | ET_NUM -> "ET_NUM"
      | ET_LOOS -> "ET_LOOS"
      | ET_HIOS -> "ET_HIOS"
      | ET_LOPROC -> "ET_LOPROC"
      | ET_HIPROC -> "ET_HIPROC"

    let to_string
        { e_ident = {
          mag0;
          mag1;
          mag2;
          mag3;
          eclass;
          adata;
          osabi;
          abiversion };
          e_type;
          e_machine;
          e_version;
          e_entry;
          e_phoff;
          e_shoff;
          e_flags;
          e_ehsize;
          e_phentsize;
          e_phnum;
          e_shentsize;
          e_shnum;
          e_shstrndx } =
      let c = int_of_char in
      let f = Printf.sprintf in
      "Magic:\t" ^ f "%2x %2x %2x %2x" (c mag0) (c mag1) (c mag2) (c mag3) ^ "\n"
      ^ "Class:\t" ^ (string_of_eclass eclass)  ^ "\n"
      ^ "Data:\t" ^ (string_of_adata adata)  ^ "\n"
      ^ "Version:\t" ^ (string_of_version e_version) ^ "\n"
      ^ "OS/ABI:\t" ^ (string_of_osabi osabi) ^ "\n"
      ^ "ABI Version:\t" ^ string_of_int abiversion ^ "\n"
      ^ "Type:\t" ^ (string_of_type e_type) ^ "\n"
      ^ "Machine:\t" ^ (string_of_machine e_machine) ^ "\n"
      ^ "Entry point address:\t" ^ f "0x%s" (T.string_of_addr e_entry) ^ "\n"
      ^ "Start of program headers:\t" ^ T.string_of_off e_phoff ^ "\n"
      ^ "Start of section headers:\t" ^ T.string_of_off e_shoff ^ "\n"
      ^ "Flags:\t" ^ T.string_of_word e_flags ^ "\n"
      ^ "Size of this header:\t" ^ T.string_of_half e_ehsize ^ "\n"
      ^ "Size of program headers:\t" ^ T.string_of_half e_phentsize ^ "\n"
      ^ "Number of program headers:\t" ^ T.string_of_half e_phnum ^ "\n"
      ^ "Size of section headers:\t" ^ T.string_of_half e_shentsize ^ "\n"
      ^ "Number of section headers:\t" ^ T.string_of_half e_shnum ^ "\n"
      ^ "Section header string table index:\t" ^ T.string_of_half e_shstrndx ^ "\n"

  end


  module Phdr = struct
      
    type native_t

    type t = {
      p_type : pt;
      p_offset : T.todo;
      p_vaddr : T.todo;
      p_paddr : T.todo;
      p_filesz : int;
      p_memsz : int;
      p_flags : int;
      p_align : int;
      phdr: native_t;
    }

    external create : native_t -> t = "caml_Elf64_Phdr_create"
    external update : t -> unit = "caml_Elf64_Phdr_update"

  end


  module Shdr = struct

    type native_t

    type t = {
      sh_name		:T.todo;
      sh_type		:sh_type;
      sh_flags		:sh_flags list;
      sh_addr		:T.todo;
      sh_offset		:T.todo;
      sh_size		:T.todo;
      sh_link		:T.todo;
      sh_info		:T.todo;
      sh_addralign	:T.todo;
      sh_entsize	:T.todo;
      shdr              :native_t;
    }

    external create : native_t -> t = "caml_Elf64_Shdr_create"
    external update : t -> unit = "caml_Elf64_Shdr_update"

  end


end
