open Elf
module Phdr = struct

  type native_t

  type t = {
    p_type : pt;
    p_offset : int32;
    p_vaddr : int32;
    p_paddr : int32;
    p_filesz : int;
    p_memsz : int;
    p_flags : int;
    p_align : int;
    phdr: native_t;
  }

  external create : native_t -> t = "caml_Elf32_Phdr_create"
  external update : t -> unit = "caml_Elf32_Phdr_update"

end


module Shdr = struct

  type native_t

  type t = {
    sh_name		:int32;
    sh_type		:sh_type;
    sh_flags		:sh_flags list;
    sh_addr		:int32;
    sh_offset		:int32;
    sh_size		:int32;
    sh_link		:int32;
    sh_info		:int32;
    sh_addralign	:int32;
    sh_entsize		:int32;
    shdr                :native_t;
  }

  external create : native_t -> t = "caml_Elf32_Shdr_create"
  external update : t -> unit = "caml_Elf32_Shdr_update"

end

module Ehdr = struct

  type native_t

  type t = {
    e_ident     : ident;
    e_type      : type_;
    e_machine   : machine;
    e_version   : version;
    e_entry     : int32;
    e_phoff     : int32;
    e_shoff     : int32;
    e_flags     : int32;
    e_ehsize    : int;
    e_phentsize : int;
    e_phnum     : int;
    e_shentsize : int;
    e_shnum     : int;
    e_shstrndx  : int;
    ehdr: native_t;
  }


  external create : native_t -> t = "caml_Elf32_Ehdr_create"
  external update : t -> unit = "caml_Elf32_Ehdr_update"

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
    ^ "Class:\t" ^ (match eclass with
      | ELFCLASSNONE -> "ELFCLASSNONE"
      | ELFCLASS32 -> "ELFCLASS32"
      | ELFCLASS64 -> "ELFCLASS64"
      | ELFCLASSNUM -> "ELFCLASSNUM")  ^ "\n"
    ^ "Data:\t" ^ (match adata with
      | ELFDATANONE -> "ELFDATANONE"
      | ELFDATA2LSB -> "ELFDATA2LSB"
      | ELFDATA2MSB -> "ELFDATA2MSB"
      | ELFDATANUM -> "ELFDATANUM")  ^ "\n"
    ^ "Version:\t" ^ (match e_version with
      | EV_NONE -> "EV_NONE"
      | EV_CURRENT -> "EV_CURRENT"
      | EV_NUM -> "EV_NUM") ^ "\n"
    ^ "OS/ABI:\t" ^ (match osabi with
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
      | ELFOSABI_STANDALONE -> "ELFOSABI_STANDALONE") ^ "\n"
    ^ "ABI Version:\t" ^ string_of_int abiversion ^ "\n"
    ^ "Type:\t" ^ (match e_type with
      | ET_NONE -> "ET_NONE"
      | ET_REL -> "ET_REL"
      | ET_EXEC -> "ET_EXEC"
      | ET_DYN -> "ET_DYN"
      | ET_CORE -> "ET_CORE"
      | ET_NUM -> "ET_NUM"
      | ET_LOOS -> "ET_LOOS"
      | ET_HIOS -> "ET_HIOS"
      | ET_LOPROC -> "ET_LOPROC"
      | ET_HIPROC -> "ET_HIPROC") ^ "\n"
    ^ "Machine:\t" ^ (string_of_machine e_machine) ^ "\n"
    ^ "Entry point address:\t" ^ f "0x%lx" e_entry ^ "\n"
    ^ "Start of program headers:\t" ^ Int32.to_string e_phoff ^ "\n"
    ^ "Start of section headers:\t" ^ Int32.to_string e_shoff ^ "\n"
    ^ "Flags:\t" ^ Int32.to_string e_flags ^ "\n"
    ^ "Size of this header:\t" ^ string_of_int e_ehsize ^ "\n"
    ^ "Size of program headers:\t" ^ string_of_int e_phentsize ^ "\n"
    ^ "Number of program headers:\t" ^ string_of_int e_phnum ^ "\n"
    ^ "Size of section headers:\t" ^ string_of_int e_shentsize ^ "\n"
    ^ "Number of section headers:\t" ^ string_of_int e_shnum ^ "\n"
    ^ "Section header string table index:\t" ^ string_of_int e_shstrndx ^ "\n"

end

external newehdr : t -> Ehdr.native_t = "caml_elf32_newehdr"
external newphdr : t -> int -> Phdr.native_t = "caml_elf32_newphdr"
external fsize : dtype -> int32 -> version -> int = "caml_elf32_fsize"
external getehdr : t -> Ehdr.native_t option = "caml_elf32_getehdr"
external getshdr : scn -> Shdr.native_t option = "caml_elf32_getshdr"
