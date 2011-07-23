include Arch_type

type addr = Int64.t
type off = Int64.t
type size = Int64.t
type word = Int32.t
type half = int
type scn
type t
type version =
  | EV_NONE
  | EV_CURRENT
  | EV_NUM
and cmd =
  | C_NULL
  | C_READ
  | C_WRITE
  | C_CLR
  | C_SET
  | C_FDDONE
  | C_FDREAD
  | C_RDWR
  | C_NUM
and kind =
  | K_NONE
  | K_AR
  | K_COFF
  | K_ELF
  | K_NUM
and ei =
  | EI_MAG0
  | EI_MAG1
  | EI_MAG2
  | EI_MAG3
  | EI_CLASS
  | EI_DATA
  | EI_VERSION
  | EI_OSABI
  | EI_ABIVERSION
  | EI_PAD
and ident = {
  mag0 : char;
  mag1 : char;
  mag2 : char;
  mag3 : char;
  eclass: class_type;
  adata: data_type;
  osabi: osabi_type;
  abiversion: int;
}
and data_type =
  | ELFDATANONE
  | ELFDATA2LSB
  | ELFDATA2MSB
  | ELFDATANUM
and class_type =
  | ELFCLASSNONE
  | ELFCLASS32
  | ELFCLASS64
  | ELFCLASSNUM
and osabi_type =
  | ELFOSABI_NONE
  | ELFOSABI_SYSV
  | ELFOSABI_HPUX
  | ELFOSABI_NETBSD
  | ELFOSABI_LINUX
  | ELFOSABI_SOLARIS
  | ELFOSABI_AIX
  | ELFOSABI_IRIX
  | ELFOSABI_FREEBSD
  | ELFOSABI_TRU64
  | ELFOSABI_MODESTO
  | ELFOSABI_OPENBSD
  | ELFOSABI_OPENVMS
  | ELFOSABI_NSK
  | ELFOSABI_AROS
  | ELFOSABI_ARM
  | ELFOSABI_STANDALONE
and type_ =
  | ET_NONE
  | ET_REL
  | ET_EXEC
  | ET_DYN
  | ET_CORE
  | ET_NUM
  | ET_LOOS
  | ET_HIOS
  | ET_LOPROC
  | ET_HIPROC
and pt =
  | PT_NULL
  | PT_LOAD
  | PT_DYNAMIC
  | PT_INTERP
  | PT_NOTE
  | PT_SHLIB
  | PT_PHDR
  | PT_TLS
  | PT_NUM
  | PT_LOOS
  | PT_HIOS
  | PT_LOPROC
  | PT_HIPROC
and sh_type =
    | SHT_NULL
    | SHT_PROGBITS
    | SHT_SYMTAB
    | SHT_STRTAB
    | SHT_RELA
    | SHT_HASH
    | SHT_DYNAMIC
    | SHT_NOTE
    | SHT_NOBITS
    | SHT_REL
    | SHT_SHLIB
    | SHT_DYNSYM
    | SHT_INIT_ARRAY
    | SHT_FINI_ARRAY
    | SHT_PREINIT_ARRAY
    | SHT_GROUP
    | SHT_SYMTAB_SHNDX
    | SHT_NUM
    | SHT_LOOS
    | SHT_HIOS
    | SHT_LOPROC
    | SHT_HIPROC
    | SHT_LOUSER
    | SHT_HIUSER
and sh_flags =
  | SHF_WRITE
  | SHF_ALLOC
  | SHF_EXECINSTR
  | SHF_MERGE
  | SHF_STRINGS
  | SHF_INFO_LINK
  | SHF_LINK_ORDER
  | SHF_OS_NONCONFORMING
  | SHF_GROUP
  | SHF_TLS
  | SHF_MASKOS
  | SHF_MASKPROC
and dtype =
  | T_BYTE
  | T_ADDR
  | T_DYN
  | T_EHDR
  | T_HALF
  | T_OFF
  | T_PHDR
  | T_RELA
  | T_REL
  | T_SHDR
  | T_SWORD
  | T_SYM
  | T_WORD
  | T_SXWORD
  | T_XWORD
  | T_VDEF
  | T_VNEED
  | T_NUM
and flags =
  | F_DIRTY
  | F_LAYOUT
  | F_LAYOUT_OVERLAP

module Data = struct
  type ('a,'b) t = {
    d_buf     : ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t option;
    d_type    : dtype;
    d_size    : size;
    d_off     : off;
    d_align   : size;
    d_version : version;
    data      : data_type;
  }
      
  type native_t
  external create : native_t -> ('a, 'b) t = "caml_Elf_Data_create"
  external update : ('a, 'b) t -> unit = "caml_Elf_Data_update"
end

external version : int -> int = "caml_elf_version"
external begins : Unix.file_descr -> cmd -> t option -> t option = "caml_elf_begin"
external ends :  t -> unit = "caml_elf_end"
external errmsg : int -> string = "caml_elf_errmsg"
external kind : t -> kind = "caml_elf_kind"
external getshdrstrndx : t -> int = "caml_elf_getshdrstrndx"
external getscn : t -> int -> scn option = "caml_elf_getscn"
external ndxscn : scn -> int = "caml_elf_ndxscn"
external newscn : t -> scn option = "caml_elf_newscn"
external update : t -> cmd -> int = "caml_elf_update"
external flagphdr : t -> cmd -> int -> unit = "caml_elf_flagphdr"
external nextscn : t -> scn option -> scn option = "caml_elf_nextscn"
external update_shstrndx : t -> int -> unit = "caml_elfx_update_shstrndx"
external newdata  : scn -> Data.native_t = "caml_elf_newdata"
external strptr : t -> int -> int -> string option = "caml_elf_strptr"
external getclass : t -> class_type = "caml_gelf_getclass"
external getident : t -> string option = "caml_elf_getident"
external getshdrnum : t -> int = "caml_elf_getshdrnum"
external getphdrnum : t -> int = "caml_elf_getphdrnum"
external update_shstrndx : t -> int -> unit = "caml_elfx_update_shstrndx"
external vis : char -> int -> string = "caml_vis"

  
let int_of_ev = function
  | `NONE -> 0
  | `CURRENT -> 1
  | `NUM -> 2

let int_of_flag = function
  | F_DIRTY -> 0x1
  | F_LAYOUT -> 0x4
  | F_LAYOUT_OVERLAP -> 0x10000000
    
let ev_of_int = function
  | 0 -> `NONE
  | 1 -> `CURRENT
  | 2 -> `NUM
  | _ -> failwith "ev_of_int: Wrong value"

let offset_of_ei : ei -> int = Obj.magic

let flagphdr elf cmd flag = flagphdr elf cmd (int_of_flag flag)
let version v =  ev_of_int (version (int_of_ev v))
let int_of_ei : ei -> int = Obj.magic

let rec sections elf =
  let rec loop = function
    | None -> []
    | Some scn -> scn :: loop (nextscn elf (Some scn))
  in
  loop (nextscn elf None)

let ei_nident = 16

exception Elf_error of string * string
let _ = Callback.register_exception "Elf.Elf_error" (Elf_error ("",""))
let err x = failwith (Printf.fprintf stderr x (errmsg (-1)))

