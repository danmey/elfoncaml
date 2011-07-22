let (-|) f g x = f (g x)
let (<|) f x = f x
let (|>) x f = f x

type version =
  | EV_NONE
  | EV_CURRENT
  | EV_NUM

let int_of_ev = function
  | `NONE -> 0
  | `CURRENT -> 1
  | `NUM -> 2

let ev_of_int = function
  | 0 -> `NONE
  | 1 -> `CURRENT
  | 2 -> `NUM
  | _ -> failwith "ev_of_int: Wrong value"

type elf_cmd =
  | C_NULL
  | C_READ
  | C_WRITE
  | C_CLR
  | C_SET
  | C_FDDONE
  | C_FDREAD
  | C_RDWR
  | C_NUM

type elf_kind =
  | K_NONE
  | K_AR
  | K_COFF
  | K_ELF
  | K_NUM

type elf_ei =
    EI_MAG0
  | EI_MAG1
  | EI_MAG2
  | EI_MAG3
  | EI_CLASS
  | EI_DATA
  | EI_VERSION
  | EI_OSABI
  | EI_ABIVERSION
  | EI_PAD

let offset_of_ei : elf_ei -> int = Obj.magic

type machine =
  [ `M32 (* AT&T WE 32100 *)
  | `SPARC (* SPARC *)
  | `I386 (* Intel 80386 *)
  | `M68K (* Motorola 68000 *)
  | `M88K (* Motorola 88000 *)
  | `I486 (* Intel i486 (DO NOT USE THIS ONE) *)
  | `I860 (* Intel 80860 *)
  | `MIPS (* MIPS I Architecture *)
  | `S370 (* IBM System)370 Processor *)
  | `MIPS (* MIPS RS3000 Little-endian *)
  | `SPARC64 (* SPARC 64-bit *)
  | `PARISC (* Hewlett-Packard PA-RISC *)
  | `VPP500 (* Fujitsu VPP500 *)
  | `SPARC32PLUS (* Enhanced instruction set SPARC *)
  | `I960 (* Intel 80960 *)
  | `PPC (* PowerPC *)
  | `PPC64 (* 64-bit PowerPC *)
  | `S390 (* IBM System)390 Processor *)
  | `V800 (* NEC V800 *)
  | `FR20 (* Fujitsu FR20 *)
  | `RH32 (* TRW RH-32 *)
  | `RCE (* Motorola RCE *)
  | `ARM (* Advanced RISC Machines ARM *)
  | `ALPHA (* Digital Alpha *)
  | `SH (* Hitachi SH *)
  | `SPARCV9 (* SPARC Version 9 *)
  | `TRICORE (* Siemens TriCore embedded processor *)
  | `ARC (* Argonaut RISC Core, Argonaut Technologies Inc. *)
  | `H8 (* Hitachi H8)300 *)
  | `H8 (* Hitachi H8)300H *)
  | `H8S (* Hitachi H8S *)
  | `H8 (* Hitachi H8)500 *)
  | `IA (* Intel IA-64 processor architecture *)
  | `MIPS (* Stanford MIPS-X *)
  | `COLDFIRE (* Motorola ColdFire *)
  | `M68HC12 (* Motorola M68HC12 *)
  | `MMA (* Fujitsu MMA Multimedia Accelerator *)
  | `PCP (* Siemens PCP *)
  | `NCPU (* Sony nCPU embedded RISC processor *)
  | `NDR1 (* Denso NDR1 microprocessor *)
  | `STARCORE (* Motorola Star*Core processor *)
  | `ME16 (* Toyota ME16 processor *)
  | `ST100 (* STMicroelectronics ST100 processor *)
  | `TINYJ (* Advanced Logic Corp. TinyJ embedded processor family *)
  | `X86 (* AMD x86-64 architecture *)
  | `AMD64
  | `PDSP (* Sony DSP Processor *)
  | `FX66 (* Siemens FX66 microcontroller *)
  | `ST9PLUS (* STMicroelectronics ST9+ 8)16 bit microcontroller *)
  | `ST7 (* STMicroelectronics ST7 8-bit microcontroller *)
  | `M68HC16 (* Motorola MC68HC16 Microcontroller *)
  | `M68HC11 (* Motorola MC68HC11 Microcontroller *)
  | `M68HC08 (* Motorola MC68HC08 Microcontroller *)
  | `M68HC05 (* Motorola MC68HC05 Microcontroller *)
  | `SVX (* Silicon Graphics SVx *)
  | `ST19 (* STMicroelectronics ST19 8-bit microcontroller *)
  | `VAX (* Digital VAX *)
  | `CRIS (* Axis Communications 32-bit embedded processor *)
  | `JAVELIN (* Infineon Technologies 32-bit embedded processor *)
  | `FIREPATH (* Element 14 64-bit DSP Processor *)
  | `ZSP (* LSI Logic 16-bit DSP Processor *)
  | `MMIX (* Donald Knuth's educational 64-bit processor *)
  | `HUANY (* Harvard University machine-independent object files *)
  | `PRISM (* SiTera Prism *)
  | `AVR (* Atmel AVR 8-bit microcontroller *)
  | `FR30 (* Fujitsu FR30 *)
  | `D10V (* Mitsubishi D10V *)
  | `D30V (* Mitsubishi D30V *)
  | `V850 (* NEC v850 *)
  | `M32R (* Mitsubishi M32R *)
  | `MN10300 (* Matsushita MN10300 *)
  | `MN10200 (* Matsushita MN10200 *)
  | `PJ (* picoJava *)
  | `OPENRISC (* OpenRISC 32-bit embedded processor *)
  | `ARC (* ARC Cores Tangent-A5 *)
  | `XTENSA (* Tensilica Xtensa Architecture *)
  | `VIDEOCORE (* Alphamosaic VideoCore processor *)
  | `TMM (* Thompson Multimedia General Purpose Processor *)
  | `NS32K (* National Semiconductor 32000 series *)
  | `TPC (* Tenor Network TPC processor *)
  | `SNP1K (* Trebia SNP 1000 processor *)
  | `ST200 (* STMicroelectronics (www.st.com) ST200 microcontroller *)
  | `IP2K (* Ubicom IP2xxx microcontroller family *)
  | `MAX (* MAX Processor *)
  | `CR (* National Semiconductor CompactRISC microprocessor *)
  | `F2MC16 (* Fujitsu F2MC16 *)
  | `MSP430 (* Texas Instruments embedded microcontroller msp430 *)
  | `BLACKFIN (* Analog Devices Blackfin (DSP) processor *)
  | `SE (* S1C33 Family of Seiko Epson processors *)
  | `SEP (* Sharp embedded microprocessor *)
  | `ARCA (* Arca RISC Microprocessor *)
  | `UNICORE (* Microprocessor series from PKU-Unity Ltd. and MPRC of Peking University *)
  | `NUM]

type elf
type str_sec
type elf_scn
type elf32_ehdr
type elf32_phdr
type elf32_shdr
type gelf_shdr
module SectionBuffer = Bigarray.Array1

type addr = Int64.t
type off = Int64.t
type size = Int64.t
type word = Int32.t
type half = int
type ('a, 'b) elf_image = {
  elf_header: ehdr;
  program_header: phdr option;
  sections: ('a, 'b) section_entry list
}
and ('a, 'b) section_entry = {
  section_header: shdr;
  section_data: ('a, 'b) data;
}
and ehdr = {
  e_ident     : elf_ident;
  e_type      : elf_type;
  e_machine   : machine;
  e_version   : version;
  e_entry     : addr;
  e_phoff     : off;
  e_shoff     : off;
  e_flags     : word;
  e_ehsize    : half;
  e_phentsize : half;
  e_phnum     : half;
  e_shentsize : half;
  e_shnum     : half;
  e_shstrndx  : half;
  ehdr: elf32_ehdr;
}
and elf_ident = {
  mag0 : char;
  mag1 : char;
  mag2 : char;
  mag3 : char;
  eclass: elf_class;
  adata: elf_data;
  osabi: elf_osabi;
  abiversion: int;
}
and elf_data =
  | ELFDATANONE
  | ELFDATA2LSB
  | ELFDATA2MSB
  | ELFDATANUM
and elf_class =
  | ELFCLASSNONE
  | ELFCLASS32
  | ELFCLASS64
  | ELFCLASSNUM
and elf_osabi =
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
and elf_type =
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

and phdr = {
  p_type : pt;
  p_offset : off;
  p_vaddr : addr;
  p_paddr : addr;
  p_filesz : int;
  p_memsz : int;
  p_flags : int;
  p_align : int;
  phdr: elf32_phdr;
}
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
and shdr = {
    sh_name		:word;
    sh_type		:sh_type;
    sh_flags		:sh_flags list;
    sh_addr		:addr;
    sh_offset		:off;
    sh_size		:word;
    sh_link		:word;
    sh_info		:word;
    sh_addralign	:word;
    sh_entsize		:word;
    shdr                :elf32_shdr;
} and sh_type =
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

and ('a,'b) data = {
  d_buf     : ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t option;
  d_type    : dtype;
  d_size    : size;
  d_off     : off;
  d_align   : size;
  d_version : version;
  data      : elf_data;
}
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
and elf_flags =
  | F_DIRTY
  | F_LAYOUT
  | F_LAYOUT_OVERLAP

let int_of_flag = function
  | F_DIRTY -> 0x1
  | F_LAYOUT -> 0x4
  | F_LAYOUT_OVERLAP -> 0x10000000


external version : int -> int = "caml_elf_version"
external begins : Unix.file_descr -> elf_cmd -> elf option -> elf option = "caml_elf_begin"
external ends : elf -> unit = "caml_elf_end"
external errmsg : int -> string = "caml_elf_errmsg"
external kind : elf -> elf_kind = "caml_elf_kind"
external getshdrstrndx : elf -> int = "caml_elf_getshdrstrndx"
external getscn : elf -> int -> elf_scn option = "caml_elf_getscn"
external ndxscn : elf_scn -> int = "caml_elf_ndxscn"
external newehdr : elf -> elf32_ehdr = "caml_elf32_newehdr"
external newphdr : elf -> int -> elf32_phdr = "caml_elf32_newphdr"
external newscn : elf -> elf_scn option = "caml_elf_newscn"
external update : elf -> elf_cmd -> int = "caml_elf_update"
external fsize : dtype -> int32 -> version -> int = "caml_elf32_fsize"
external flagphdr : elf -> elf_cmd -> int -> unit = "caml_elf_flagphdr"
external nextscn : elf -> elf_scn option -> elf_scn option = "caml_elf_nextscn"
external update_shstrndx : elf -> int -> unit = "caml_elfx_update_shstrndx"
external newdata  : elf_scn -> elf_data = "caml_elf_newdata"
external strptr : elf -> int -> int -> string option = "caml_elf_strptr"
external getehdr : elf -> elf32_ehdr option = "caml_elf32_getehdr"
external getshdr : elf_scn -> elf32_shdr option = "caml_elf32_getshdr"
external getclass : elf -> elf_class = "caml_gelf_getclass"
external getident : elf -> string option = "caml_elf_getident"
external getshdrnum : elf -> int = "caml_elf_getshdrnum"
external getphdrnum : elf -> int = "caml_elf_getphdrnum"
external update_shstrndx : elf -> int -> unit = "caml_elfx_update_shstrndx"
external vis : char -> int -> string = "caml_vis"

let flagphdr elf cmd flag = flagphdr elf cmd (int_of_flag flag)
let version v = version (int_of_ev v) |> ev_of_int
let int_of_ei : elf_ei -> int = Obj.magic

let rec sections elf =
  let rec loop = function
    | None -> []
    | Some scn -> scn :: loop (nextscn elf (Some scn))
  in
  loop (nextscn elf None)

(* let section_data section = *)
(*   let size = section_size section in *)
(*   let ba = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout size in *)
(*   section_data_fill section ba; *)
(*   ba *)

let ei_nident = 16

module Elf32_Ehdr = struct
  type t = ehdr
  type native_t = elf32_ehdr

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
    ^ "Machine:\t" ^ (match e_machine with
      | `M32 -> "AT&T WE 32100"
      | `SPARC -> "SPARC"
      | `I386 -> "Intel 80386"
      | `M68K -> "Motorola 68000"
      | `M88K -> "Motorola 88000"
      | `I486 -> "Intel i486 (DO NOT USE THIS ONE)"
      | `I860 -> "Intel 80860"
      | `MIPS -> "MIPS I Architecture"
      | `S370 -> "IBM System)370 Processor"
      | `SPARC64 -> "SPARC 64-bit"
      | `PARISC -> "Hewlett-Packard PA-RISC"
      | `VPP500 -> "Fujitsu VPP500"
      | `SPARC32PLUS -> "Enhanced instruction set SPARC"
      | `I960 -> "Intel 80960"
      | `PPC -> "PowerPC"
      | `PPC64 -> "64-bit PowerPC"
      | `S390 -> "IBM System)390 Processor"
      | `V800 -> "NEC V800"
      | `FR20 -> "Fujitsu FR20"
      | `RH32 -> "TRW RH-32"
      | `RCE -> "Motorola RCE"
      | `ARM -> "Advanced RISC Machines ARM"
      | `ALPHA -> "Digital Alpha"
      | `SH -> "Hitachi SH"
      | `SPARCV9 -> "SPARC Version 9"
      | `TRICORE -> "Siemens TriCore embedded processor"
      | `ARC -> "Argonaut RISC Core, Argonaut Technologies Inc."
      | `H8 -> "Hitachi H8)300"
      | `H8S -> "Hitachi H8S"
      | `IA -> "Intel IA-64 processor architecture"
      | `COLDFIRE -> "Motorola ColdFire"
      | `M68HC12 -> "Motorola M68HC12"
      | `MMA -> "Fujitsu MMA Multimedia Accelerator"
      | `PCP -> "Siemens PCP"
      | `NCPU -> "Sony nCPU embedded RISC processor"
      | `NDR1 -> "Denso NDR1 microprocessor"
      | `STARCORE -> "Motorola Star*Core processor"
      | `ME16 -> "Toyota ME16 processor"
      | `ST100 -> "STMicroelectronics ST100 processor"
      | `TINYJ -> "Advanced Logic Corp. TinyJ embedded processor family"
      | `X86 -> "AMD x86-64 architecture"
      | `AMD64
      | `PDSP -> "Sony DSP Processor"
      | `FX66 -> "Siemens FX66 microcontroller"
      | `ST9PLUS -> "STMicroelectronics ST9+ 8)16 bit microcontroller"
      | `ST7 -> "STMicroelectronics ST7 8-bit microcontroller"
      | `M68HC16 -> "Motorola MC68HC16 Microcontroller"
      | `M68HC11 -> "Motorola MC68HC11 Microcontroller"
      | `M68HC08 -> "Motorola MC68HC08 Microcontroller"
      | `M68HC05 -> "Motorola MC68HC05 Microcontroller"
      | `SVX -> "Silicon Graphics SVx"
      | `ST19 -> "STMicroelectronics ST19 8-bit microcontroller"
      | `VAX -> "Digital VAX"
      | `CRIS -> "Axis Communications 32-bit embedded processor"
      | `JAVELIN -> "Infineon Technologies 32-bit embedded processor"
      | `FIREPATH -> "Element 14 64-bit DSP Processor"
      | `ZSP -> "LSI Logic 16-bit DSP Processor"
      | `MMIX -> "Donald Knuth's educational 64-bit processor"
      | `HUANY -> "Harvard University machine-independent object files"
      | `PRISM -> "SiTera Prism"
      | `AVR -> "Atmel AVR 8-bit microcontroller"
      | `FR30 -> "Fujitsu FR30"
      | `D10V -> "Mitsubishi D10V"
      | `D30V -> "Mitsubishi D30V"
      | `V850 -> "NEC v850"
      | `M32R -> "Mitsubishi M32R"
      | `MN10300 -> "Matsushita MN10300"
      | `MN10200 -> "Matsushita MN10200"
      | `PJ -> "picoJava"
      | `OPENRISC -> "OpenRISC 32-bit embedded processor"
      | `XTENSA -> "Tensilica Xtensa Architecture"
      | `VIDEOCORE -> "Alphamosaic VideoCore processor"
      | `TMM -> "Thompson Multimedia General Purpose Processor"
      | `NS32K -> "National Semiconductor 32000 series"
      | `TPC -> "Tenor Network TPC processor"
      | `SNP1K -> "Trebia SNP 1000 processor"
      | `ST200 -> "STMicroelectronics (www.st.com) ST200 microcontroller"
      | `IP2K -> "Ubicom IP2xxx microcontroller family"
      | `MAX -> "MAX Processor"
      | `CR -> "National Semiconductor CompactRISC microprocessor"
      | `F2MC16 -> "Fujitsu F2MC16"
      | `MSP430 -> "Texas Instruments embedded microcontroller msp430"
      | `BLACKFIN -> "Analog Devices Blackfin (DSP) processor"
      | `SE -> "S1C33 Family of Seiko Epson processors"
      | `SEP -> "Sharp embedded microprocessor"
      | `ARCA -> "Arca RISC Microprocessor"
      | `UNICORE -> "Microprocessor series from PKU-Unity Ltd. and MPRC of Peking University"
      | `NUM -> "NUM") ^ "\n"
    ^ "Entry point address:\t" ^ f "0x%Lx" e_entry ^ "\n"
    ^ "Start of program headers:\t" ^ Int64.to_string e_phoff ^ "\n"
    ^ "Start of section headers:\t" ^ Int64.to_string e_shoff ^ "\n"
    ^ "Flags:\t" ^ Int32.to_string e_flags ^ "\n"
    ^ "Size of this header:\t" ^ string_of_int e_ehsize ^ "\n"
    ^ "Size of program headers:\t" ^ string_of_int e_phentsize ^ "\n"
    ^ "Number of program headers:\t" ^ string_of_int e_phnum ^ "\n"
    ^ "Size of section headers:\t" ^ string_of_int e_shentsize ^ "\n"
    ^ "Number of section headers:\t" ^ string_of_int e_shnum ^ "\n"
    ^ "Section header string table index:\t" ^ string_of_int e_shstrndx ^ "\n"
end

module Elf32_Phdr = struct
  type t = phdr
  type native_t = elf32_phdr
  external create : native_t -> t = "caml_Elf32_Phdr_create"
  external update : t -> unit = "caml_Elf32_Phdr_update"
end

module Elf32_Shdr = struct
  type native_t = elf32_shdr
  type t = shdr
  external create : native_t -> t = "caml_Elf32_Shdr_create"
  external update : t -> unit = "caml_Elf32_Shdr_update"
end

module Elf_Data = struct
  type native_t = elf_data
  type ('a,'b) t = ('a,'b) data
  external create : native_t -> ('a, 'b) t = "caml_Elf_Data_create"
  external update : ('a, 'b) t -> unit = "caml_Elf_Data_update"
end

exception Elf_error of string * string
let _ = Callback.register_exception "Elf.Elf_error" (Elf_error ("",""))
let err x = failwith (Printf.fprintf stderr x (errmsg (-1)))

(* let read file_name = *)
(*   match version `CURRENT with *)
(*     | `NONE -> err "version" *)
(*     | _ -> *)
(*       begin *)
(*         let fd = Unix.openfile file_name [Unix.O_RDONLY] 0 in *)
(*         match begins fd C_READ None with *)
(*             | None -> err "begins" *)
(*             | Some elf -> *)
(*               begin *)
(*                 match kind with *)
(*                   | K_ELF -> begin *)
(*                     let ehdr = Elf32Header.get (elf32_header elf) in *)
(*                     let sections = sections elf in *)
(*                     let data = section_data str_section in *)
(*                     { elf_header = Elf32Header.create elf; *)
                      
