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
type section
type section_data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type elf32_ehdr
type elf32_phdr
module SectionData = Bigarray.Array1

let (-|) f g x = f (g x)
let (<|) f x = f x
let (|>) x f = f x

external elf_version : int -> int = "caml_elf_version"
let version v = elf_version (int_of_ev v) |> ev_of_int

external begins : Unix.file_descr -> elf_cmd -> elf option -> elf = "caml_elf_begin"
external kind : elf -> elf_kind = "caml_elf_kind"
external str_section : elf -> section = "caml_elf_str_section"
external sections : elf -> section list = "caml_elf_sections"
external section_name : elf -> section -> section -> string = "caml_elf_section_name"
external section_index : section -> int = "caml_elf_section_index"
external section_size : section -> int = "caml_elf_section_size"
external section_data_fill : section -> section_data -> unit
  = "caml_elf_section_data_fill"
external elf32_header : elf -> elf32_ehdr = "caml_elf_elf32_header"
external program_header : elf -> elf32_phdr = "caml_elf_ph"

let section_data section =
  let size = section_size section in
  let ba = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout size in
  section_data_fill section ba;
  ba

type ptr = Int64.t
type offset = Int64.t
type size = Int64.t

type elf_header = {
  e_ident     : elf_ident;
  e_type      : elf_type;
  e_machine   : machine;
  e_version   : version;
  e_entry     : ptr;
  e_phoff     : offset;
  e_shoff     : offset;
  e_flags     : int;
  e_ehsize    : size;
  e_phentsize : size;
  e_phnum     : int;
  e_shentsize : size;
  e_shnum     : int;
  e_shstrndx  : int;
}
and elf_ident = {
  mag0 : char;
  mag1 : char;
  mag2 : char;
  mag3 : char;
  eclass: elf_class;
  data: elf_data;
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
  p_offset : offset;
  p_vaddr : ptr;
  p_paddr : ptr;
  p_filesz : int;
  p_memsz : int;
  p_flags : int;
  p_align : int;
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


let ei_nident = 16
module type HEADER = sig
  type t = elf_header
  type native_t
  val create : elf -> t
  val put : t -> native_t -> unit
  val get : native_t -> t
  (* val to_string : t -> string *)
end

module Elf32Header = struct
  type t = elf_header
  type native_t = elf32_ehdr
  external put : t -> native_t -> unit = "caml_elf_elf32_put"
  external get_internal : native_t -> t -> unit = "caml_elf_elf32_get_internal"
  let get nt =
    let default = {
      e_ident =
        { mag0 = char_of_int 0x7f;
          mag1 = 'E';
          mag2 = 'L';
          mag3 = 'F';
          eclass = ELFCLASS32;
          data = ELFDATA2MSB;
          osabi = ELFOSABI_LINUX;
          abiversion = 0; };
      e_type      = ET_NONE;
      e_machine   = `I386;
      e_version   = EV_NONE;
      e_entry     = 0L;
      e_phoff     = 0L;
      e_shoff     = 0L;
      e_flags     = 0;
      e_ehsize    = 0L;
      e_phentsize = 0L;
      e_phnum     = 0;
      e_shentsize = 0L;
      e_shnum     = 0;
      e_shstrndx  = 0; }
    in
    get_internal nt default;
    default
  let create elf = get (elf32_header elf)
  let to_string
    { e_ident = { 
      mag0;
      mag1;
      mag2;
      mag3;
      eclass;
      data;
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
    ^ "Data:\t" ^ (match data with
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
      | `MIPS -> "MIPS RS3000 Little-endian"
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
      | `H8 -> "Hitachi H8)300H"
      | `H8S -> "Hitachi H8S"
      | `H8 -> "Hitachi H8)500"
      | `IA -> "Intel IA-64 processor architecture"
      | `MIPS -> "Stanford MIPS-X"
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
      | `ARC -> "ARC Cores Tangent-A5"
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
    ^ "Version:\t" ^ (match e_version with
      | EV_NONE -> "EV_NONE"
      | EV_CURRENT -> "EV_CURRENT"
      | EV_NUM -> "EV_NUM") ^ "\n"
    ^ "Entry point address:\t" ^ f "0x%Lx" e_entry ^ "\n"
    ^ "Start of program headers:\t" ^ Int64.to_string e_phoff ^ "\n"
    ^ "Start of section headers:\t" ^ Int64.to_string e_shoff ^ "\n"
    ^ "Flags:\t" ^ string_of_int e_flags ^ "\n"
    ^ "Size of this header:\t" ^ Int64.to_string e_ehsize ^ "\n"
    ^ "Size of program headers:\t" ^ Int64.to_string e_phentsize ^ "\n"
    ^ "Number of program headers:\t" ^ string_of_int e_phnum ^ "\n"
    ^ "Size of section headers:\t" ^ Int64.to_string e_shentsize ^ "\n"
    ^ "Number of section headers:\t" ^ string_of_int e_shnum ^ "\n"
    ^ "Section header string table index:\t" ^ string_of_int e_shstrndx ^ "\n"
end

module ProgramHeader = struct
  type t = phdr
  type native_t = elf32_phdr
  external put : t -> native_t -> unit = "caml_elf_ph_put"
  external get_internal : native_t -> t -> unit = "caml_elf_ph_get_internal"
  let get nt =
    let default = {
      p_type = PT_NULL;
      p_offset = 0L;
      p_vaddr = 0L;
      p_paddr = 0L;
      p_filesz = 0;
      p_memsz = 0;
      p_flags = 0;
      p_align = 0;
    }
    in
    get_internal nt default;
    default
  let create elf = get (program_header elf)
end


exception Elf_error of string * string
