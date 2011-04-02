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

type elf_type =
  | K_NONE
  | K_AR
  | K_COFF
  | K_ELF
  | K_NUM

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

module SectionData = Bigarray.Array1

let (-|) f g x = f (g x)
let (<|) f x = f x
let (|>) x f = f x

external elf_version : int -> int = "caml_elf_version"
let version v = elf_version (int_of_ev v) |> ev_of_int

external begins : Unix.file_descr -> elf_cmd -> elf option -> elf = "caml_elf_begin"
external kind : elf -> elf_type = "caml_elf_kind"
external str_section : elf -> section = "caml_elf_str_section"
external sections : elf -> section list = "caml_elf_sections"
external section_name : elf -> section -> section -> string = "caml_elf_section_name"
external section_index : section -> int = "caml_elf_section_index"
external section_size : section -> int = "caml_elf_section_size"
external section_data_fill : section -> section_data -> unit
  = "caml_elf_section_data_fill"
external elf32_header : elf -> elf32_ehdr = "caml_elf_elf32_header"

let section_data section =
  let size = section_size section in
  let ba = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout size in
  section_data_fill section ba;
  ba

type ptr = Int64.t
type offset = Int64.t
type size = Int64.t

type elf_header = { 
  e_ident     : string;
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

module type HEADER = sig
  type t
  type native_t
  val create : elf -> native_t
  val put : t -> native_t -> unit
  val get : native_t -> t
end

module Elf32Header : HEADER = struct
  type t = elf_header
  type native_t = elf32_ehdr
  let create = elf32_header
  external put : t -> native_t -> unit = "caml_elf_elf32_put"
  external get_internal : native_t -> t -> unit = "caml_elf_elf32_get_internal"
  let get nt =
    let default = { 
      e_ident     = "";
      e_type      = K_NONE;
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
end
    
exception Elf_error of string * string
