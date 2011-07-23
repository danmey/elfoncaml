open Elf
module Phdr = struct
  type native_t
  type t = {
    p_type : pt;
    p_offset : off;
    p_vaddr : addr;
    p_paddr : addr;
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

external newehdr : t -> Ehdr.native_t = "caml_elf32_newehdr"
external newphdr : t -> int -> Phdr.native_t = "caml_elf32_newphdr"
external fsize : dtype -> int32 -> version -> int = "caml_elf32_fsize"
external getehdr : t -> Ehdr.native_t option = "caml_elf32_getehdr"
external getshdr : scn -> Shdr.native_t option = "caml_elf32_getshdr"
