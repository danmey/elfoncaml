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

let string_of_machine = function
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
      | `NUM -> "NUM"
