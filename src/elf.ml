type ev = [ 
  `NONE
| `CURRENT
| `NUM ]

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

type elf
type str_sec
type section
type section_data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
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
let section_data section =
  let size = section_size section in
  let ba = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout size in
  section_data_fill section ba;
  ba

    
exception Elf_error of string * string
