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

let (-|) f g x = f (g x)
let (<|) f x = f x
let (|>) x f = f x

external elf_version : int -> int = "caml_elf_version"
let version v = elf_version (int_of_ev v) |> ev_of_int

external begins : Unix.file_descr -> elf_cmd -> elf = "caml_elf_begin"
external kind : elf -> elf_type = "caml_elf_kind"


