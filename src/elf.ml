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
  | NULL
  | READ
  | WRITE
  | CLR
  | SET
  | FDDONE
  | FDREAD
  | RDWR
  | NUM

type elf

let (-|) f g x = f (g x)
let (<|) f x = f x
let (|>) x f = f x

external elf_version : int -> int = "caml_elf_version"
let version v = elf_version (int_of_ev v) |> ev_of_int

external elf_begin : Unix.file_descr -> elf_cmd -> elf -> elf = "caml_elf_begin"



