#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>
#include <caml/signals.h>
#include <caml/config.h>
#include <caml/custom.h>
#include <libelf.h>

static struct custom_operations elf_ops = {
  "org.danmey",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

/* Accessing the WINDOW * part of a Caml custom block */
#define Elf_val(v) (*((Elf **) Data_custom_val(v)))

static value alloc_elf(Elf* elf)
{
  value v = alloc_custom(&elf_ops, sizeof(Elf *), 0, 1);
  Elf_val(v) = elf;
  return v;
}

CAMLprim value caml_elf_version (value version) {
  CAMLparam1 (version); 
  CAMLreturn (Val_int (elf_version (Int_val (version))));
}

CAMLprim value caml_elf_begin (value fd, value cmd/*, value ref*/) {
  CAMLparam2 (fd, cmd /*,ref*/); 
  CAMLreturn (alloc_elf
              (elf_begin 
               (Int_val (fd),
                Int_val (cmd),
                0)));
  /*, Elf_val (ref)))); */
}

CAMLprim value caml_elf_kind (value elf) {
  CAMLparam1 (elf); 
  CAMLreturn (Val_int (elf_kind (Elf_val (elf))));
}
