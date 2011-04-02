#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>
#include <caml/signals.h>
#include <caml/config.h>
#include <caml/custom.h>
#include <caml/callback.h>
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

static value * elf_error_exn = NULL;

static void elf_error (char *cmdname) {
  value res;
  value name = Val_unit, err = Val_unit;

  Begin_roots2 (name, err);
    /* name = copy_string (cmdname); */
    err = copy_string (elf_errmsg (-1));
    /* if (elf_error_exn == NULL) { */
    /*   elf_error_exn = caml_named_value("Elf.Elf_error"); */
    /*   if (elf_error_exn == NULL) */
    /*     invalid_argument("Exception Elf.Elf_error not initialized, please link elf.cma"); */
    /* } */
    /* res = alloc_small(3, 0); */
    /* Field(res, 0) = *elf_error_exn; */
    /* Field(res, 1) = name; */
    /* Field(res, 2) = err; */
    failwith(err);
  End_roots();
  //  mlraise(res);
}

static value alloc_elf(Elf* elf) {
  value v = alloc_custom(&elf_ops, sizeof(Elf *), 0, 1);
  Elf_val(v) = elf;
  return v;
}

CAMLprim value caml_elf_version (value version) {
  CAMLparam1 (version); 
  CAMLreturn (Val_int (elf_version (Int_val (version))));
}

CAMLprim value caml_elf_begin (value fd, value cmd, value ref) {
  CAMLparam3 (fd, cmd, ref);
  Elf* elf = 0;
  if (Is_block (ref))           /* If it's Some elf then get Elf* from it */
    elf = Elf_val (Field (ref, 0));
  elf = elf_begin (Int_val (fd), Int_val (cmd), elf);
  if (elf == 0)
    elf_error ("elf_begin");
  CAMLreturn (alloc_elf (elf));
}

CAMLprim value caml_elf_kind (value elf) {
  CAMLparam1 (elf); 
  CAMLreturn (Val_int (elf_kind (Elf_val (elf))));
}
