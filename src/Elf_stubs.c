#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>
#include <caml/signals.h>
#include <caml/config.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/bigarray.h>
#include <libelf.h>
#include <gelf.h>
#include <string.h>
#include <stdio.h>
#include <vis.h>

#define Elf_Half_val Int_val
#define Elf_Word_val Int32_val
#define Elf_Addr_val Int32_val
#define Elf_Off_val Int32_val

#define Val_Elf_Half Val_int
#define Val_Elf_Word copy_int32
#define Val_Elf_Addr copy_int32
#define Val_Elf_Off copy_int32

static struct custom_operations elf_ops = {
  "org.danmey",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

/* Accessing the libelf data structures in O'Caml block */
#define Struct(name) \
  static inline name* name##_val(value v) { return (*((name**) Data_custom_val(v))); } \
  static inline value alloc_##name(name* s) { \
    value v = alloc_custom(&elf_ops, sizeof(name *), 0, 1); \
    (*((name**) Data_custom_val(v))) = s; \
    return v; } \
  static inline value Val_##name(name* s) { return alloc_##name(s); }

Struct(Elf)
Struct(Elf_Scn)
Struct(Elf32_Ehdr)
Struct(Elf32_Phdr)
Struct(Elf32_Shdr)
Struct(Elf_Data)
Struct(GElf_Shdr)

static value * elf_error_exn = NULL;

/* Throw Elf_error exception */
static void elf_error (char *cmdname) {
  CAMLlocal3 (res, name, err);
  name = Val_unit, err = Val_unit;

  Begin_roots2 (name, err);
  name = copy_string (cmdname);
  err = copy_string (elf_errmsg (-1));
  if (elf_error_exn == NULL) {
    elf_error_exn = caml_named_value("Elf.Elf_error");
    if (elf_error_exn == NULL)
      invalid_argument("Exception Elf.Elf_error not initialized, please link elf.cma");
  }
  res = alloc_small(3, 0);
  Field(res, 0) = *elf_error_exn;
  Field(res, 1) = name;
  Field(res, 2) = err;
  End_roots();
  mlraise(res);
}

#define Decl_option_val(name)                            \
  static inline name* name##_option_val (value option) { \
    if (Is_block (option))                               \
      return name##_val (Field (option, 0));             \
    return 0;                                            \
  }

#define Decl_Val_named_option(name, conv)                  \
  static inline value Val_##name##_option (name* ptr)      \
  {                                                        \
    value option = Val_int (0);                            \
    if (ptr) {                                             \
      option = caml_alloc_small (1, 1);                    \
      Field(option, 0) = conv (ptr);                       \
    }                                                      \
    return option;                                         \
  }

#define Decl_Val_option(typ) Decl_Val_named_option(typ, alloc_##typ)

Decl_Val_option (Elf)
Decl_Val_option (Elf_Scn)
Decl_Val_option (GElf_Shdr)
Decl_Val_option (Elf32_Shdr)
Decl_Val_option (Elf32_Ehdr)
Decl_option_val (Elf)
Decl_option_val (Elf_Scn)
Decl_option_val (GElf_Shdr)

#define string char
Decl_Val_named_option (string, copy_string)
#undef string

#define ml_val(name)                                        \
  CAMLprim value caml_val_##name () {                       \
    CAMLparam0 ();                                          \
    CAMLreturn (Val_int (name)); }

#define ml_fun0(ret, name)                                  \
  CAMLprim value caml_##name () {                           \
    CAMLparam0 ();                                          \
    CAMLreturn (Val_ ## ret (name ())); }

#define ml_fun1(ret, name, arg1)                            \
  CAMLprim value caml_##name (value _a1) {                  \
    CAMLparam1 (_a1);                                       \
    CAMLreturn (Val_ ## ret (name (arg1##_val (_a1)))); }

#define ml_fun2(ret, name, arg1, arg2)                                \
  CAMLprim value caml_##name (value _a1, value _a2) {                 \
    CAMLparam2 (_a1, _a2);                                            \
    CAMLreturn (Val_ ## ret (name (arg1##_val (_a1), arg2##_val (_a2)))); }

#define ml_fun3(ret, name, arg1, arg2, arg3)                           \
  CAMLprim value caml_##name (value _a1, value _a2, value _a3) {       \
    CAMLparam3 (_a1, _a2, _a3);                                        \
    CAMLreturn (Val_ ## ret (name (arg1##_val (_a1), arg2##_val (_a2), arg3##_val (_a3)))); }

#define ml_internal_fun1(ret, name, arg1)                           \
  CAMLprim value caml_##name (value _a1) {                          \
    CAMLparam1 (_a1);                                               \
    CAMLreturn (Val_ ## ret (caml_##name##_internal (arg1##_val (_a1)))); }

#define ml_internal_fun2(ret, name, arg1, arg2)                     \
  CAMLprim value caml_##name (value _a1, value _a2) {               \
    CAMLparam2 (_a1, _a2);                                                  \
    CAMLreturn (Val_ ## ret (caml_##name##_internal (arg1##_val (_a1), arg2##_val (_a2)))); }

#define ml_internal_fun3(ret, name, arg1, arg2, arg3)                   \
  CAMLprim value caml_##name (value _a1, value _a2, value _a3) {        \
    CAMLparam3 (_a1, _a2, _a3);                                         \
    CAMLreturn (Val_ ## ret (caml_internal_##name (arg1##_val (_a1), arg2##_val (_a2), arg3##_val (_a3)))); }
    
CAMLprim int caml_elf_getshdrstrndx_internal (Elf* elf) {
  size_t shstrndx;
  if (elf_getshdrstrndx (elf, &shstrndx) != 0)
    elf_error ("elf_getshdrstrndx");
  return shstrndx;
}

/* TODO: Remove leaks, wrap it with a finalizer */
CAMLprim GElf_Shdr* caml_gelf_getshdr_internal (Elf_Scn* elf_scn) {
  GElf_Shdr* shdr = malloc (sizeof (GElf_Shdr));
  if ( gelf_getshdr (elf_scn, shdr) != shdr) {
    free (shdr);
    return 0;
  }
  return shdr;
}

CAMLprim char* caml_elf_getident_internal (Elf* elf) {
  return elf_getident (elf, NULL);
}

/* TODO: Should be factored in macro */
CAMLprim int caml_elf_getshdrnum_internal (Elf* elf) {
  size_t num;
  if (elf_getshdrnum (elf, &num) == 0)
    return -1;
  return num;
}

CAMLprim int caml_elf_getphdrnum_internal (Elf* elf) {
  size_t num;
  if (elf_getphdrnum (elf, &num) == 0)
    return -1;
  return num;
}

/* TODO: This should be removed along with vis.h header! */
CAMLprim char* caml_vis_internal (int a, int b) {
  char* str = malloc (256);
  vis (str, a, VIS_WHITE, b);
  return str;
}


#define Val_copy_string copy_string
#define Val_unit2(_a) Val_unit

ml_fun3 (Elf_option, elf_begin, Int, Int, Elf_option)
ml_fun2 (unit2, elf_cntl, Int, Elf_option)
ml_fun1 (unit2, elf_end, Elf)
ml_fun1 (copy_string, elf_errmsg, Int)
ml_fun0 (int, elf_errno)
ml_fun1 (int, elf_kind, Elf)
ml_fun2 (Elf_Scn_option, elf_getscn, Elf, Int)
ml_fun1 (int, elf_ndxscn, Elf_Scn)
ml_fun1 (int, elf_version, Int)
ml_fun3 (int, elf_flagdata, Elf_Data, Int, Int)
ml_fun3 (int, elf_flagehdr, Elf, Int, Int)
ml_fun3 (int, elf_flagelf,  Elf, Int, Int)
ml_fun3 (unit2, elf_flagphdr, Elf, Int, Int)
ml_fun3 (int, elf_flagscn , Elf_Scn, Int, Int)
ml_fun3 (int, elf_flagshdr, Elf_Scn, Int, Int)
ml_fun3 (int, elf32_fsize, Int, Int32, Int)
ml_fun2 (int, elf_update, Elf, Int)
ml_fun1 (Elf_Scn_option, elf_newscn, Elf)
ml_fun2 (Elf_Scn_option, elf_nextscn, Elf, Elf_Scn_option)
ml_fun2 (unit2, elfx_update_shstrndx, Elf, Int)
ml_fun2 (Elf32_Phdr, elf32_newphdr, Elf, Int)
ml_fun1 (Elf32_Ehdr, elf32_newehdr, Elf)
ml_fun1 (Elf32_Ehdr_option, elf32_getehdr, Elf)
ml_fun1 (Elf32_Shdr_option, elf32_getshdr, Elf_Scn)
ml_fun3 (string_option, elf_strptr, Elf, Int, Int)
ml_fun1 (int, gelf_getclass, Elf)
ml_internal_fun1 (string_option, elf_getident, Elf)
ml_internal_fun1 (int, elf_getshdrstrndx, Elf)
ml_internal_fun1 (int, elf_getshdrnum, Elf)
ml_internal_fun1 (int, elf_getphdrnum, Elf)

/* TODO: This should be removed along with vis.h header! */
ml_internal_fun2 (copy_string, vis, Int, Int)
ml_val (EI_ABIVERSION)
/* Shall we support Gelf, the only advantage I see here to handle
   uniformly architecture dependent bits */
/* ml_internal_fun1 (GElf_Shdr_option, gelf_getshdr, Elf_Scn); */
/* ml_internal_fun1 (GElf_Shdr_option, gelf_getehdr, Elf_Scn); */

#undef Val_copy_string


CAMLprim value caml_elf_section_data_fill (value section, value bigarray) {
  CAMLparam2 (section, bigarray);
  Elf_Data* data = NULL; int n = 0;
  GElf_Shdr shdr;
  char* buffer = (char*)Data_bigarray_val (bigarray);
  if (gelf_getshdr (Elf_Scn_val (section), &shdr) != &shdr)
    elf_error ("gelf_getshdr");
  while (n < shdr.sh_size &&
         (data = elf_getdata (Elf_Scn_val (section),
                              data)) != NULL) {
    char* p = (char *) data->d_buf;
    memcpy (buffer + n, p, data->d_size);
    n += data->d_size;
  }
  CAMLreturn (Val_unit);
}


CAMLprim value caml_elf_newdata (value scn)
{
  CAMLparam1 (scn);
  Elf_Data* data = elf_newdata (Elf_Scn_val (scn));
  CAMLreturn (alloc_Elf_Data (data));
}

#define Variant(name) { #name, EM_##name, 0}

#define EM_I386 EM_386
#define EM_M68K EM_68K
#define EM_M88K EM_68K
#define EM_I860 EM_860

struct {
  char* name;
  unsigned int constant;
  value hash;
} variants[] =
  {
    Variant (NONE),
    Variant (M32),
    Variant (SPARC),
    Variant (I386),
    Variant (M68K),
    Variant (M88K),
    //    Variant (I486),
    Variant (I860),
    Variant (MIPS),
    Variant (S370),
    Variant (MIPS),
    //    Variant (SPARC64),
    Variant (PARISC),
    Variant (VPP500),
    Variant (SPARC32PLUS),
    //    Variant (I960),
    Variant (PPC),
    Variant (PPC64),
    Variant (S390),
    Variant (V800),
    Variant (FR20),
    Variant (RH32),
    Variant (RCE),
    Variant (ARM),
    Variant (ALPHA),
    Variant (SH),
    Variant (SPARCV9),
    Variant (TRICORE),
    Variant (ARC),
    //    Variant (H8),
    //    Variant (H8),
    Variant (H8S),
    //    Variant (H8),
    //    Variant (IA),
    Variant (MIPS),
    Variant (COLDFIRE),
    //    Variant (M68HC12),
    Variant (MMA),
    Variant (PCP),
    Variant (NCPU),
    Variant (NDR1),
    Variant (STARCORE),
    Variant (ME16),
    Variant (ST100),
    Variant (TINYJ),
    //    Variant (X86),
    //    Variant (AMD64),
    Variant (PDSP),
    Variant (FX66),
    Variant (ST9PLUS),
    Variant (ST7),
    //    Variant (M68HC16),
    //    Variant (M68HC11),
    //    Variant (M68HC08),
    //    Variant (M68HC05),
    Variant (SVX),
    Variant (ST19),
    Variant (VAX),
    Variant (CRIS),
    Variant (JAVELIN),
    Variant (FIREPATH),
    Variant (ZSP),
    Variant (MMIX),
    Variant (HUANY),
    Variant (PRISM),
    Variant (AVR),
    Variant (FR30),
    Variant (D10V),
    Variant (D30V),
    Variant (V850),
    Variant (M32R),
    Variant (MN10300),
    Variant (MN10200),
    Variant (PJ),
    Variant (OPENRISC),
    Variant (ARC),
    Variant (XTENSA),
    //    Variant (VIDEOCORE),
    //    Variant (TMM),
    //    Variant (NS32K),
    //    Variant (TPC),
    //    Variant (SNP1K),
    //    Variant (ST200),
    //    Variant (IP2K),
    //    Variant (MAX),
    //    Variant (CR),
    //    Variant (F2MC16),
    //    Variant (MSP430),
    //    Variant (BLACKFIN),
    //    Variant (SE),
    //    Variant (SEP),
    //    Variant (ARCA),
    //    Variant (UNICORE),
    Variant (NUM),
  };

static void init_polvariants () {
  int i;
  if (variants[0].hash == 0)
    {
      for (i=0; i < sizeof(variants) / sizeof(variants[0]);
           i++)
        variants[i].hash = hash_variant (variants[i].name);
    }
}

static unsigned int variant_to_enum (value hash)
{
  int i=0;
  init_polvariants ();
  for (i=0;
       i < sizeof(variants) / sizeof(variants[0]);
       i++) {
        if (variants[i].hash == hash)
          return variants[i].constant;
  }
  failwith ("variant_to_enum: Wrong variant.");
  return 0;
}

static value enum_to_variant (unsigned int en)
{
  int i=0;
  init_polvariants ();
  for (i=0;
       i < sizeof(variants) / sizeof(variants[0]);
       i++)
    if (variants[i].constant == en)
      return variants[i].hash;
  failwith ("enum_to_variant: Wrong enum.");
  return 0;
}

static int et_tab[] =
{
  ET_NONE,
  ET_REL,
  ET_EXEC,
  ET_DYN,
  ET_CORE,
  ET_NUM,
  ET_LOOS,
  ET_HIOS,
  ET_LOPROC,
  ET_HIPROC,
};

int et_to_int(int v)
{
  int i;
  for (i=0; i < sizeof(et_tab)/sizeof(et_tab[0]); i++)
    if (et_tab[i] == v)
      return i;
  failwith ("et_to_int: Wrong enum.");
  return 0;
}

/* TODO: This mess needs to be refactored */
#define BEGIN_CAML_BLOCK(f,x) do { int _field = f; value _hdr = x;
#define END_CAML_BLOCK()  } while (0)
#define READ_FIELD(name, convert) hdr->name = convert (Field (_hdr, _field)); _field++;
#define WRITE_FIELD(name, convert) Store_field (_hdr, _field, convert (hdr->name)); _field++
#define WRITE_FIELD_IM(name, convert) Store_field (_hdr, _field, convert (name)); _field++;

CAMLprim value caml_Elf32_Ehdr_update (value ehdr)
{
  CAMLparam1 (ehdr);
  CAMLlocal1 (e_ident);
  Elf32_Ehdr* hdr = Elf32_Ehdr_val(Field(ehdr, 14));
  e_ident = Field (ehdr, 0);
  BEGIN_CAML_BLOCK (0, e_ident);
  READ_FIELD (e_ident[_field], Int_val);
  READ_FIELD (e_ident[_field], Int_val);
  READ_FIELD (e_ident[_field], Int_val);
  READ_FIELD (e_ident[_field], Int_val);
  READ_FIELD (e_ident[_field], Int_val);
  READ_FIELD (e_ident[_field], Int_val);
  READ_FIELD (e_ident[_field], Int_val);
  READ_FIELD (e_ident[_field], Int_val);
  END_CAML_BLOCK ();
#define ET_TAB(what) et_tab[Int_val (what)]
  BEGIN_CAML_BLOCK (1, ehdr);
  READ_FIELD(e_type, ET_TAB);
  READ_FIELD(e_machine, variant_to_enum);
  READ_FIELD(e_version   , Int_val);
  READ_FIELD(e_entry     , Elf_Addr_val);
  READ_FIELD(e_phoff     , Elf_Off_val);
  READ_FIELD(e_shoff     , Elf_Off_val);
  READ_FIELD(e_flags     , Elf_Word_val);
  READ_FIELD(e_ehsize    , Elf_Half_val);
  READ_FIELD(e_phentsize , Elf_Half_val);
  READ_FIELD(e_phnum     , Elf_Half_val);
  READ_FIELD(e_shentsize , Elf_Half_val);
  READ_FIELD(e_shnum     , Elf_Half_val);
  READ_FIELD(e_shstrndx  , Elf_Half_val);
  END_CAML_BLOCK ();
  CAMLreturn (Val_unit);
}

CAMLprim value caml_Elf32_Ehdr_create (value elf32_ehdr)
{
  CAMLparam1 (elf32_ehdr);
  CAMLlocal2 (e_ident, ehdr);
  Elf32_Ehdr* hdr = Elf32_Ehdr_val (elf32_ehdr);

  e_ident = caml_alloc(8, 0);

  BEGIN_CAML_BLOCK (0, e_ident);
  WRITE_FIELD (e_ident[_field], Val_int);
  WRITE_FIELD (e_ident[_field], Val_int);
  WRITE_FIELD (e_ident[_field], Val_int);
  WRITE_FIELD (e_ident[_field], Val_int);
  WRITE_FIELD (e_ident[_field], Val_int);
  WRITE_FIELD (e_ident[_field], Val_int);
  WRITE_FIELD (e_ident[_field], Val_int);
  WRITE_FIELD (e_ident[_field], Val_int);
  END_CAML_BLOCK ();
  ehdr = caml_alloc(15, 0);
#define ET_TO_INT(x) Val_int (et_to_int (x))
#define ID(x) x
  BEGIN_CAML_BLOCK (0, ehdr);
  WRITE_FIELD_IM (e_ident, ID);
  WRITE_FIELD (e_type, ET_TO_INT);
  WRITE_FIELD (e_machine, enum_to_variant);
  WRITE_FIELD (e_version, Val_int);
  WRITE_FIELD (e_entry, Val_Elf_Addr);
  WRITE_FIELD (e_phoff, Val_Elf_Off);
  WRITE_FIELD (e_shoff, Val_Elf_Off);
  WRITE_FIELD (e_flags, Val_Elf_Word);
  WRITE_FIELD (e_ehsize, Val_Elf_Half);
  WRITE_FIELD (e_phentsize, Val_Elf_Half);
  WRITE_FIELD (e_phnum, Val_Elf_Half);
  WRITE_FIELD (e_shentsize, Val_Elf_Half);
  WRITE_FIELD (e_shnum, Val_Elf_Half);
  WRITE_FIELD (e_shstrndx, Val_Elf_Half);
  WRITE_FIELD_IM (elf32_ehdr, ID);
  END_CAML_BLOCK ();
  CAMLreturn (ehdr);
}

static unsigned long pt_tab[] =
{
  0,1,2,3,4,5,6,7,8,
  0x60000000,
  0x6fffffff,
  0x70000000,
  0x7fffffff
};

unsigned long pt_to_int(int v)
{
  int i;
  for (i=0; i < sizeof(pt_tab)/sizeof(pt_tab[0]); i++)
    if (pt_tab[i] == v)
      return i;
  failwith ("pt_to_int: Wrong enum.");
  return 0;
}

static unsigned long sht_tab[] =
  { SHT_NULL,
  SHT_PROGBITS,
  SHT_SYMTAB,
  SHT_STRTAB,
  SHT_RELA,
  SHT_HASH,
  SHT_DYNAMIC,
  SHT_NOTE,
  SHT_NOBITS,
  SHT_REL,
  SHT_SHLIB,
  SHT_DYNSYM,
  SHT_INIT_ARRAY,
  SHT_FINI_ARRAY,
  SHT_PREINIT_ARRAY,
  SHT_GROUP,
  SHT_SYMTAB_SHNDX,
  SHT_NUM,
  SHT_LOOS,
  SHT_HIOS,
  SHT_LOPROC,
  SHT_HIPROC,
  SHT_LOUSER,
  SHT_HIUSER};

unsigned long sht_to_int(unsigned long v)
{
  int i;
  for (i=0; i < sizeof(sht_tab)/sizeof(sht_tab[0]); i++)
    if (sht_tab[i] == v)
      return i;
  failwith ("sht_to_int: Wrong enum.");
  return 0;
}

static unsigned long shf_tab[] =
  { SHF_WRITE,
    SHF_ALLOC,
    SHF_EXECINSTR,
    SHF_MERGE,
    SHF_STRINGS,
    /* SHF_INFO, */
    /* SHF_LINK, */
    /* SHF_OS, */
    SHF_GROUP,
    SHF_TLS,
    SHF_MASKOS,
    SHF_MASKPROC };

unsigned long shf_to_int(int v)
{
  int i;
  for (i=0; i < sizeof(shf_tab)/sizeof(shf_tab[0]); i++)
    if (shf_tab[i] == v)
      return i;
  failwith ("shf_to_int: Wrong enum.");
  return 0;
}

unsigned long mlflags_to_int (value list)
{
  unsigned long result = 0;
  do {
    if (Is_block (list)) {
      result |= shf_tab[Int_val (Field (list, 0))];
      list = Field (list, 1);
    }
    else return result;
  } while (1);
}

value int_to_mlflags (unsigned long flags)
{
  CAMLparam0 ();
  CAMLlocal2 (result, list);
  int i;
  if (flags == 0)
    result = Val_int (0);
  else {
    result = alloc_small (2,0);
    for (i=0; i < sizeof(shf_tab)/sizeof(shf_tab[0]); i++) {
      if (shf_tab[i] & flags) {
        Field (list, 0) = Val_int (i);
        if (i == sizeof(shf_tab)/sizeof(shf_tab[0]) - 1) {
          Field (list, 1) = Val_int (0);
        }
        else
          {
            Field (list, 1) = alloc_small (2,0);
            list = Field (list, 1);
          }
      }
    }
  }
  CAMLreturn (result);
}


CAMLprim value caml_Elf32_Phdr_update (value phdr)
{
  CAMLparam1 (phdr);
  Elf32_Phdr* hdr = Elf32_Phdr_val(Field(phdr, 8));

  BEGIN_CAML_BLOCK (0, phdr);
#define PT_TAB(x) pt_tab[Int_val (x)]
  READ_FIELD (p_type, PT_TAB);
  READ_FIELD (p_offset, Int32_val);
  READ_FIELD (p_vaddr , Int32_val);
  READ_FIELD (p_paddr , Int32_val);
  READ_FIELD (p_filesz, Int_val);
  READ_FIELD (p_memsz , Int_val);
  READ_FIELD (p_flags , Int_val);
  READ_FIELD (p_align , Int_val);
  END_CAML_BLOCK ();
  CAMLreturn (Val_unit);
}

CAMLprim value caml_Elf32_Phdr_create (value elf32_phdr)
{
  CAMLparam1 (elf32_phdr);
  CAMLlocal1 (phdr);
  phdr = caml_alloc(9, 0);
  Elf32_Phdr* hdr = Elf32_Phdr_val (elf32_phdr);
  Field (phdr, 0) = Val_int (pt_to_int (hdr->p_type));
  Field (phdr, 1) = copy_int32 (hdr->p_offset);
  Field (phdr, 2) = copy_int32 (hdr->p_vaddr);
  Field (phdr, 3) = copy_int32 (hdr->p_paddr);
  Field (phdr, 4) = Val_int (hdr->p_filesz);
  Field (phdr, 5) = Val_int (hdr->p_memsz);
  Field (phdr, 6) = Val_int (hdr->p_flags);
  Field (phdr, 7) = Val_int (hdr->p_align);
  Field (phdr, 8) = elf32_phdr;
  CAMLreturn (phdr);
}

CAMLprim value caml_Elf32_Shdr_update (value shdr)
{
  CAMLparam1 (shdr);
  Elf32_Shdr* hdr = Elf32_Shdr_val(Field(shdr, 10));
  hdr->sh_name      = Int32_val (Field (shdr, 0));
  hdr->sh_type      = sht_tab [Int_val (Field (shdr, 1))];
  hdr->sh_flags     = mlflags_to_int (Field (shdr, 2));
  hdr->sh_addr      = Int32_val (Field (shdr, 3));
  hdr->sh_offset    = Int32_val (Field (shdr, 4));
  hdr->sh_size      = Int32_val (Field (shdr, 5));
  hdr->sh_link      = Int32_val (Field (shdr, 6));
  hdr->sh_info      = Int32_val (Field (shdr, 7));
  hdr->sh_addralign = Int32_val (Field (shdr, 8));
  hdr->sh_entsize   = Int32_val (Field (shdr, 9));
  CAMLreturn (Val_unit);
}

CAMLprim value caml_Elf32_Shdr_create (value elf32_shdr)
{
  CAMLparam1 (elf32_shdr);
  CAMLlocal1 (shdr);
  shdr = caml_alloc(11, 0);
  Elf32_Shdr* hdr = Elf32_Shdr_val (elf32_shdr);
  Field (shdr, 0) = copy_int32 (hdr->sh_name);
  Field (shdr, 1) = Val_int (sht_to_int (hdr->sh_type));
  Field (shdr, 2) = int_to_mlflags (hdr->sh_flags);
  Field (shdr, 3) = copy_int32 (hdr->sh_addr);
  Field (shdr, 4) = copy_int32 (hdr->sh_offset);
  Field (shdr, 5) = copy_int32 (hdr->sh_size);
  Field (shdr, 6) = copy_int32 (hdr->sh_link);
  Field (shdr, 7) = copy_int32 (hdr->sh_info);
  Field (shdr, 8) = copy_int32 (hdr->sh_addralign);
  Field (shdr, 9) = copy_int32 (hdr->sh_entsize);
  Field (shdr, 10) = elf32_shdr;
  CAMLreturn (shdr);
}

/* CAMLprim value caml_elf_elf32_getshdr (value section) */
/* { */
/*   CAMLparam1 (section); */
/*   Elf32_Shdr* shdr = 0; */
/*   if ( (shdr = elf32_getshdr (Elf_Scn_val (section))) == 0) */
/*     elf_error ("elf_getshdr"); */
/*   CAMLreturn (alloc_Elf32_Shdr (shdr)); */
/* } */


CAMLprim value caml_Elf_Data_update (value data)
{
  CAMLparam1 (data);
  Elf_Data* hdr = Elf_Data_val(Field(data, 6));

  BEGIN_CAML_BLOCK (0, data);
#define BA(x) (Is_block(x) ? Data_bigarray_val (Field(x,0)) : 0)
  READ_FIELD (d_buf, BA);
#undef BA
  READ_FIELD (d_type, Int_val);
  READ_FIELD (d_size, Int32_val);
  READ_FIELD (d_off, Int32_val);
  READ_FIELD (d_align, Int32_val);
  READ_FIELD (d_version, Int_val);
  END_CAML_BLOCK ();
  CAMLreturn (Val_unit);
}

value build_ba (void *x)
{
  CAMLparam0();
  CAMLlocal1(option);
  option = caml_alloc_small (1, 1);
  Field(option, 0) = caml_ba_alloc(CAML_BA_UINT8|CAML_BA_C_LAYOUT|CAML_BA_EXTERNAL, 1, x, 0);
  CAMLreturn (option);
}

CAMLprim value caml_Elf_Data_create (value elf_data)
{
  CAMLparam1 (elf_data);
  CAMLlocal1 (data);
  Elf_Data* hdr = Elf_Data_val (elf_data);
  data = caml_alloc_small(7, 0);
  BEGIN_CAML_BLOCK (0, data);
#define BA(x) ((x) != 0 ? build_ba (x) : Val_int (0))
  WRITE_FIELD (d_buf, BA);
  WRITE_FIELD (d_type, Val_int);
  WRITE_FIELD (d_size, copy_int32);
  WRITE_FIELD (d_off, copy_int32);
  WRITE_FIELD (d_align, copy_int32);
  WRITE_FIELD (d_version, Val_int);
  WRITE_FIELD_IM (elf_data, ID);
  END_CAML_BLOCK ();
  CAMLreturn (data);
}


