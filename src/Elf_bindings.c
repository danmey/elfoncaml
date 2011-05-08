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
    return v; }

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

CAMLprim value caml_elf_version (value version) {
  CAMLparam1 (version);
  CAMLreturn (Val_int (elf_version (Int_val (version))));
}

static inline void* handle_null_option (value option) {
  if (Is_block (option))
    return (void*)Field (option, 0);
  return 0;
}

static inline value null_option (value ptr)
{
  value option = Val_int (0);
  if (ptr) {
    option = caml_alloc_small (2, 0);
    Field(option, 0) = Val_int(1);
    Field(option, 1) = ptr;
  }
  return option;
}

#define Val_null_option(conv, ptr) null_option (conv (ptr))
  
#define ml_fun1(ret, name, arg1)                            \
  CAMLprim value caml_##name (value _a1) {                          \
    CAMLparam1 (_a1); \
    CAMLreturn (ret (name (arg1 (_a1)))); }

#define ml_internal_fun1(ret, name, arg1)                            \
  CAMLprim value caml_##name (value _a1) {                          \
    CAMLparam1 (_a1); \
    CAMLreturn (ret (caml_internal_##name (arg1 (_a1)))); }

#define ml_internal_fun3(ret, name, arg1, arg2, arg3)                            \
  CAMLprim value caml_##name (value _a1, value _a2, value _a3) {                          \
    CAMLparam3 (_a1, _a2, _a3); \
    CAMLreturn (ret (caml_internal_##name (arg1 (_a1), arg2 (_a2), arg3 (_a3)))); }
    
CAMLprim Elf* caml_internal_elf_begin (int fd, int cmd, Elf* ref) {
  Elf* elf = elf_begin (fd, cmd, ref);
  if (elf == 0)
    elf_error ("elf_begin");
  return elf;
}

CAMLprim Elf_Scn* caml_internal_str_section (Elf* elf) {
  size_t shstrndx;
  if (elf_getshdrstrndx (elf, &shstrndx) != 0)
    elf_error ("elf_getshdrstrndx");
  return elf_getscn (elf, shstrndx);
}

ml_internal_fun3 (alloc_Elf, elf_begin, Int_val, Int_val, handle_null_option);
ml_internal_fun1 (alloc_Elf_Scn, str_section, Elf_val);
ml_fun1 (Val_int, elf_kind, Elf_val);
//ml_fun1 (alloc_Elf32_Shdr, gelf_getshdr, Elf_Scn_val);
//ml_fun3 (alloc_string, elf_strptr, Elf_val, Int_val, String_val);

CAMLprim value caml_elf_sections (value e) {
  CAMLparam1 (e);
  CAMLlocal2 (list, node);
  Elf* elf = Elf_val (e);
  Elf_Scn* scn = 0;
  scn = elf_nextscn (elf , scn);
  if (!scn)
    CAMLreturn (Val_int (0));
  list = caml_alloc_small(2, 0);
  node = list;
  while (scn) {
    Field(node, 0) = alloc_Elf_Scn (scn);
    scn = elf_nextscn (elf , scn);
    if (scn != 0) {
      Field(node, 1) = caml_alloc_small(2, 0);
      node = Field(node, 1);
    } else {
        Field(node, 1) = Val_int (0);
      }
  }
  CAMLreturn (list);
}

CAMLprim value caml_elf_section_name (value elf, value section, value str_section) {
  CAMLparam3 (elf, section, str_section);
  GElf_Shdr shdr;
  if ( gelf_getshdr (Elf_Scn_val (section), &shdr) != &shdr)
    elf_error ("gelf_getshdr");
  char* name = elf_strptr
    (Elf_val (elf),
     elf_ndxscn (Elf_Scn_val (str_section)),
     shdr.sh_name);
  if (!name)
    elf_error ("gelf_strptr");
  CAMLreturn (copy_string (name));
}

CAMLprim value caml_elf_section_index (value section) {
  CAMLparam1 (section);
  CAMLreturn (Val_int (elf_ndxscn (Elf_Scn_val (section))));
}

CAMLprim value caml_elf_section_size (value section) {
  CAMLparam1 (section);
  GElf_Shdr shdr;
  if ( gelf_getshdr (Elf_Scn_val (section), &shdr) != &shdr)
    elf_error ("gelf_getshdr");
  CAMLreturn (Val_int (shdr.sh_size));
}

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

CAMLprim value caml_elf_elf32_header (value elf) {
  CAMLparam1 (elf);
  Elf32_Ehdr* v = elf32_newehdr (Elf_val (elf));
  if (!v) elf_error ("elf32_newehdr");
  CAMLreturn (alloc_Elf32_Ehdr (v));
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

#define BEGIN_CAML_BLOCK(f,x) do { int _field = f; value _hdr = x;
#define END_CAML_BLOCK()  } while (0)
#define READ_FIELD(name, convert) hdr->name = convert (Field (_hdr, _field)); _field++;
#define WRITE_FIELD(name, convert) Field (_hdr, _field) = convert (hdr->name); _field++;
#define WRITE_FIELD_IM(name, convert) Field (_hdr, _field) = convert (name); _field++;

CAMLprim value caml_elf_elf32_put (value efhdr, value elf_header)
{
  CAMLparam2 (efhdr, elf_header);
  CAMLlocal1 (e_ident);
  Elf32_Ehdr* hdr = Elf32_Ehdr_val (efhdr);
  e_ident = Field (elf_header, 0);
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
  BEGIN_CAML_BLOCK (1, elf_header);
  READ_FIELD(e_type, ET_TAB);
  READ_FIELD(e_machine, variant_to_enum);
  READ_FIELD(e_version   , Int_val);
  READ_FIELD(e_entry     , Int64_val);
  READ_FIELD(e_phoff     , Int64_val);
  READ_FIELD(e_shoff     , Int64_val);
  READ_FIELD(e_flags     , Int64_val);
  READ_FIELD(e_ehsize    , Int_val);
  READ_FIELD(e_phentsize , Int_val);
  READ_FIELD(e_phnum     , Int_val);
  READ_FIELD(e_shentsize , Int_val);
  READ_FIELD(e_shnum     , Int_val);
  READ_FIELD(e_shstrndx  , Int_val);
  END_CAML_BLOCK ();
  CAMLreturn (Val_unit);
}

CAMLprim value caml_elf_elf32_get (value efhdr)
{
  CAMLparam1 (efhdr);
  CAMLlocal2 (e_ident, elf_header);
  Elf32_Ehdr* hdr = Elf32_Ehdr_val (efhdr);
  e_ident = caml_alloc_small(8, 0);
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
  elf_header = caml_alloc_small(15, 0);
#define ET_TO_INT(x) Val_int (et_to_int (x));
#define ID(x) x
  BEGIN_CAML_BLOCK (0, elf_header);
  WRITE_FIELD_IM (e_ident, ID);
  WRITE_FIELD (e_type, ET_TO_INT);
  WRITE_FIELD (e_machine, enum_to_variant);
  WRITE_FIELD (e_version, Val_int);
  WRITE_FIELD (e_entry, copy_int64);
  WRITE_FIELD (e_phoff, copy_int64);
  WRITE_FIELD (e_shoff, copy_int64);
  WRITE_FIELD (e_flags, copy_int64);
  WRITE_FIELD (e_ehsize, Val_int);
  WRITE_FIELD (e_phentsize, Val_int);
  WRITE_FIELD (e_phnum, Val_int);
  WRITE_FIELD (e_shentsize, Val_int);
  WRITE_FIELD (e_shnum, Val_int);
  WRITE_FIELD (e_shstrndx, Val_int);
  WRITE_FIELD_IM (efhdr, ID);
  END_CAML_BLOCK ();
  CAMLreturn (elf_header);
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


CAMLprim value caml_elf_ph (value elf)
{
  CAMLparam1 (elf);
  Elf32_Phdr* v = elf32_newphdr (Elf_val (elf), 1);
  if (!v) elf_error ("elf32_newphdr");
  CAMLreturn (alloc_Elf32_Phdr (v));
}

CAMLprim value caml_elf_ph_put (value elf_header, value phdr)
{
  CAMLparam2 (elf_header, phdr);
  Elf32_Phdr* hdr = Elf32_Phdr_val (phdr);
  BEGIN_CAML_BLOCK (0, elf_header);
#define PT_TAB(x) pt_tab[Int_val (x)]
  READ_FIELD (p_type, PT_TAB);
  READ_FIELD (p_offset, Int64_val);
  READ_FIELD (p_vaddr , Int64_val);
  READ_FIELD (p_paddr , Int64_val);
  READ_FIELD (p_filesz, Int_val);
  READ_FIELD (p_memsz , Int_val);
  READ_FIELD (p_flags , Int_val);
  READ_FIELD (p_align , Int_val);
  END_CAML_BLOCK ();
  CAMLreturn (Val_unit);
}

CAMLprim value caml_elf_ph_get_internal (value phdr, value elf_phdr)
{
  CAMLparam2 (phdr, elf_phdr);
  Elf32_Phdr* hdr = Elf32_Phdr_val (phdr);
  Field (elf_phdr, 0) = Val_int (pt_to_int (hdr->p_type));
  Field (elf_phdr, 1) = copy_int64 (hdr->p_offset);
  Field (elf_phdr, 2) = copy_int64 (hdr->p_vaddr);
  Field (elf_phdr, 3) = copy_int64 (hdr->p_paddr);
  Field (elf_phdr, 4) = Val_int (hdr->p_filesz);
  Field (elf_phdr, 5) = Val_int (hdr->p_memsz);
  Field (elf_phdr, 6) = Val_int (hdr->p_flags);
  Field (elf_phdr, 7) = Val_int (hdr->p_align);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_elf_elf32_getshdr (value section)
{
  CAMLparam1 (section);
  Elf32_Shdr* shdr = 0;
  if ( (shdr = elf32_getshdr (Elf_Scn_val (section))) == 0)
    elf_error ("elf_getshdr");
  CAMLreturn (alloc_Elf32_Shdr (shdr));
}

CAMLprim value caml_elf_sh_put (value shdr, value elf_shdr)
{
  CAMLparam2 (shdr, elf_shdr);
  Elf32_Shdr* hdr = Elf32_Shdr_val (shdr);
  hdr->sh_name      = Int32_val (Field (elf_shdr, 0));
  hdr->sh_type      = sht_tab [Int_val (Field (elf_shdr, 1))];
  hdr->sh_flags     = mlflags_to_int (Field (elf_shdr, 2));
  hdr->sh_addr      = Int64_val (Field (elf_shdr, 3));
  hdr->sh_offset    = Int64_val (Field (elf_shdr, 4));
  hdr->sh_size      = Int32_val (Field (elf_shdr, 5));
  hdr->sh_link      = Int32_val (Field (elf_shdr, 6));
  hdr->sh_info      = Int32_val (Field (elf_shdr, 7));
  hdr->sh_addralign = Int32_val (Field (elf_shdr, 8));
  hdr->sh_entsize   = Int32_val (Field (elf_shdr, 9));
  CAMLreturn (Val_unit);
}

CAMLprim value caml_elf_sh_get (value shdr)
{
  CAMLparam1 (shdr);
  CAMLlocal1 (elf_shdr);
  Elf32_Shdr* hdr = Elf32_Shdr_val (shdr);
  elf_shdr  = caml_alloc_small(11, 0);
  Field (elf_shdr, 0) = copy_int32 (hdr->sh_name);
  Field (elf_shdr, 1) = Val_int (sht_to_int (hdr->sh_type));
  Field (elf_shdr, 2) = int_to_mlflags (hdr->sh_flags);
  Field (elf_shdr, 3) = copy_int64 (hdr->sh_addr);
  Field (elf_shdr, 4) = copy_int64 (hdr->sh_offset);
  Field (elf_shdr, 5) = copy_int32 (hdr->sh_size);
  Field (elf_shdr, 6) = copy_int32 (hdr->sh_link);
  Field (elf_shdr, 7) = copy_int32 (hdr->sh_info);
  Field (elf_shdr, 8) = copy_int32 (hdr->sh_addralign);
  Field (elf_shdr, 9) = copy_int32 (hdr->sh_entsize);
  Field (elf_shdr, 10) = shdr;
  CAMLreturn (elf_shdr);
}

CAMLprim value caml_elf_newscn (value elf)
{
  CAMLparam1 (elf);
  Elf_Scn* scn = elf_newscn (Elf_val (elf));
  CAMLreturn (alloc_Elf_Scn (scn));
}

CAMLprim value caml_elf_newdata (value scn)
{
  CAMLparam1 (scn);
  Elf_Data* data = elf_newdata (Elf_Scn_val (scn));
  CAMLreturn (alloc_Elf_Data (data));
}

CAMLprim value caml_elf_data_put (value elf_data, value data)
{
  CAMLparam2 (elf_data, data);
  Elf_Data* hdr = Elf_Data_val (elf_data);
  BEGIN_CAML_BLOCK (0, data);
#define BA(x) (Is_block(x) ? Data_bigarray_val (Field(x,0)) : 0)
  READ_FIELD (d_buf, BA);
#undef BA
  READ_FIELD (d_type, Int_val);
  READ_FIELD (d_size, Int64_val);
  READ_FIELD (d_off, Int64_val);
  READ_FIELD (d_align, Int64_val);
  READ_FIELD (d_version, Int_val);
  END_CAML_BLOCK ();
  CAMLreturn (Val_unit);
}

value build_ba (void *x)
{
  CAMLparam0();
  CAMLlocal1(option);
  option = Val_int(0);
  /* option = caml_alloc_small (2, 0); */
  /* Field(option, 0) = Val_int(1); */
  //  Field(option, 1) = caml_ba_alloc(CAML_BA_UINT8|CAML_BA_C_LAYOUT|CAML_BA_EXTERNAL, 1, x, 0);
  CAMLreturn (option);
}

CAMLprim value caml_elf_data_get (value elf_data)
{
  CAMLparam1 (elf_data);
  CAMLlocal1 (elf_header);
  Elf_Data* hdr = Elf_Data_val (elf_data);
  elf_header = caml_alloc_small(7, 0);
  BEGIN_CAML_BLOCK (0, elf_header);
#define BA(x) ((x) != 0 ? build_ba (x) : Val_int (0))
  WRITE_FIELD (d_buf, BA);
  WRITE_FIELD (d_type, Val_int);
  WRITE_FIELD (d_size, copy_int64);
  WRITE_FIELD (d_off, copy_int64);
  WRITE_FIELD (d_align, copy_int64);
  WRITE_FIELD (d_version, Val_int);
  WRITE_FIELD_IM (elf_data, ID);
  END_CAML_BLOCK ();
  CAMLreturn (elf_header);
}

CAMLprim value caml_elf_set_str_section_index  (value e, value index) {
  CAMLparam2 (e, index);
  Elf* elf = Elf_val (e);
  elfx_update_shstrndx (elf, Int_val (index));
  CAMLreturn (Val_unit);
  }

CAMLprim value caml_elf_update (value e, value ec) {
  CAMLparam2 (e, ec);
  Elf* elf = Elf_val (e);
  if (elf_update (elf, Int_val (ec)) < 0)
    elf_error ("elf_update");
  CAMLreturn (Val_unit);
}

CAMLprim value caml_elf_fsize (value t, value c, value v) {
  CAMLparam3 (t, c, v);
  CAMLreturn (Val_int (elf32_fsize (Int_val (t), Int32_val (c), Int_val(v))));
}

unsigned long f_tab[] = {ELF_F_DIRTY, ELF_F_LAYOUT, ELF_F_LAYOUT_OVERLAP};

unsigned long f_to_int(int v)
{
  int i;
  for (i=0; i < sizeof(f_tab)/sizeof(f_tab[0]); i++)
    if (f_tab[i] == v)
      return i;
  failwith ("f_to_int: Wrong enum.");
  return 0;
}

CAMLprim value caml_elf_program_header_flags (value e, value ec, value f)
{
  CAMLparam3 (e, ec, f);
  Elf* elf = Elf_val (e);
  elf_flagphdr (elf, Int_val (ec), f_tab [Int_val (f)]);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_elf_ends (value e)
{
  CAMLparam1 (e);
  Elf* elf = Elf_val (e);
  elf_end (elf);
  CAMLreturn (Val_unit);
}
