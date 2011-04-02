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
#undef __LIBELF_HEADER_ELF_H
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
#define Elf_val(v)        (*((Elf **)        Data_custom_val(v)))
#define Elf_Scn_val(v)    (*((Elf_Scn **)    Data_custom_val(v)))
#define Elf32_Ehdr_val(v) (*((Elf32_Ehdr **) Data_custom_val(v)))
#define Elf32_Phdr_val(v) (*((Elf32_Phdr **) Data_custom_val(v)))
#define Elf32_Shdr_val(v) (*((Elf32_Shdr **) Data_custom_val(v)))
#define GElf_Shdr_val(v)  (*((GElf_Shdr **)  Data_custom_val(v)))

//static value * elf_error_exn = NULL;

static void elf_error (char *cmdname) {
  //value res;
  value name = Val_unit, err = Val_unit;

  Begin_roots2 (name, err);
    /* name = copy_string (cmdname); */
  //    err = copy_string (elf_errmsg (-1));
    /* if (elf_error_exn == NULL) { */
    /*   elf_error_exn = caml_named_value("Elf.Elf_error"); */
    /*   if (elf_error_exn == NULL) */
    /*     invalid_argument("Exception Elf.Elf_error not initialized, please link elf.cma"); */
    /* } */
    /* res = alloc_small(3, 0); */
    /* Field(res, 0) = *elf_error_exn; */
    /* Field(res, 1) = name; */
    /* Field(res, 2) = err; */
    failwith(elf_errmsg (-1));
  End_roots();
  //  mlraise(res);
}

static value alloc_elf(Elf* s) {
  value v = alloc_custom(&elf_ops, sizeof(Elf *), 0, 1);
  Elf_val(v) = s;
  return v;
}

static value alloc_elf_scn(Elf_Scn* s) {
  value v = alloc_custom(&elf_ops, sizeof(Elf_Scn *), 0, 1);
  Elf_Scn_val(v) = s;
  return v;
}

static value alloc_elf32_ehdr(Elf32_Ehdr* s) {
  value v = alloc_custom(&elf_ops, sizeof(Elf32_Ehdr *), 0, 1);
  Elf32_Ehdr_val(v) = s;
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

CAMLprim value caml_elf_str_section (value e) {
  CAMLparam1 (e);
  Elf* elf = Elf_val (e);
  size_t shstrndx;
  if (elf_getshdrstrndx (elf, &shstrndx) != 0)
    elf_error ("elf_getshdrstrndx");
  Elf_Scn* scn = elf_getscn (elf, shstrndx);
  CAMLreturn (alloc_elf_scn (scn));
}

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
    Field(node, 0) = alloc_elf_scn (scn);
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
  CAMLreturn (alloc_elf32_ehdr (v));
}

struct { 
  char* name;
  unsigned int constant; 
  value hash;
} variants[] =
  {
    { "M32", EM_M32, 0},
    { "SPARC", EM_SPARC, 0},
    { "I386", EM_386, 0},
    { "M68K", EM_68K, 0},
    { "M88K", EM_88K, 0},
    //    { "I486", EM_486, 0},
    { "I860", EM_860, 0},
    { "MIPS", EM_MIPS, 0},
    { "S370", EM_S370, 0},
    { "MIPS", EM_MIPS, 0},
    //    { "SPARC64", EM_SPARC64, 0},
    { "PARISC", EM_PARISC, 0},
    { "VPP500", EM_VPP500, 0},
    { "SPARC32PLUS", EM_SPARC32PLUS, 0},
    //    { "I960", EM_I960, 0},
    { "PPC", EM_PPC, 0},
    { "PPC64", EM_PPC64, 0},
    { "S390", EM_S390, 0},
    { "V800", EM_V800, 0},
    { "FR20", EM_FR20, 0},
    { "RH32", EM_RH32, 0},
    { "RCE", EM_RCE, 0},
    { "ARM", EM_ARM, 0},
    { "ALPHA", EM_ALPHA, 0},
    { "SH", EM_SH, 0},
    { "SPARCV9", EM_SPARCV9, 0},
    { "TRICORE", EM_TRICORE, 0},
    { "ARC", EM_ARC, 0},
    //    { "H8", EM_H8, 0},
    //    { "H8", EM_H8, 0},
    { "H8S", EM_H8S, 0},
    //    { "H8", EM_H8, 0},
    //    { "IA", EM_IA, 0},
    { "MIPS", EM_MIPS, 0},
    { "COLDFIRE", EM_COLDFIRE, 0},
    //    { "M68HC12", EM_M68HC12, 0},
    { "MMA", EM_MMA, 0},
    { "PCP", EM_PCP, 0},
    { "NCPU", EM_NCPU, 0},
    { "NDR1", EM_NDR1, 0},
    { "STARCORE", EM_STARCORE, 0},
    { "ME16", EM_ME16, 0},
    { "ST100", EM_ST100, 0},
    { "TINYJ", EM_TINYJ, 0},
    //    { "X86", EM_X86, 0},
    //    { "AMD64", EM_AMD64, 0},
    { "PDSP", EM_PDSP, 0},
    { "FX66", EM_FX66, 0},
    { "ST9PLUS", EM_ST9PLUS, 0},
    { "ST7", EM_ST7, 0},
    //    { "M68HC16", EM_M68HC16, 0},
    //    { "M68HC11", EM_M68HC11, 0},
    //    { "M68HC08", EM_M68HC08, 0},
    //    { "M68HC05", EM_M68HC05, 0},
    { "SVX", EM_SVX, 0},
    { "ST19", EM_ST19, 0},
    { "VAX", EM_VAX, 0},
    { "CRIS", EM_CRIS, 0},
    { "JAVELIN", EM_JAVELIN, 0},
    { "FIREPATH", EM_FIREPATH, 0},
    { "ZSP", EM_ZSP, 0},
    { "MMIX", EM_MMIX, 0},
    { "HUANY", EM_HUANY, 0},
    { "PRISM", EM_PRISM, 0},
    { "AVR", EM_AVR, 0},
    { "FR30", EM_FR30, 0},
    { "D10V", EM_D10V, 0},
    { "D30V", EM_D30V, 0},
    { "V850", EM_V850, 0},
    { "M32R", EM_M32R, 0},
    { "MN10300", EM_MN10300, 0},
    { "MN10200", EM_MN10200, 0},
    { "PJ", EM_PJ, 0},
    { "OPENRISC", EM_OPENRISC, 0},
    { "ARC", EM_ARC, 0},
    { "XTENSA", EM_XTENSA, 0},
    //    { "VIDEOCORE", EM_VIDEOCORE, 0},
    //    { "TMM", EM_TMM, 0},
    //    { "NS32K", EM_NS32K, 0},
    //    { "TPC", EM_TPC, 0},
    //    { "SNP1K", EM_SNP1K, 0},
    //    { "ST200", EM_ST200, 0},
    //    { "IP2K", EM_IP2K, 0},
    //    { "MAX", EM_MAX, 0},
    //    { "CR", EM_CR, 0},
    //    { "F2MC16", EM_F2MC16, 0},
    //    { "MSP430", EM_MSP430, 0},
    //    { "BLACKFIN", EM_BLACKFIN, 0},
    //    { "SE", EM_SE, 0},
    //    { "SEP", EM_SEP, 0},
    //    { "ARCA", EM_ARCA, 0},
    //    { "UNICORE", EM_UNICORE, 0},
    { "NUM", EM_NUM, 0},
  };

static void init_polvariants () {
  int i;
  for (i=0; variants[0].hash == 0 
         && i < sizeof(variants) / sizeof(variants[0]); 
       i++)
    variants[i].hash = hash_variant (variants[i].name);
}

static unsigned int variant_to_enum (value hash)
{
  int i=0;
  init_polvariants ();
  for (i=0; 
       i < sizeof(variants) / sizeof(variants[0]); 
       i++)
    if (variants[i].hash == hash)
      return variants[i].constant;
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
  0,
  1,
  2,
  3,
  4,
  5,
  0xfe00,
  0xfeff,
  0xff00,
  0xffff
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

CAMLprim value caml_elf_elf32_put (value elf_header, value efhdr)
{
  CAMLparam2 (elf_header, efhdr);
  CAMLlocal1 (e_ident);
  Elf32_Ehdr* hdr = Elf32_Ehdr_val (efhdr);
  e_ident = Field (elf_header, 0);
  hdr->e_ident[0] = Int_val (Field (e_ident, 0));
  hdr->e_ident[1] = Int_val (Field (e_ident, 1));
  hdr->e_ident[2] = Int_val (Field (e_ident, 2));
  hdr->e_ident[3] = Int_val (Field (e_ident, 3));
  hdr->e_ident[4] = Int_val (Field (e_ident, 4));
  hdr->e_ident[5] = Int_val (Field (e_ident, 5));
  hdr->e_ident[6] = Int_val (Field (e_ident, 6));
  hdr->e_ident[7] = Int_val (Field (e_ident, 7));
  hdr->e_type      = et_tab[Int_val (Field (elf_header, 1))];
  hdr->e_machine   = variant_to_enum  (Field (elf_header, 2));
  hdr->e_version   = Int_val (Field (elf_header, 3));
  hdr->e_entry     = Int64_val (Field (elf_header, 4));
  hdr->e_phoff     = Int64_val (Field (elf_header, 5));
  hdr->e_shoff     = Int64_val (Field (elf_header, 6));
  hdr->e_flags     = Int_val (Field (elf_header, 7));
  hdr->e_ehsize    = Int64_val (Field (elf_header, 8));
  hdr->e_phentsize = Int64_val (Field (elf_header, 9));
  hdr->e_phnum     = Int_val (Field (elf_header, 10));
  hdr->e_shentsize = Int64_val (Field (elf_header, 11));
  hdr->e_shnum     = Int_val (Field (elf_header, 12));
  hdr->e_shstrndx  = Int_val (Field (elf_header, 12));
  CAMLreturn (Val_unit);
}

CAMLprim value caml_elf_elf32_get_internal (value efhdr, value elf_header)
{
  CAMLparam2 (efhdr, elf_header);
  CAMLlocal1 (e_ident);
  Elf32_Ehdr* hdr = Elf32_Ehdr_val (efhdr);
  e_ident = caml_alloc_small(8, 0);
  Field (e_ident, 0) = Int_val (hdr->e_ident[0]);
  Field (e_ident, 1) = Int_val (hdr->e_ident[1]);
  Field (e_ident, 2) = Int_val (hdr->e_ident[2]);
  Field (e_ident, 3) = Int_val (hdr->e_ident[3]);
  Field (e_ident, 4) = Int_val (hdr->e_ident[4]);
  Field (e_ident, 5) = Int_val (hdr->e_ident[5]);
  Field (e_ident, 6) = Int_val (hdr->e_ident[6]);
  Field (e_ident, 7) = Int_val (hdr->e_ident[7]);
  Field (elf_header, 0) = e_ident;
  Field (elf_header, 1) = Val_int (et_to_int (hdr->e_type));
  Field (elf_header, 2) = variant_to_enum  (hdr->e_machine);
  Field (elf_header, 3) = Val_int (hdr->e_version);
  Field (elf_header, 4) = copy_int64 (hdr->e_entry);
  Field (elf_header, 5) = copy_int64 (hdr->e_phoff);
  Field (elf_header, 6) = copy_int64 (hdr->e_shoff);
  Field (elf_header, 7) = Val_int (hdr->e_flags);
  Field (elf_header, 8) = copy_int64 (hdr->e_ehsize);
  Field (elf_header, 9) = copy_int64 (hdr->e_phentsize);
  Field (elf_header, 10) = Val_int (hdr->e_phnum);
  Field (elf_header, 11) = copy_int64 (hdr->e_shentsize);
  Field (elf_header, 12) = Val_int (hdr->e_shnum);
  Field (elf_header, 12) = Val_int (hdr->e_shstrndx);
  CAMLreturn (Val_unit);
}

static int pt_tab[] = 
{
  0,1,2,3,4,5,6,7,8,
  0x60000000,
  0x6fffffff,
  0x70000000,
  0x7fffffff
};

  
CAMLprim value caml_elf_ph (value elf)
{
}
