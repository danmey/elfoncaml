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
#define Elf_Data_val(v) (*((Elf_Data **) Data_custom_val(v)))

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

static value alloc_elf32_phdr(Elf32_Phdr* s) {
  value v = alloc_custom(&elf_ops, sizeof(Elf32_Phdr *), 0, 1);
  Elf32_Phdr_val(v) = s;
  return v;
}

static value alloc_elf32_shdr(Elf32_Shdr* s) {
  value v = alloc_custom(&elf_ops, sizeof(Elf32_Shdr *), 0, 1);
  Elf32_Shdr_val(v) = s;
  return v;
}

static value alloc_elf32_scn(Elf_Scn* s) {
  value v = alloc_custom(&elf_ops, sizeof(Elf_Scn *), 0, 1);
  Elf_Scn_val(v) = s;
  return v;
}

static value alloc_elf32_data(Elf_Data* s) {
  value v = alloc_custom(&elf_ops, sizeof(Elf_Data *), 0, 1);
  Elf_Data_val(v) = s;
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
    { "NONE", EM_NONE, 0},
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

static int pt_tab[] = 
{
  0,1,2,3,4,5,6,7,8,
  0x60000000,
  0x6fffffff,
  0x70000000,
  0x7fffffff
};

int pt_to_int(int v)
{
  int i;
  for (i=0; i < sizeof(pt_tab)/sizeof(pt_tab[0]); i++)
    if (pt_tab[i] == v)
      return i;
  failwith ("pt_to_int: Wrong enum.");
  return 0;
}

  
CAMLprim value caml_elf_ph (value elf)
{
  CAMLparam1 (elf);
  Elf32_Phdr* v = elf32_newphdr (Elf_val (elf), 1);
  if (!v) elf_error ("elf32_newphdr");
  CAMLreturn (alloc_elf32_phdr (v));
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
  CAMLreturn (alloc_elf32_shdr (shdr));
}
  
CAMLprim value caml_elf_sh_put (value elf_shdr, value shdr)
{
  CAMLparam2 (elf_shdr, shdr);
  Elf32_Shdr* hdr = Elf32_Shdr_val (shdr);
  hdr->sh_name      = Int32_val (Field (elf_shdr, 0));
  hdr->sh_type      = Int32_val (Field (elf_shdr, 1));
  hdr->sh_flags     = Int32_val (Field (elf_shdr, 2));
  hdr->sh_addr      = Int64_val (Field (elf_shdr, 3));
  hdr->sh_offset    = Int64_val (Field (elf_shdr, 4));
  hdr->sh_size      = Int32_val (Field (elf_shdr, 5));
  hdr->sh_link      = Int32_val (Field (elf_shdr, 6));
  hdr->sh_info      = Int32_val (Field (elf_shdr, 7));
  hdr->sh_addralign = Int32_val (Field (elf_shdr, 8));
  hdr->sh_entsize   = Int32_val (Field (elf_shdr, 9));
  CAMLreturn (Val_unit);
}

CAMLprim value caml_elf_sh_get_internal (value shdr, value elf_shdr)
{
  CAMLparam2 (shdr, elf_shdr);
  Elf32_Shdr* hdr = Elf32_Shdr_val (shdr);
  Field (elf_shdr, 0) = copy_int32 (hdr->sh_name);
  Field (elf_shdr, 1) = copy_int32 (hdr->sh_type);
  Field (elf_shdr, 2) = copy_int32 (hdr->sh_flags);
  Field (elf_shdr, 3) = copy_int64 (hdr->sh_addr);
  Field (elf_shdr, 4) = copy_int64 (hdr->sh_offset);
  Field (elf_shdr, 5) = copy_int32 (hdr->sh_size);
  Field (elf_shdr, 6) = copy_int32 (hdr->sh_link);
  Field (elf_shdr, 7) = copy_int32 (hdr->sh_info);
  Field (elf_shdr, 8) = copy_int32 (hdr->sh_addralign);
  Field (elf_shdr, 9) = copy_int32 (hdr->sh_entsize);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_elf_newscn (value elf)
{
  CAMLparam1 (elf);
  Elf_Scn* scn = elf_newscn (Elf_val (elf));
  CAMLreturn (alloc_elf32_scn (scn));
}

CAMLprim value caml_elf_newdata (value scn)
{
  CAMLparam1 (scn);
  Elf_Data* data = elf_newdata (Elf_Scn_val (scn));
  CAMLreturn (alloc_elf32_data (data));
}
