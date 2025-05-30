#include "elf.h"

#include "../compiler.h"
#include "../log.h"
#include "../util.h"
#include "../vector.h"
#include "elf_types.h"

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

struct reloc_info {
  size_t num_text_reloc_instrs;
  size_t num_const_data_reloc_instrs;
  size_t num_data_reloc_instrs;

  struct vector *text_relocs;
  struct vector *const_data_relocs;
  struct vector *data_relocs;
};

static void safe_fseek_set(FILE *file, long offset);

static void write_header_64(FILE *file, Elf64_Half e_machine,
                            Elf64_Word e_flags, Elf64_Off shoff) {
  Elf64_Ehdr ehdr;
  memset(&ehdr, 0, sizeof(ehdr));
  ehdr.e_ident[EI_MAG0] = 0x7f;
  ehdr.e_ident[EI_MAG1] = 'E';
  ehdr.e_ident[EI_MAG2] = 'L';
  ehdr.e_ident[EI_MAG3] = 'F';
  ehdr.e_ident[EI_DATA] = ELFDATA2LSB;
  ehdr.e_ident[EI_VERSION] = EV_CURRENT;
  ehdr.e_ident[EI_OSABI] = ELFOSABI_NONE;
  ehdr.e_type = ET_REL;
  ehdr.e_ident[EI_CLASS] = ELFCLASS64;
  ehdr.e_machine = e_machine;
  ehdr.e_flags = e_flags;
  ehdr.e_version = EV_CURRENT;
  ehdr.e_ehsize = sizeof(Elf64_Ehdr);
  ehdr.e_shoff = shoff;
  ehdr.e_shentsize = sizeof(Elf64_Shdr);
  ehdr.e_shnum = 10;
  ehdr.e_shstrndx = 9;
  safe_fseek_set(file, 0);
  fwrite(&ehdr, sizeof(ehdr), 1, file);
}

static void write_header_32(FILE *file, Elf32_Half e_machine,
                            Elf32_Word e_flags, Elf32_Off shoff) {
  Elf32_Ehdr ehdr;
  memset(&ehdr, 0, sizeof(ehdr));
  ehdr.e_ident[EI_MAG0] = 0x7f;
  ehdr.e_ident[EI_MAG1] = 'E';
  ehdr.e_ident[EI_MAG2] = 'L';
  ehdr.e_ident[EI_MAG3] = 'F';
  ehdr.e_ident[EI_DATA] = ELFDATA2LSB;
  ehdr.e_ident[EI_VERSION] = EV_CURRENT;
  ehdr.e_ident[EI_OSABI] = ELFOSABI_NONE;
  ehdr.e_type = ET_REL;
  ehdr.e_ident[EI_CLASS] = ELFCLASS32;
  ehdr.e_machine = e_machine;
  ehdr.e_flags = e_flags;
  ehdr.e_version = EV_CURRENT;
  ehdr.e_ehsize = sizeof(Elf32_Ehdr);
  ehdr.e_shoff = shoff;
  ehdr.e_shentsize = sizeof(Elf32_Shdr);
  ehdr.e_shnum = 10;
  ehdr.e_shstrndx = 9;
  safe_fseek_set(file, 0);
  fwrite(&ehdr, sizeof(ehdr), 1, file);
}

static struct reloc_info build_reloc_info(const struct build_object_args *args,
                                          const size_t *entry_offsets) {
  struct reloc_info info = {
      .text_relocs = vector_create(sizeof(struct relocation)),
      .data_relocs = vector_create(sizeof(struct relocation)),
      .const_data_relocs = vector_create(sizeof(struct relocation)),
  };

  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *entry = &args->entries[i];

    if (!entry->num_relocations) {
      continue;
    }

    size_t *num_relocs;
    struct vector *relocs;

    switch (entry->ty) {
    case OBJECT_ENTRY_TY_FUNC:
      num_relocs = &info.num_text_reloc_instrs;
      relocs = info.text_relocs;
      break;
    case OBJECT_ENTRY_TY_CONST_DATA:
      num_relocs = &info.num_const_data_reloc_instrs;
      relocs = info.const_data_relocs;
      break;
    case OBJECT_ENTRY_TY_MUT_DATA:
      num_relocs = &info.num_data_reloc_instrs;
      relocs = info.data_relocs;
      break;
    case OBJECT_ENTRY_TY_C_STRING:
    case OBJECT_ENTRY_TY_DECL:
      BUG("reloc for cstring/decl makes no sense");
    }

    size_t entry_offset = entry_offsets[i];

    for (size_t j = 0; j < entry->num_relocations; j++) {
      struct relocation reloc = entry->relocations[j];
      reloc.address += entry_offset;

      vector_push_back(relocs, &reloc);

      switch (reloc.ty) {
      case RELOCATION_TY_POINTER:
      case RELOCATION_TY_CALL:
      case RELOCATION_TY_LOCAL_SINGLE:
      case RELOCATION_TY_UNDEF_SINGLE:
        *num_relocs += 1;
        break;
      case RELOCATION_TY_LOCAL_PAIR:
      case RELOCATION_TY_UNDEF_PAIR:
        *num_relocs += 2;
      }
    }
  }

  return info;
}

static void write_relocations_elf(FILE *file,
                                  const struct build_object_args *args,
                                  const size_t *sym_id_to_idx,
                                  struct vector *relocs,
                                  enum compile_target target) {
  size_t n = vector_length(relocs);
  for (size_t i = 0; i < n; i++) {
    const struct relocation *r = vector_get(relocs, i);
    switch (r->ty) {
    case RELOCATION_TY_POINTER: {
      uint32_t type;
      switch (target) {
      case COMPILE_TARGET_LINUX_X86_64:
        type = R_X86_64_64;
        goto simple_reloc;
      case COMPILE_TARGET_LINUX_ARM64:
        type = R_AARCH64_ABS64;
        goto simple_reloc;

      simple_reloc: {
        Elf64_Rela rela;
        memset(&rela, 0, sizeof(rela));
        rela.r_offset = r->address;
        rela.r_addend = r->offset;
        rela.r_info = ELF64_R_INFO(sym_id_to_idx[r->symbol_index], type);
        fwrite(&rela, sizeof(rela), 1, file);
        break;
      }
      case COMPILE_TARGET_LINUX_RV32I: {
        // FIXME: allow linker relaxation by doing branches as relocations

        Elf32_Rela rela;
        memset(&rela, 0, sizeof(rela));
        rela.r_offset = r->address;
        rela.r_addend = r->offset;
        rela.r_info = ELF32_R_INFO(sym_id_to_idx[r->symbol_index], R_RISCV_32);
        fwrite(&rela, sizeof(rela), 1, file);

        // memset(&rela, 0, sizeof(rela));
        // rela.r_offset = r->address;
        // rela.r_addend = r->offset;
        // rela.r_info = ELF32_R_INFO(0, R_RISCV_RELAX);
        // fwrite(&rela, sizeof(rela), 1, file);
        break;
      }
      default:
        unreachable();
      }
      break;
    }
    case RELOCATION_TY_CALL: {
      const struct symbol *symbol = &args->entries[r->symbol_index].symbol;

      switch (target) {
      case COMPILE_TARGET_LINUX_X86_64:
      case COMPILE_TARGET_LINUX_ARM64: {
        Elf64_Rela rela;
        memset(&rela, 0, sizeof(rela));
        rela.r_offset = r->address;
        // for some reason x64 needs a -4 addend on call relocs
        rela.r_addend = (target == COMPILE_TARGET_LINUX_X86_64) ? -4 : 0;

        uint32_t type;
        if (symbol->visibility == SYMBOL_VISIBILITY_UNDEF) {
          type = (target == COMPILE_TARGET_LINUX_X86_64) ? R_X86_64_PLT32
                                                         : R_AARCH64_CALL26;
        } else {
          type = (target == COMPILE_TARGET_LINUX_X86_64) ? R_X86_64_PC32
                                                         : R_AARCH64_CALL26;
        }
        rela.r_info = ELF64_R_INFO(sym_id_to_idx[r->symbol_index], type);
        fwrite(&rela, sizeof(rela), 1, file);
        break;
      }
      case COMPILE_TARGET_LINUX_RV32I: {
        Elf32_Rela rela;
        memset(&rela, 0, sizeof(rela));
        rela.r_offset = r->address;
        rela.r_addend = 0;
        rela.r_info = ELF32_R_INFO(sym_id_to_idx[r->symbol_index],
                                   symbol->visibility == SYMBOL_VISIBILITY_UNDEF
                                       ? R_RISCV_CALL_PLT
                                       : R_RISCV_JAL);
        fwrite(&rela, sizeof(rela), 1, file);

        memset(&rela, 0, sizeof(rela));
        rela.r_offset = r->address;
        rela.r_addend = 0;
        rela.r_info = ELF32_R_INFO(0, R_RISCV_RELAX);
        // fwrite(&rela, sizeof(rela), 1, file);
        break;
      }
      default:
        unreachable();
      }

      break;
    }
    case RELOCATION_TY_LOCAL_SINGLE: {
      Elf64_Rela rela;
      memset(&rela, 0, sizeof(rela));
      rela.r_offset = r->address;
      rela.r_addend = r->offset - 4;
      rela.r_info = ELF64_R_INFO(sym_id_to_idx[r->symbol_index], R_X86_64_PC32);
      fwrite(&rela, sizeof(rela), 1, file);
      break;
    }
    case RELOCATION_TY_LOCAL_PAIR: {
      if (target == COMPILE_TARGET_LINUX_ARM64) {
        Elf64_Rela rela1;
        Elf64_Rela rela2;
        memset(&rela1, 0, sizeof(rela1));
        memset(&rela2, 0, sizeof(rela2));
        rela1.r_offset = r->address;
        rela1.r_addend = r->offset;
        rela1.r_info = ELF64_R_INFO(sym_id_to_idx[r->symbol_index],
                                    R_AARCH64_ADR_PREL_PG_HI21);
        rela2.r_offset = r->address + 4;
        rela2.r_addend = r->offset;
        rela2.r_info = ELF64_R_INFO(sym_id_to_idx[r->symbol_index],
                                    R_AARCH64_ADD_ABS_LO12_NC);
        fwrite(&rela1, sizeof(rela1), 1, file);
        fwrite(&rela2, sizeof(rela2), 1, file);
      } else if (target == COMPILE_TARGET_LINUX_RV32I) {
        Elf32_Rela rela1;
        Elf32_Rela rela2;
        memset(&rela1, 0, sizeof(rela1));
        memset(&rela2, 0, sizeof(rela2));
        rela1.r_offset = r->address;
        rela1.r_addend = r->offset;
        rela1.r_info =
            ELF32_R_INFO(sym_id_to_idx[r->symbol_index], R_RISCV_HI20);
        rela2.r_offset = r->address + 4;
        rela2.r_addend = r->offset;
        rela2.r_info =
            ELF32_R_INFO(sym_id_to_idx[r->symbol_index], R_RISCV_LO12_I);

        Elf32_Rela rela;
        memset(&rela, 0, sizeof(rela));
        rela.r_offset = r->address;
        rela.r_addend = 0;
        rela.r_info = ELF32_R_INFO(0, R_RISCV_RELAX);
        fwrite(&rela1, sizeof(rela1), 1, file);
        // fwrite(&rela, sizeof(rela), 1, file);

        memset(&rela, 0, sizeof(rela));
        rela.r_offset = r->address + 4;
        rela.r_addend = 0;
        rela.r_info = ELF32_R_INFO(0, R_RISCV_RELAX);
        fwrite(&rela2, sizeof(rela2), 1, file);
        // fwrite(&rela, sizeof(rela), 1, file);
      } else {
        BUG("two-part reloc not supported for x86_64 in elf");
      }
      break;
    }
    case RELOCATION_TY_UNDEF_SINGLE: {
      Elf64_Rela rela;
      memset(&rela, 0, sizeof(rela));
      rela.r_offset = r->address;
      rela.r_addend = r->offset - 4;
      rela.r_info =
          ELF64_R_INFO(sym_id_to_idx[r->symbol_index], R_X86_64_GOTPCREL);
      fwrite(&rela, sizeof(rela), 1, file);
      break;
    }
    case RELOCATION_TY_UNDEF_PAIR: {
      if (target == COMPILE_TARGET_LINUX_ARM64) {
        Elf64_Rela rela1;
        Elf64_Rela rela2;
        memset(&rela1, 0, sizeof(rela1));
        memset(&rela2, 0, sizeof(rela2));
        rela1.r_offset = r->address;
        rela1.r_addend = r->offset;
        rela1.r_info = ELF64_R_INFO(sym_id_to_idx[r->symbol_index],
                                    R_AARCH64_ADR_GOT_PAGE);
        rela2.r_offset = r->address + 4;
        rela2.r_addend = r->offset;
        rela2.r_info = ELF64_R_INFO(sym_id_to_idx[r->symbol_index],
                                    R_AARCH64_LD64_GOT_LO12_NC);
        fwrite(&rela1, sizeof(rela1), 1, file);
        fwrite(&rela2, sizeof(rela2), 1, file);
      } else if (target == COMPILE_TARGET_LINUX_RV32I) {
        Elf32_Rela rela1;
        Elf32_Rela rela2;
        memset(&rela1, 0, sizeof(rela1));
        memset(&rela2, 0, sizeof(rela2));
        rela1.r_offset = r->address;
        rela1.r_addend = r->offset;
        rela1.r_info =
            ELF32_R_INFO(sym_id_to_idx[r->symbol_index], R_RISCV_HI20);
        rela2.r_offset = r->address + 4;
        rela2.r_addend = r->offset;
        rela2.r_info =
            ELF32_R_INFO(sym_id_to_idx[r->symbol_index], R_RISCV_LO12_I);

        Elf32_Rela rela;
        memset(&rela, 0, sizeof(rela));
        rela.r_offset = r->address;
        rela.r_addend = 0;
        rela.r_info = ELF32_R_INFO(0, R_RISCV_RELAX);
        fwrite(&rela1, sizeof(rela1), 1, file);
        // fwrite(&rela, sizeof(rela), 1, file);

        memset(&rela, 0, sizeof(rela));
        rela.r_offset = r->address + 4;
        rela.r_addend = 0;
        rela.r_info = ELF32_R_INFO(0, R_RISCV_RELAX);
        fwrite(&rela2, sizeof(rela2), 1, file);
        // fwrite(&rela, sizeof(rela), 1, file);
      } else {
        BUG("two-part reloc not supported for x86_64 in elf");
      }
      break;
    }
    }
  }
}

static void safe_fseek_set(FILE *file, long offset) {
  // this function is no longer needed as the writer is written to be contigous

#ifdef __unix__
  fseek(file, offset, SEEK_SET);
  return;
#else
  long pos = ftell(file);

  invariant_assert(pos != -1, "ftell failed but was needed for safe_fseek_set");

  if (offset < pos) {
    BUG("cant go back");
    invariant_assert(fseek(file, offset, SEEK_SET),
                     "fseek failed! (offset=%ld, pos=%ld)", offset, pos);
    return;
  }

  if (!pos) {
    return;
  }

  long rem = offset - pos;

  for (long i = 0; i < rem; i++) {
    fputc(0, file);
  }

  DEBUG_ASSERT(ftell(file) == offset, "seek failed?");
#endif
}

static void write_elf_object(const struct build_object_args *args) {
  FILE *file = args->output;

  invariant_assert(file, "could not open object output file");

  size_t *entry_offsets =
      nonnull_malloc(args->num_entries * sizeof(*entry_offsets));

  size_t text_align = 1;
  size_t const_align = 1;
  size_t data_align = 1;

  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *e = &args->entries[i];
    switch (e->ty) {
    case OBJECT_ENTRY_TY_FUNC:
      text_align = MAX(text_align, e->alignment);
      break;
    case OBJECT_ENTRY_TY_C_STRING:
    case OBJECT_ENTRY_TY_CONST_DATA:
      const_align = MAX(const_align, e->alignment);
      break;
    case OBJECT_ENTRY_TY_MUT_DATA:
      data_align = MAX(data_align, e->alignment);
      break;
    case OBJECT_ENTRY_TY_DECL:
      break;
    }
  }

  size_t total_text_size = 0;
  size_t total_const_size = 0;
  size_t total_data_size = 0;

  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *e = &args->entries[i];
    switch (e->ty) {
    case OBJECT_ENTRY_TY_FUNC:
      entry_offsets[i] = total_text_size;
      total_text_size += ROUND_UP(e->len_data, text_align);
      break;
    case OBJECT_ENTRY_TY_C_STRING:
    case OBJECT_ENTRY_TY_CONST_DATA:
      entry_offsets[i] = total_const_size;
      total_const_size += ROUND_UP(e->len_data, const_align);
      break;
    case OBJECT_ENTRY_TY_MUT_DATA:
      entry_offsets[i] = total_data_size;
      total_data_size += ROUND_UP(e->len_data, data_align);
      break;
    case OBJECT_ENTRY_TY_DECL:
      entry_offsets[i] = 0;
      break;
    }
  }

  total_text_size = ROUND_UP(total_text_size, const_align);
  total_const_size = ROUND_UP(total_text_size + total_const_size, data_align) -
                     total_text_size;

  struct reloc_info info = build_reloc_info(args, entry_offsets);

  /* assign file offsets to sections in order:
     elf header | .text | .cstring | .const | .data |
     .rela.text | .rela.const | .rela.data | .symtab | .strtab | .shstrtab |
     shdrs
  */
#define MK_OFFSETS(bit)                                                        \
  offset = sizeof(Elf##bit##_Ehdr);                                            \
  text_off = offset;                                                           \
  offset += total_text_size;                                                   \
  const_off = offset;                                                          \
  offset += total_const_size;                                                  \
  data_off = offset;                                                           \
  offset += total_data_size;                                                   \
  rela_text_off = offset;                                                      \
  offset += info.num_text_reloc_instrs * sizeof(Elf##bit##_Rela);              \
  rela_const_off = offset;                                                     \
  offset += info.num_const_data_reloc_instrs * sizeof(Elf##bit##_Rela);        \
  rela_data_off = offset;                                                      \
  offset += info.num_data_reloc_instrs * sizeof(Elf##bit##_Rela);              \
  symtab_off = offset;                                                         \
  offset += (args->num_entries + 1) * sizeof(Elf##bit##_Sym);                  \
  strtab_off = offset;                                                         \
  total_sym_str = 1; /* initial null */                                        \
  for (size_t i = 0; i < args->num_entries; i++) {                             \
    total_sym_str += strlen(args->entries[i].symbol.name) + 1;                 \
  }                                                                            \
  offset += total_sym_str;                                                     \
  shstr_off = offset;                                                          \
                                                                               \
  shstr_size = sizeof(shstrtab);                                               \
  offset += shstr_size;                                                        \
  shoff = offset;

  const char shstrtab[] = "\0.text\0.rodata\0.data\0.rela.text\0.rela."
                          "rodata\0.rela.data\0.symtab\0.strtab\0.shstrtab\0";
  size_t offset;
  size_t text_off;
  size_t const_off;
  size_t data_off;
  size_t rela_text_off;
  size_t total_sym_str;
  size_t rela_const_off;
  size_t rela_data_off;
  size_t symtab_off;
  size_t strtab_off;
  size_t shstr_size;
  size_t shstr_off;
  size_t shoff;

  switch (args->target) {
  case COMPILE_TARGET_LINUX_RV32I: {
    MK_OFFSETS(32);
    write_header_32(file, EM_RISCV, EF_RISCV_FLOAT_ABI_DOUBLE, shoff);
    break;
  }
  case COMPILE_TARGET_LINUX_ARM64: {
    MK_OFFSETS(64);
    write_header_64(file, EM_AARCH64, 0, shoff);
    break;
  }
  case COMPILE_TARGET_LINUX_X86_64: {
    MK_OFFSETS(64);
    write_header_64(file, EM_X86_64, 0, shoff);
    break;
  }
  default:
    unsupported("unsupported arch for elf");
  }

  /* write section contents */

  size_t *sym_id_to_idx =
      nonnull_malloc(sizeof(*sym_id_to_idx) * args->num_entries);

  safe_fseek_set(file, text_off);
  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *e = &args->entries[i];
    if (e->ty == OBJECT_ENTRY_TY_FUNC) {
      fwrite(e->data, 1, e->len_data, file);
      for (size_t j = e->len_data; j < ROUND_UP(e->len_data, text_align); j++) {
        fputc(0, file);
      }
    }
  }

  /* .rodata */
  safe_fseek_set(file, const_off);
  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *e = &args->entries[i];
    if (e->ty == OBJECT_ENTRY_TY_CONST_DATA ||
        e->ty == OBJECT_ENTRY_TY_C_STRING) {
      fwrite(e->data, 1, e->len_data, file);
      for (size_t j = e->len_data; j < ROUND_UP(e->len_data, const_align);
           j++) {
        fputc(0, file);
      }
    }
  }

  /* .data */
  safe_fseek_set(file, data_off);
  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *e = &args->entries[i];
    if (e->ty == OBJECT_ENTRY_TY_MUT_DATA && e->data) {
      fwrite(e->data, 1, e->len_data, file);
      for (size_t j = e->len_data; j < ROUND_UP(e->len_data, data_align); j++) {
        fputc(0, file);
      }
    }
  }

  struct vector *symbols;

  // first null entry
  switch (args->target) {
  case COMPILE_TARGET_LINUX_RV32I: {
    symbols = vector_create(sizeof(Elf32_Sym));
    Elf32_Sym sym;
    memset(&sym, 0, sizeof(sym));
    vector_push_back(symbols, &sym);
    break;
  }
  case COMPILE_TARGET_LINUX_ARM64:
  case COMPILE_TARGET_LINUX_X86_64: {
    symbols = vector_create(sizeof(Elf64_Sym));
    Elf64_Sym sym;
    memset(&sym, 0, sizeof(sym));
    vector_push_back(symbols, &sym);
    break;
  }
  default:
    unsupported("unsupported arch for elf");
  }

  size_t *sym_order = nonnull_malloc(sizeof(*sym_order) * args->num_entries);

  size_t str_off = 1;

  enum symbol_visibility sym_vises[3] = {SYMBOL_VISIBILITY_PRIVATE,
                                         SYMBOL_VISIBILITY_GLOBAL,
                                         SYMBOL_VISIBILITY_UNDEF};

  size_t num_local = 0;

  size_t last_sym_idx = 1;
  size_t last_sym_pos = 0;
  for (size_t j = 0; j < ARR_LENGTH(sym_vises); j++) {
    enum symbol_visibility vis = sym_vises[j];

    for (size_t i = 0; i < args->num_entries; i++) {
      const struct symbol *s = &args->entries[i].symbol;

      if (s->visibility != vis) {
        continue;
      }

      sym_id_to_idx[i] = last_sym_idx++;
      sym_order[last_sym_pos++] = i;

      unsigned short st_shndx = 0;
      unsigned long st_name = 0;
      unsigned long st_value = 0;
      unsigned long st_size = 0;

      st_name = str_off;
      if (s->visibility != SYMBOL_VISIBILITY_UNDEF) {
        switch (s->ty) {
        case SYMBOL_TY_FUNC:
          st_shndx = 1;
          st_value = entry_offsets[i];
          st_size = args->entries[i].len_data;
          break;
        case SYMBOL_TY_STRING:
        case SYMBOL_TY_CONST_DATA:
          st_shndx = 2;
          st_value = entry_offsets[i];
          st_size = args->entries[i].len_data;
          break;
        case SYMBOL_TY_DATA:
          st_shndx = 3;
          st_value = entry_offsets[i];
          st_size = args->entries[i].len_data;
          break;
        case SYMBOL_TY_DECL:
          BUG("decl symbol must be undefined");
        }
      } else {
        st_shndx = SHN_UNDEF;
      }

      unsigned char stb;
      unsigned char stt;

      switch (s->visibility) {
      case SYMBOL_VISIBILITY_PRIVATE:
        stb = STB_LOCAL;
        num_local++;
        break;
      case SYMBOL_VISIBILITY_GLOBAL:
      case SYMBOL_VISIBILITY_UNDEF:
        stb = STB_GLOBAL;
        break;
      }

      switch (s->ty) {
      case SYMBOL_TY_DECL:
      case SYMBOL_TY_FUNC:
        stt = STT_FUNC;
        break;
      case SYMBOL_TY_STRING:
      case SYMBOL_TY_CONST_DATA:
      case SYMBOL_TY_DATA:
        stt = STT_OBJECT;
        break;
      }

      switch (args->target) {
      case COMPILE_TARGET_LINUX_RV32I: {
        Elf32_Sym sym = {.st_name = st_name,
                         .st_shndx = st_shndx,
                         .st_value = st_value,
                         .st_size = st_size,
                         .st_info = ELF32_ST_INFO(stb, stt)};
        vector_push_back(symbols, &sym);
        break;
      }
      case COMPILE_TARGET_LINUX_ARM64:
      case COMPILE_TARGET_LINUX_X86_64: {
        Elf64_Sym sym = {.st_name = st_name,
                         .st_shndx = st_shndx,
                         .st_value = st_value,
                         .st_size = st_size,
                         .st_info = ELF64_ST_INFO(stb, stt)};
        vector_push_back(symbols, &sym);
        break;
      }
      default:
        unsupported("unsupported arch for elf");
      }

      str_off += strlen(s->name) + 1;
    }
  }

  /* relocations */
  safe_fseek_set(file, rela_text_off);
  write_relocations_elf(file, args, sym_id_to_idx, info.text_relocs,
                        args->target);
  safe_fseek_set(file, rela_const_off);
  write_relocations_elf(file, args, sym_id_to_idx, info.const_data_relocs,
                        args->target);
  safe_fseek_set(file, rela_data_off);
  write_relocations_elf(file, args, sym_id_to_idx, info.data_relocs,
                        args->target);

  /* symbol table */
  safe_fseek_set(file, symtab_off);
  fwrite(vector_head(symbols), 1, vector_byte_size(symbols), file);

  /* string table */
  safe_fseek_set(file, strtab_off);
  fputc(0, file);
  for (size_t i = 0; i < args->num_entries; i++) {
    size_t idx = sym_order[i];
    fwrite(args->entries[idx].symbol.name, 1,
           strlen(args->entries[idx].symbol.name) + 1, file);
  }

  /* section header string table */
  safe_fseek_set(file, shstr_off);
  fwrite(shstrtab, 1, shstr_size, file);

#define MK_SHDR(sz)                                                            \
  /* entry 0 is null section */                                                \
  shdr[1].sh_name = 1;                                                         \
  shdr[1].sh_type = SHT_PROGBITS;                                              \
  shdr[1].sh_flags = SHF_ALLOC | SHF_EXECINSTR;                                \
  shdr[1].sh_offset = text_off;                                                \
  shdr[1].sh_size = total_text_size;                                           \
  shdr[1].sh_addralign = text_align;                                           \
                                                                               \
  shdr[2].sh_name = 7;                                                         \
  shdr[2].sh_type = SHT_PROGBITS;                                              \
  shdr[2].sh_flags = SHF_ALLOC;                                                \
  shdr[2].sh_offset = const_off;                                               \
  shdr[2].sh_size = total_const_size;                                          \
  shdr[2].sh_addralign = const_align;                                          \
                                                                               \
  shdr[3].sh_name = 15;                                                        \
  shdr[3].sh_type = SHT_PROGBITS;                                              \
  shdr[3].sh_flags = SHF_ALLOC;                                                \
  shdr[3].sh_offset = data_off;                                                \
  shdr[3].sh_size = total_data_size;                                           \
  shdr[3].sh_addralign = data_align;                                           \
                                                                               \
  shdr[4].sh_name = 21;                                                        \
  shdr[4].sh_type = SHT_RELA;                                                  \
  shdr[4].sh_offset = rela_text_off;                                           \
  shdr[4].sh_size = info.num_text_reloc_instrs * sizeof(Elf##sz##_Rela);       \
  shdr[4].sh_link = 7;                                                         \
  shdr[4].sh_info = 1;                                                         \
  shdr[4].sh_addralign = 8;                                                    \
  shdr[4].sh_entsize = sizeof(Elf##sz##_Rela);                                 \
  shdr[4].sh_flags = SHF_INFO_LINK;                                            \
                                                                               \
  shdr[5].sh_name = 32;                                                        \
  shdr[5].sh_type = SHT_RELA;                                                  \
  shdr[5].sh_offset = rela_const_off;                                          \
  shdr[5].sh_size = info.num_const_data_reloc_instrs * sizeof(Elf##sz##_Rela); \
  shdr[5].sh_link = 7;                                                         \
  shdr[5].sh_info = 2;                                                         \
  shdr[5].sh_addralign = 8;                                                    \
  shdr[5].sh_entsize = sizeof(Elf##sz##_Rela);                                 \
                                                                               \
  shdr[6].sh_name = 45;                                                        \
  shdr[6].sh_type = SHT_RELA;                                                  \
  shdr[6].sh_offset = rela_data_off;                                           \
  shdr[6].sh_size = info.num_data_reloc_instrs * sizeof(Elf##sz##_Rela);       \
  shdr[6].sh_link = 7;                                                         \
  shdr[6].sh_info = 3;                                                         \
  shdr[6].sh_addralign = 8;                                                    \
  shdr[6].sh_entsize = sizeof(Elf##sz##_Rela);                                 \
                                                                               \
  shdr[7].sh_name = 56;                                                        \
  shdr[7].sh_type = SHT_SYMTAB;                                                \
  shdr[7].sh_offset = symtab_off;                                              \
  shdr[7].sh_size = (args->num_entries + 1) * sizeof(Elf##sz##_Sym);           \
  shdr[7].sh_link = 8;                                                         \
  shdr[7].sh_addralign = 8;                                                    \
  shdr[7].sh_entsize = sizeof(Elf##sz##_Sym);                                  \
  shdr[7].sh_info = num_local + 1;                                             \
                                                                               \
  /* entry 9: .strtab */                                                       \
  shdr[8].sh_name = 64;                                                        \
  shdr[8].sh_type = SHT_STRTAB;                                                \
  shdr[8].sh_offset = strtab_off;                                              \
  shdr[8].sh_size = total_sym_str;                                             \
  shdr[8].sh_addralign = 1;                                                    \
                                                                               \
  /* entry 10: .shstrtab */                                                    \
  shdr[9].sh_name = 72;                                                        \
  shdr[9].sh_type = SHT_STRTAB;                                                \
  shdr[9].sh_offset = shstr_off;                                               \
  shdr[9].sh_size = shstr_size;                                                \
  shdr[9].sh_addralign = 1;                                                    \
                                                                               \
  safe_fseek_set(file, shoff);                                                 \
  fwrite(shdr, sizeof(shdr), 1, file);

  switch (args->target) {
  case COMPILE_TARGET_LINUX_RV32I: {
    Elf32_Shdr shdr[10];
    memset(shdr, 0, sizeof(shdr));
    MK_SHDR(32);
    break;
  }
  case COMPILE_TARGET_LINUX_ARM64:
  case COMPILE_TARGET_LINUX_X86_64: {
    Elf64_Shdr shdr[10];
    memset(shdr, 0, sizeof(shdr));
    MK_SHDR(64);
    break;
  }
  default:
    unreachable();
  }

  vector_free(&symbols);
  free(entry_offsets);
  free(sym_id_to_idx);
  free(sym_order);

  vector_free(&info.text_relocs);
  vector_free(&info.const_data_relocs);
  vector_free(&info.data_relocs);
}

void write_elf(const struct build_object_args *args) { write_elf_object(args); }
