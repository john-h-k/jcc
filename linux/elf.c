#include "elf.h"

#include "../util.h"

#if __has_include(<elf.h>)
#include "../compiler.h"
#include "../util.h"
#include "../vector.h"

#include <elf.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct reloc_info {
  size_t num_text_reloc_instrs;
  size_t num_const_data_reloc_instrs;
  size_t num_data_reloc_instrs;

  struct vector *text_relocs;
  struct vector *const_data_relocs;
  struct vector *data_relocs;
};

static struct reloc_info build_reloc_info(const struct build_object_args *args,
                                          const size_t *entry_offsets) {
  struct reloc_info info = {
      .text_relocs = vector_create(sizeof(struct relocation)),
      .data_relocs = vector_create(sizeof(struct relocation)),
      .const_data_relocs = vector_create(sizeof(struct relocation)),
  };

  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *entry = &args->entries[i];
    size_t entry_offset = entry_offsets[i];

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

    for (size_t j = 0; j < entry->num_relocations; j++) {
      struct relocation reloc = entry->relocations[j];

      reloc.address += entry_offset;

      vector_push_back(relocs, &reloc);

      switch (reloc.ty) {
      case RELOCATION_TY_POINTER:
      case RELOCATION_TY_SINGLE:
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

static void write_relocations_elf(FILE *file, struct vector *relocs,
                                  int target_arch) {
  size_t n = vector_length(relocs);
  for (size_t i = 0; i < n; i++) {
    const struct relocation *r = vector_get(relocs, i);
    switch (r->ty) {
    case RELOCATION_TY_POINTER: {
      Elf64_Rela rela;
      memset(&rela, 0, sizeof(rela));
      rela.r_offset = r->address;
      rela.r_addend = 0;
      uint32_t type = (target_arch == COMPILE_TARGET_ARCH_LINUX_X86_64)
                          ? R_X86_64_64
                          : R_AARCH64_ABS64;
      rela.r_info = ELF64_R_INFO(r->symbol_index, type);
      fwrite(&rela, sizeof(rela), 1, file);
      break;
    }
    case RELOCATION_TY_SINGLE: {
      Elf64_Rela rela;
      memset(&rela, 0, sizeof(rela));
      rela.r_offset = r->address;
      rela.r_addend = 0;
      uint32_t type = (target_arch == COMPILE_TARGET_ARCH_LINUX_X86_64)
                          ? R_X86_64_PC32
                          : R_AARCH64_CALL26;
      rela.r_info = ELF64_R_INFO(r->symbol_index + 1, type);
      fwrite(&rela, sizeof(rela), 1, file);
      break;
    }
    case RELOCATION_TY_LOCAL_PAIR: {
      if (target_arch == COMPILE_TARGET_ARCH_LINUX_ARM64) {
        Elf64_Rela rela1, rela2;
        memset(&rela1, 0, sizeof(rela1));
        memset(&rela2, 0, sizeof(rela2));
        rela1.r_offset = r->address;
        rela1.r_addend = 0;
        rela1.r_info =
            ELF64_R_INFO(r->symbol_index, R_AARCH64_ADR_PREL_PG_HI21);
        rela2.r_offset = r->address + 4;
        rela2.r_addend = 0;
        rela2.r_info = ELF64_R_INFO(r->symbol_index, R_AARCH64_ADD_ABS_LO12_NC);
        fwrite(&rela1, sizeof(rela1), 1, file);
        fwrite(&rela2, sizeof(rela2), 1, file);
      } else {
        BUG("two-part reloc not supported for x86_64 in elf");
      }
      break;
    }
    case RELOCATION_TY_UNDEF_PAIR: {
      if (target_arch == COMPILE_TARGET_ARCH_LINUX_ARM64) {
        Elf64_Rela rela1, rela2;
        memset(&rela1, 0, sizeof(rela1));
        memset(&rela2, 0, sizeof(rela2));
        rela1.r_offset = r->address;
        rela1.r_addend = 0;
        rela1.r_info =
            ELF64_R_INFO(r->symbol_index, R_AARCH64_LD64_GOT_LO12_NC);
        rela2.r_offset = r->address + 4;
        rela2.r_addend = 0;
        rela2.r_info =
            ELF64_R_INFO(r->symbol_index, R_AARCH64_LD64_GOT_LO12_NC);
        fwrite(&rela1, sizeof(rela1), 1, file);
        fwrite(&rela2, sizeof(rela2), 1, file);
      } else {
        BUG("two-part reloc not supported for x86_64 in elf");
      }
      break;
    }
    default:
      BUG("unsupported relocation type for elf");
    }
  }
}

static void write_elf_object(const struct build_object_args *args) {
  FILE *file = fopen(args->output, "wb");
  if (!file) {
    perror("fopen");
    exit(EXIT_FAILURE);
  }

  size_t text_align = 1, cstr_align = 1, const_align = 1, data_align = 1;
  size_t total_text = 0, total_cstr = 0, total_const = 0, total_data = 0;
  size_t *entry_offsets =
      nonnull_malloc(args->num_entries * sizeof(*entry_offsets));
  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *e = &args->entries[i];
    switch (e->ty) {
    case OBJECT_ENTRY_TY_FUNC:
      text_align = MAX(text_align, e->alignment);
      entry_offsets[i] = total_text;
      total_text += ROUND_UP(e->len_data, text_align);
      break;
    case OBJECT_ENTRY_TY_C_STRING:
      cstr_align = MAX(cstr_align, e->alignment);
      entry_offsets[i] = total_cstr;
      total_cstr += ROUND_UP(e->len_data, cstr_align);
      break;
    case OBJECT_ENTRY_TY_CONST_DATA:
      const_align = MAX(const_align, e->alignment);
      entry_offsets[i] = total_const;
      total_const += ROUND_UP(e->len_data, const_align);
      break;
    case OBJECT_ENTRY_TY_MUT_DATA:
      data_align = MAX(data_align, e->alignment);
      entry_offsets[i] = total_data;
      total_data += ROUND_UP(e->len_data, data_align);
      break;
    case OBJECT_ENTRY_TY_DECL:
      break;
    }
  }

  /* build relocation info */
  struct reloc_info rinfo = build_reloc_info(args, entry_offsets);

  /* assign file offsets to sections in order:
     elf header | .text | .cstring | .const | .data |
     .rela.text | .rela.const | .rela.data | .symtab | .strtab | .shstrtab |
     shdrs
  */
  size_t offset = sizeof(Elf64_Ehdr);
  size_t text_off = offset;
  offset += total_text;
  size_t cstr_off = offset;
  offset += total_cstr;
  size_t const_off = offset;
  offset += total_const;
  size_t data_off = offset;
  offset += total_data;
  size_t rela_text_off = offset;
  offset += rinfo.num_text_reloc_instrs * sizeof(Elf64_Rela);
  size_t rela_const_off = offset;
  offset += rinfo.num_const_data_reloc_instrs * sizeof(Elf64_Rela);
  size_t rela_data_off = offset;
  offset += rinfo.num_data_reloc_instrs * sizeof(Elf64_Rela);
  size_t symtab_off = offset;
  offset += (args->num_entries + 1) * sizeof(Elf64_Sym);
  size_t strtab_off = offset;
  size_t total_sym_str = 1; /* initial null */
  for (size_t i = 0; i < args->num_entries; i++) {
    total_sym_str += strlen(args->entries[i].symbol.name) + 1;
  }
  offset += total_sym_str;
  size_t shstr_off = offset;
  /* fixed shstrtab contents with known name offsets */
  const char shstrtab[] = "\0.text\0.data\0.rodata\0.rela.text\0.rela."
                          "data\0.rela.rodata\0.symtab\0.strtab\0.shstrtab\0";
  size_t shstr_size = sizeof(shstrtab);
  offset += shstr_size;
  size_t shoff = offset;

  /* prepare and write elf header */
  Elf64_Ehdr ehdr;
  memset(&ehdr, 0, sizeof(ehdr));
  ehdr.e_ident[EI_MAG0] = 0x7f;
  ehdr.e_ident[EI_MAG1] = 'E';
  ehdr.e_ident[EI_MAG2] = 'L';
  ehdr.e_ident[EI_MAG3] = 'F';
  ehdr.e_ident[EI_CLASS] = ELFCLASS64;
  ehdr.e_ident[EI_DATA] = ELFDATA2LSB;
  ehdr.e_ident[EI_VERSION] = EV_CURRENT;
  ehdr.e_ident[EI_OSABI] = ELFOSABI_NONE;
  ehdr.e_type = ET_REL;
  switch (args->compile_args->target_arch) {
  case COMPILE_TARGET_ARCH_LINUX_ARM64:
    ehdr.e_machine = EM_AARCH64;
    break;
  case COMPILE_TARGET_ARCH_LINUX_X86_64:
    ehdr.e_machine = EM_X86_64;
    break;
  default:
    unsupported("unsupported arch for elf");
  }
  ehdr.e_version = EV_CURRENT;
  ehdr.e_ehsize = sizeof(Elf64_Ehdr);
  ehdr.e_shoff = shoff;
  ehdr.e_shentsize = sizeof(Elf64_Shdr);
  ehdr.e_shnum = 11;    /* null + 10 sections */
  ehdr.e_shstrndx = 10; /* .shstrtab is section 10 */
  fseek(file, 0, SEEK_SET);
  fwrite(&ehdr, sizeof(ehdr), 1, file);

  /* write section contents */

  /* .text */
  fseek(file, text_off, SEEK_SET);
  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *e = &args->entries[i];
    if (e->ty == OBJECT_ENTRY_TY_FUNC) {
      fwrite(e->data, 1, e->len_data, file);
      for (size_t j = e->len_data; j < ROUND_UP(e->len_data, text_align); j++)
        fputc(0, file);
    }
  }

  /* .cstring */
  fseek(file, cstr_off, SEEK_SET);
  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *e = &args->entries[i];
    if (e->ty == OBJECT_ENTRY_TY_C_STRING) {
      fwrite(e->data, 1, e->len_data, file);
      for (size_t j = e->len_data; j < ROUND_UP(e->len_data, cstr_align); j++)
        fputc(0, file);
    }
  }

  /* .const */
  fseek(file, const_off, SEEK_SET);
  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *e = &args->entries[i];
    if (e->ty == OBJECT_ENTRY_TY_CONST_DATA) {
      fwrite(e->data, 1, e->len_data, file);
      for (size_t j = e->len_data; j < ROUND_UP(e->len_data, const_align); j++)
        fputc(0, file);
    }
  }

  /* .data */
  fseek(file, data_off, SEEK_SET);
  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *e = &args->entries[i];
    if (e->ty == OBJECT_ENTRY_TY_MUT_DATA && e->data) {
      fwrite(e->data, 1, e->len_data, file);
      for (size_t j = e->len_data; j < ROUND_UP(e->len_data, data_align); j++)
        fputc(0, file);
    }
  }

  /* relocation sections */
  fseek(file, rela_text_off, SEEK_SET);
  write_relocations_elf(file, rinfo.text_relocs,
                        args->compile_args->target_arch);
  fseek(file, rela_const_off, SEEK_SET);
  write_relocations_elf(file, rinfo.const_data_relocs,
                        args->compile_args->target_arch);
  fseek(file, rela_data_off, SEEK_SET);
  write_relocations_elf(file, rinfo.data_relocs,
                        args->compile_args->target_arch);

  /* symbol table */
  fseek(file, symtab_off, SEEK_SET);
  {
    Elf64_Sym sym;
    memset(&sym, 0, sizeof(sym));
    fwrite(&sym, sizeof(sym), 1, file); // first null entry

    /* fix: use 0-based offsets within sections, not file offsets */
    size_t text_sym = 0, cstr_sym = 0, const_sym = 0, data_sym = 0;
    size_t str_off = 1;
    for (size_t i = 0; i < args->num_entries; i++) {
      const struct symbol *s = &args->entries[i].symbol;
      memset(&sym, 0, sizeof(sym));
      sym.st_name = str_off;
      if (s->visibility != SYMBOL_VISIBILITY_UNDEF) {
        switch (s->ty) {
        case SYMBOL_TY_FUNC:
          sym.st_shndx = 1; /* .text */
          sym.st_value = text_sym;
          text_sym += args->entries[i].len_data;
          break;
        case SYMBOL_TY_STRING:
          sym.st_shndx = 2; /* .cstring */
          sym.st_value = cstr_sym;
          cstr_sym += args->entries[i].len_data;
          break;
        case SYMBOL_TY_CONST_DATA:
          sym.st_shndx = 3; /* .const */
          sym.st_value = const_sym;
          const_sym += args->entries[i].len_data;
          break;
        case SYMBOL_TY_DATA:
          sym.st_shndx = 4; /* .data */
          sym.st_value = data_sym;
          data_sym += args->entries[i].len_data;
          break;
        case SYMBOL_TY_DECL:
          BUG("decl symbol must be undefined");
        }
      } else {
        sym.st_shndx = SHN_UNDEF;
      }
      sym.st_info = (s->visibility == SYMBOL_VISIBILITY_GLOBAL)
                        ? ELF64_ST_INFO(STB_GLOBAL, STT_FUNC)
                        : ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE);
      fwrite(&sym, sizeof(sym), 1, file);
      str_off += strlen(s->name) + 1;
    }
  }

  /* string table */
  fseek(file, strtab_off, SEEK_SET);
  fputc(0, file);
  for (size_t i = 0; i < args->num_entries; i++) {
    fwrite(args->entries[i].symbol.name, 1,
           strlen(args->entries[i].symbol.name) + 1, file);
  }

  /* section header string table */
  fseek(file, shstr_off, SEEK_SET);
  fwrite(shstrtab, 1, shstr_size, file);

  /* build section header table (11 entries: null + 10 sections) */
  {
    Elf64_Shdr shdr[11];
    memset(shdr, 0, sizeof(shdr));

    /* entry 0: null section */

    /* entry 1: .text */
    shdr[1].sh_name = 1; /* offset in shstrtab for ".text" */
    shdr[1].sh_type = SHT_PROGBITS;
    shdr[1].sh_flags = SHF_ALLOC | SHF_EXECINSTR;
    shdr[1].sh_offset = text_off;
    shdr[1].sh_size = total_text;
    shdr[1].sh_addralign = text_align;

    /* entry 2: .data */
    shdr[2].sh_name = 7; /* ".data" */
    shdr[2].sh_type = SHT_PROGBITS;
    shdr[2].sh_flags = SHF_ALLOC;
    shdr[2].sh_offset = cstr_off;
    shdr[2].sh_size = total_cstr;
    shdr[2].sh_addralign = cstr_align;

    /* entry 3: .const */
    shdr[3].sh_name = 16; /* ".const" */
    shdr[3].sh_type = SHT_PROGBITS;
    shdr[3].sh_flags = SHF_ALLOC;
    shdr[3].sh_offset = const_off;
    shdr[3].sh_size = total_const;
    shdr[3].sh_addralign = const_align;

    /* entry 4: .data */
    shdr[4].sh_name = 23; /* ".data" */
    shdr[4].sh_type = SHT_PROGBITS;
    shdr[4].sh_flags = SHF_ALLOC | SHF_WRITE;
    shdr[4].sh_offset = data_off;
    shdr[4].sh_size = total_data;
    shdr[4].sh_addralign = data_align;

    /* entry 5: .rela.text */
    shdr[5].sh_name = 29; /* ".rela.text" */
    shdr[5].sh_type = SHT_RELA;
    shdr[5].sh_offset = rela_text_off;
    shdr[5].sh_size = rinfo.num_text_reloc_instrs * sizeof(Elf64_Rela);
    shdr[5].sh_link = 8; /* symtab index */
    shdr[5].sh_info = 1; /* associated with .text */
    shdr[5].sh_addralign = 8;
    shdr[5].sh_entsize = sizeof(Elf64_Rela);

    /* entry 6: .rela.const */
    shdr[6].sh_name = 40; /* ".rela.const" */
    shdr[6].sh_type = SHT_RELA;
    shdr[6].sh_offset = rela_const_off;
    shdr[6].sh_size = rinfo.num_const_data_reloc_instrs * sizeof(Elf64_Rela);
    shdr[6].sh_link = 8;
    shdr[6].sh_info = 3; /* .const */
    shdr[6].sh_addralign = 8;
    shdr[6].sh_entsize = sizeof(Elf64_Rela);

    /* entry 7: .rela.data */
    shdr[7].sh_name = 53; /* ".rela.data" */
    shdr[7].sh_type = SHT_RELA;
    shdr[7].sh_offset = rela_data_off;
    shdr[7].sh_size = rinfo.num_data_reloc_instrs * sizeof(Elf64_Rela);
    shdr[7].sh_link = 8;
    shdr[7].sh_info = 4; /* .data */
    shdr[7].sh_addralign = 8;
    shdr[7].sh_entsize = sizeof(Elf64_Rela);

    /* entry 8: .symtab */
    shdr[8].sh_name = 65; /* ".symtab" */
    shdr[8].sh_type = SHT_SYMTAB;
    shdr[8].sh_offset = symtab_off;
    shdr[8].sh_size = (args->num_entries + 1) * sizeof(Elf64_Sym);
    shdr[8].sh_link = 9; /* .strtab index */
    shdr[8].sh_addralign = 8;
    shdr[8].sh_entsize = sizeof(Elf64_Sym);

    size_t local_count = 1; // count the initial null symbol
    for (size_t i = 0; i < args->num_entries; i++) {
      const struct symbol *s = &args->entries[i].symbol;
      if (s->visibility != SYMBOL_VISIBILITY_GLOBAL)
        local_count++;
    }
    shdr[8].sh_info = local_count;

    /* entry 9: .strtab */
    shdr[9].sh_name = 73; /* ".strtab" */
    shdr[9].sh_type = SHT_STRTAB;
    shdr[9].sh_offset = strtab_off;
    shdr[9].sh_size = total_sym_str;
    shdr[9].sh_addralign = 1;

    /* entry 10: .shstrtab */
    shdr[10].sh_name = 81; /* ".shstrtab" */
    shdr[10].sh_type = SHT_STRTAB;
    shdr[10].sh_offset = shstr_off;
    shdr[10].sh_size = shstr_size;
    shdr[10].sh_addralign = 1;

    fseek(file, shoff, SEEK_SET);
    fwrite(shdr, sizeof(shdr), 1, file);
  }

  fclose(file);
}

void write_elf(const struct build_object_args *args) { write_elf_object(args); }

#else

void write_elf(const struct build_object_args *args) {
  unsupported("ELF not supported target for this system");
}

#endif
