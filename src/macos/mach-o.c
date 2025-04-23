#include "mach-o.h"

#include "../compiler.h"
#include "../util.h"
#include "../vector.h"
#include "mach-o_types.h"

#include <stdint.h>
#include <stdio.h>

struct reloc_info {
  size_t num_text_reloc_instrs;
  size_t num_const_data_reloc_instrs;
  size_t num_data_reloc_instrs;

  struct vector *text_relocs;
  struct vector *const_data_relocs;
  struct vector *data_relocs;
};

static void write_mach_header(FILE *file, const struct compile_args *args) {
  struct mach_header_64 header;
  memset(&header, 0, sizeof(header));

  header.magic = MH_MAGIC_64;

  switch (args->target) {
  case COMPILE_TARGET_MACOS_ARM64:
    header.cputype = CPU_TYPE_ARM64;
    header.cpusubtype = CPU_SUBTYPE_ARM64_ALL;
    break;
  case COMPILE_TARGET_MACOS_X86_64:
    header.cputype = CPU_TYPE_X86_64;
    header.cpusubtype = CPU_SUBTYPE_X86_64_ALL;
    break;
  default:
    unsupported("unsupported arch for mach-o");
  }

  header.filetype = MH_OBJECT;
  header.ncmds = 4;
  header.sizeofcmds =
      sizeof(struct segment_command_64) + sizeof(struct section_64) * 4 +
      sizeof(struct build_version_command) + sizeof(struct symtab_command) +
      sizeof(struct dysymtab_command);
  header.flags = 0;

  fwrite(&header, sizeof(header), 1, file);
}

#define ENCODE(x, y, z) ((x & 0xFF) << 16) | ((y & 0x0F) << 8) | (z & 0x0F)
#define ENCODE_MINOS(x, y, z)                                                  \
  ((x & 0xFF) << 16) | ((y & 0x0F) << 8) | (z & 0x0F)
#define ENCODE_SDK(x, y, z) ((x & 0xFF) << 16) | ((y & 0x0F) << 8) | (z & 0x0F)

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

// macOS makes them bitfields but this is not technically proper C (as you
// can't assume bitfields are packed)
// FIXME: do this for ELF too
#define RELOC_INFO_SIZE 8
static void write_single_relocation(FILE *file, struct relocation_info info) {
  fwrite(&info.r_address, sizeof(info.r_address), 1, file);

  // r_symbolnum : 24
  // r_pcrel     : 1
  // r_length    : 2
  // r_extern    : 1
  // r_type      : 4
  uint32_t other = info.r_symbolnum;
  other |= (info.r_pcrel << 24);
  other |= (info.r_length << 25);
  other |= (info.r_extern << 27);
  other |= (info.r_type << 28);
  fwrite(&other, sizeof(other), 1, file);
}

static void write_relocation(FILE *file, struct vector *relocations) {
  size_t num_relocations = vector_length(relocations);

  for (size_t i = 0; i < num_relocations; i++) {
    const struct relocation *reloc = vector_get(relocations, i);

    size_t index = reloc->symbol_index;
    size_t addr = reloc->address;

    switch (reloc->ty) {
    case RELOCATION_TY_POINTER: {
      struct relocation_info info = {.r_address = addr,
                                     .r_length = reloc->size,
                                     .r_pcrel = 0,
                                     .r_symbolnum = index,
                                     .r_extern = 1,
                                     .r_type = ARM64_RELOC_UNSIGNED};

      write_single_relocation(file, info);
      break;
    }
    case RELOCATION_TY_CALL: {
      struct relocation_info info = {.r_address = addr,
                                     .r_length = reloc->size,
                                     .r_pcrel = 1,
                                     .r_symbolnum = index,
                                     .r_extern = 1,
                                     .r_type = ARM64_RELOC_BRANCH26};

      write_single_relocation(file, info);
      break;
    }
    case RELOCATION_TY_LOCAL_SINGLE: {
      struct relocation_info info = {.r_address = addr,
                                     .r_length = reloc->size,
                                     .r_pcrel = 1,
                                     .r_symbolnum = index,
                                     .r_extern = 1,
                                     .r_type = X86_64_RELOC_SIGNED};

      write_single_relocation(file, info);
      break;
    }
    case RELOCATION_TY_LOCAL_PAIR: {
      struct relocation_info infos[2] = {
          {.r_address = addr,
           .r_length = reloc->size,
           .r_pcrel = 1,
           .r_symbolnum = index,
           .r_extern = 1,
           .r_type = ARM64_RELOC_PAGE21},
          {.r_address = addr + 4,
           .r_length = reloc->size,
           .r_pcrel = 0,
           .r_symbolnum = index,
           .r_extern = 1,
           .r_type = ARM64_RELOC_PAGEOFF12},
      };

      write_single_relocation(file, infos[0]);
      write_single_relocation(file, infos[1]);
      break;
    }
    case RELOCATION_TY_UNDEF_SINGLE: {
      struct relocation_info info = {.r_address = addr,
                                     .r_length = reloc->size,
                                     .r_pcrel = 1,
                                     .r_symbolnum = index,
                                     .r_extern = 1,
                                     .r_type = X86_64_RELOC_GOT};

      write_single_relocation(file, info);
      break;
    }
    case RELOCATION_TY_UNDEF_PAIR: {
      struct relocation_info infos[2] = {
          {.r_address = addr,
           .r_length = reloc->size,
           .r_pcrel = 1,
           .r_symbolnum = index,
           .r_extern = 1,
           .r_type = ARM64_RELOC_GOT_LOAD_PAGE21},
          {.r_address = addr + 4,
           .r_length = reloc->size,
           .r_pcrel = 0,
           .r_symbolnum = index,
           .r_extern = 1,
           .r_type = ARM64_RELOC_GOT_LOAD_PAGEOFF12},
      };

      write_single_relocation(file, infos[0]);
      write_single_relocation(file, infos[1]);
      break;
    }
    }
  }
}

static void write_relocations(FILE *file, const struct reloc_info *info) {

  write_relocation(file, info->text_relocs);
  write_relocation(file, info->const_data_relocs);
  write_relocation(file, info->data_relocs);
}

static void write_segment_command(const struct build_object_args *args) {
  FILE *file = args->output;

  size_t str_align = 1;
  size_t func_align = 4;
  size_t const_align = 1;
  size_t data_align = 1;

  // TODO: sort by alignment instead of aligning each entry by max align

  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *entry = &args->entries[i];

    switch (entry->ty) {
    case OBJECT_ENTRY_TY_FUNC:
      func_align = MAX(func_align, entry->alignment);
      break;
    case OBJECT_ENTRY_TY_C_STRING:
      str_align = MAX(str_align, entry->alignment);
      break;
    case OBJECT_ENTRY_TY_CONST_DATA:
      const_align = MAX(const_align, entry->alignment);
      break;
    case OBJECT_ENTRY_TY_MUT_DATA:
      data_align = MAX(data_align, entry->alignment);
      break;
    case OBJECT_ENTRY_TY_DECL:
      break;
    }
  }

  size_t total_str_size = 0;
  size_t total_const_size = 0;
  size_t total_data_size = 0;
  size_t total_func_size = 0;

  size_t *entry_offsets =
      nonnull_malloc(args->num_entries * sizeof(*entry_offsets));

  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *entry = &args->entries[i];

    switch (entry->ty) {
    case OBJECT_ENTRY_TY_FUNC:
      entry_offsets[i] = total_func_size;
      total_func_size += ROUND_UP(entry->len_data, func_align);
      break;
    case OBJECT_ENTRY_TY_C_STRING:
      entry_offsets[i] = total_str_size;
      total_str_size += ROUND_UP(entry->len_data, str_align);
      break;
    case OBJECT_ENTRY_TY_CONST_DATA:
      entry_offsets[i] = total_const_size;
      total_const_size += ROUND_UP(entry->len_data, const_align);
      break;
    case OBJECT_ENTRY_TY_MUT_DATA:
      entry_offsets[i] = total_data_size;
      total_data_size += ROUND_UP(entry->len_data, data_align);
      break;
    case OBJECT_ENTRY_TY_DECL:
      break;
    }
  }

  total_func_size = ROUND_UP(total_func_size, str_align);
  total_str_size =
      ROUND_UP(total_func_size + total_str_size, const_align) - total_func_size;
  total_const_size =
      ROUND_UP(total_func_size + total_str_size + total_const_size,
               data_align) -
      (total_func_size + total_str_size);

  size_t total_size =
      total_func_size + total_str_size + total_const_size + total_data_size;

  struct segment_command_64 segment;
  memset(&segment, 0, sizeof(segment));
  segment.cmd = LC_SEGMENT_64;
  segment.cmdsize = sizeof(segment) + sizeof(struct section_64) * 4;
  strcpy(segment.segname, "");
  segment.vmaddr = 0;
  segment.vmsize = total_size;
  segment.fileoff =
      sizeof(struct mach_header_64) + sizeof(struct segment_command_64) +
      sizeof(struct section_64) * 4 + sizeof(struct symtab_command) +
      sizeof(struct dysymtab_command) + sizeof(struct build_version_command);
  segment.filesize = total_size;
  segment.maxprot = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE;
  segment.initprot = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE;
  segment.nsects = 4;
  segment.flags = 0;

  struct reloc_info info = build_reloc_info(args, entry_offsets);
  size_t relocs_offset = segment.fileoff + total_size;
  size_t total_reloc_instrs = info.num_text_reloc_instrs +
                              info.num_data_reloc_instrs +
                              info.num_const_data_reloc_instrs;

  struct section_64 text;
  memset(&text, 0, sizeof(text));
  strcpy(text.sectname, "__text");
  strcpy(text.segname, "__TEXT");
  text.addr = 0;
  text.size = total_func_size;
  text.offset = segment.fileoff;
  text.align = ILOG2(func_align);
  text.reloff = relocs_offset;
  text.nreloc = info.num_text_reloc_instrs;
  text.flags = S_REGULAR | S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS;
  text.reserved1 = 0;
  text.reserved2 = 0;
  text.reserved3 = 0;

  struct section_64 cstrings;
  memset(&cstrings, 0, sizeof(cstrings));
  strcpy(cstrings.sectname, "__cstring");
  strcpy(cstrings.segname, "__TEXT");
  cstrings.addr = text.addr + text.size;
  cstrings.size = total_str_size;
  cstrings.offset = segment.fileoff + text.size;
  cstrings.align = ILOG2(str_align);
  cstrings.reloff = 0; // no relocs
  cstrings.nreloc = 0;
  cstrings.flags = S_CSTRING_LITERALS;
  cstrings.reserved1 = 0;
  cstrings.reserved2 = 0;
  cstrings.reserved3 = 0;

  struct section_64 const_data;
  memset(&const_data, 0, sizeof(const_data));
  strcpy(const_data.sectname, "__const");
  strcpy(const_data.segname, "__TEXT");
  const_data.addr = cstrings.addr + cstrings.size;
  const_data.size = total_const_size;
  const_data.offset = segment.fileoff + text.size + cstrings.size;
  const_data.align = ILOG2(const_align);
  const_data.reloff =
      text.reloff + (RELOC_INFO_SIZE * info.num_text_reloc_instrs);
  const_data.nreloc = info.num_const_data_reloc_instrs;
  const_data.flags = S_REGULAR;
  const_data.reserved1 = 0;
  const_data.reserved2 = 0;
  const_data.reserved3 = 0;

  struct section_64 data;
  memset(&data, 0, sizeof(data));
  strcpy(data.sectname, "__data");
  strcpy(data.segname, "__DATA");
  data.addr = const_data.addr + const_data.size;
  data.size = total_data_size;
  data.offset = segment.fileoff + text.size + cstrings.size + const_data.size;
  data.align = ILOG2(data_align);
  data.reloff =
      const_data.reloff + (RELOC_INFO_SIZE * info.num_const_data_reloc_instrs);
  data.nreloc = info.num_data_reloc_instrs;
  data.flags = S_REGULAR;
  data.reserved1 = 0;
  data.reserved2 = 0;
  data.reserved3 = 0;

  struct build_version_command version;
  version.cmd = LC_BUILD_VERSION;
  version.cmdsize = sizeof(version);
  version.platform = PLATFORM_MACOS;
  version.minos = ENCODE_MINOS(12, 0, 0);
  version.sdk = ENCODE_SDK(0, 0, 0);
  version.ntools = 0;

  struct symtab_command symtab;
  memset(&symtab, 0, sizeof(symtab));
  symtab.cmd = LC_SYMTAB;
  symtab.cmdsize = sizeof(symtab);
  symtab.symoff = relocs_offset + RELOC_INFO_SIZE * total_reloc_instrs;
  symtab.nsyms = args->num_entries;
  symtab.stroff = symtab.symoff + sizeof(struct nlist_64) * symtab.nsyms;

  size_t total_sym_str_len = 0;
  for (size_t i = 0; i < args->num_entries; i++) {
    total_sym_str_len += strlen(args->entries[i].symbol.name);
    total_sym_str_len++; // null terminator
  }

  symtab.strsize = total_sym_str_len;

  size_t num_defined_symbols = 0;
  // finally add offsets to the sections
  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *entry = &args->entries[i];

    if (entry->symbol.visibility != SYMBOL_VISIBILITY_UNDEF) {
      num_defined_symbols++;
    }
  }

  // "local" symbols are debugging
  // "external" symbols are what we call symbols
  // "undefined" symbols are what we call external symbols
  struct dysymtab_command dysymtab;
  memset(&dysymtab, 0, sizeof(dysymtab));
  dysymtab.cmd = LC_DYSYMTAB;
  dysymtab.cmdsize = sizeof(dysymtab);
  dysymtab.iextdefsym = 0;
  dysymtab.nextdefsym = num_defined_symbols;
  dysymtab.iundefsym = num_defined_symbols;
  dysymtab.nundefsym = args->num_entries - num_defined_symbols;

  fwrite(&segment, sizeof(segment), 1, file);
  fwrite(&text, sizeof(text), 1, file);
  fwrite(&cstrings, sizeof(cstrings), 1, file);
  fwrite(&const_data, sizeof(const_data), 1, file);
  fwrite(&data, sizeof(data), 1, file);

  fwrite(&version, sizeof(version), 1, file);
  fwrite(&symtab, sizeof(symtab), 1, file);
  fwrite(&dysymtab, sizeof(dysymtab), 1, file);

  /* Functions */

  fseek(file, text.offset, SEEK_SET);

  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *entry = &args->entries[i];

    if (entry->ty == OBJECT_ENTRY_TY_FUNC) {
      fwrite(entry->data, 1, entry->len_data, file);
      size_t align = ROUND_UP(entry->len_data, func_align) - entry->len_data;
      for (size_t j = 0; j < align; j++) {
        fputc(0, file);
      }
    }
  }

  /* C Strings */

  fseek(file, cstrings.offset, SEEK_SET);

  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *entry = &args->entries[i];

    if (entry->ty == OBJECT_ENTRY_TY_C_STRING) {
      fwrite(entry->data, 1, entry->len_data, file);
      size_t align = ROUND_UP(entry->len_data, str_align) - entry->len_data;
      for (size_t j = 0; j < align; j++) {
        fputc(0, file);
      }
    }
  }

  /* Const data */

  fseek(file, const_data.offset, SEEK_SET);

  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *entry = &args->entries[i];

    if (entry->ty == OBJECT_ENTRY_TY_CONST_DATA) {
      fwrite(entry->data, 1, entry->len_data, file);

      size_t align = ROUND_UP(entry->len_data, const_align) - entry->len_data;
      for (size_t j = 0; j < align; j++) {
        fputc(0, file);
      }
    }
  }

  /* Mutable data */

  fseek(file, data.offset, SEEK_SET);

  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *entry = &args->entries[i];

    if (entry->ty == OBJECT_ENTRY_TY_MUT_DATA && entry->data) {
      fwrite(entry->data, 1, entry->len_data, file);
      size_t align = ROUND_UP(entry->len_data, data_align) - entry->len_data;
      for (size_t j = 0; j < align; j++) {
        fputc(0, file);
      }
    }
  }

  /* Relocations */

  fseek(file, text.reloff, SEEK_SET);
  write_relocations(file, &info);

  /* Symbol table */

  fseek(file, symtab.symoff, SEEK_SET);

  size_t func_offset = text.addr;
  size_t str_offset = cstrings.addr;
  size_t const_offset = const_data.addr;
  size_t data_offset = data.addr;

  size_t str_off = 1; // first index is just a `null` char
  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *entry = &args->entries[i];
    const struct symbol *symbol = &entry->symbol;

    uint8_t n_sect = NO_SECT;
    uint64_t n_value = 0;
    uint8_t n_type = 0;
    uint16_t n_desc = 0;

    if (symbol->visibility != SYMBOL_VISIBILITY_UNDEF) {
      switch (symbol->ty) {
      case SYMBOL_TY_FUNC:
        n_sect = 1;
        n_value = func_offset;
        func_offset += ROUND_UP(entry->len_data, func_align);
        break;
      case SYMBOL_TY_STRING:
        n_sect = 2;
        n_value = str_offset;
        str_offset += ROUND_UP(entry->len_data, str_align);
        break;
      case SYMBOL_TY_CONST_DATA:
        n_sect = 3;
        n_value = const_offset;
        const_offset += ROUND_UP(entry->len_data, const_align);
        break;
      case SYMBOL_TY_DATA:
        n_sect = 4;
        n_value = data_offset;
        data_offset += ROUND_UP(entry->len_data, data_align);
        break;
      case SYMBOL_TY_DECL:
        BUG("DECL symbol must be VISIBILITY_UNDEF");
      }
    }

    switch (symbol->visibility) {
    case SYMBOL_VISIBILITY_GLOBAL:
      if (symbol->flags & SYMBOL_FLAG_WEAK) {
        n_desc |= N_WEAK_DEF;
      }

      n_type = N_SECT | N_EXT;
      break;
    case SYMBOL_VISIBILITY_PRIVATE:
      if (symbol->flags & SYMBOL_FLAG_WEAK) {
        n_desc |= N_WEAK_DEF;
      }

      n_type = N_SECT;
      break;
    case SYMBOL_VISIBILITY_UNDEF:
      if (symbol->flags & SYMBOL_FLAG_WEAK) {
        n_desc |= N_WEAK_REF;
      }

      n_type = N_EXT | N_UNDF;
      break;
    }

    struct nlist_64 nlist;
    nlist.n_un.n_strx = str_off;
    nlist.n_type = n_type;
    nlist.n_sect = n_sect;
    nlist.n_desc = n_desc;
    nlist.n_value = n_value;

    str_off += strlen(symbol->name) + 1;
    fwrite(&nlist, sizeof(nlist), 1, file);
  }

  char null = 0;
  fwrite(&null, sizeof(null), 1, file);

  /* String table */

  for (size_t i = 0; i < args->num_entries; i++) {
    const struct symbol *symbol = &args->entries[i].symbol;
    fwrite(symbol->name, strlen(symbol->name) + 1, 1, file);
  }

  free(entry_offsets);

  vector_free(&info.text_relocs);
  vector_free(&info.const_data_relocs);
  vector_free(&info.data_relocs);
}

void write_macho(const struct build_object_args *args) {
  write_mach_header(args->output, args->compile_args);
  write_segment_command(args);
}
