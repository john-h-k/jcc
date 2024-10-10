#include "mach-o.h"

#include "../compiler.h"
#include "../log.h"
#include "../util.h"

#include <mach-o/arm64/reloc.h>
#include <mach-o/loader.h>
#include <mach-o/reloc.h>
#include <mach/machine.h>
#include <stdint.h>
#include <stdio.h>

void write_mach_header(FILE *file, const struct compile_args *args) {
  struct mach_header_64 header;
  memset(&header, 0, sizeof(header));

  header.magic = MH_MAGIC_64;

  switch (args->target_arch) {
  case COMPILE_TARGET_ARCH_NATIVE:
    bug("NATIVE arch reached linker, should have been selected earlier");
    break;
  case COMPILE_TARGET_ARCH_MACOS_ARM64:
    header.cputype = CPU_TYPE_ARM64;
    header.cpusubtype = CPU_SUBTYPE_ARM64_ALL;
    break;
  case COMPILE_TARGET_ARCH_MACOS_X86_64:
    header.cputype = CPU_TYPE_X86_64;
    header.cpusubtype = CPU_SUBTYPE_X86_64_ALL;
    todo("unsupported arch x86_64");
  case COMPILE_TARGET_ARCH_EEP:
    todo("mach-o does not support EEP");
    break;
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

size_t count_relocation_instrs(const struct build_object_args *args) {
  size_t num_instrs = 0;

  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *entry = &args->entries[i];

    for (size_t j = 0; j < entry->num_relocations; j++) {
      const struct relocation *reloc = &entry->relocations[j];

      switch (reloc->ty) {
      case RELOCATION_TY_SINGLE:
        num_instrs += 1;
        break;
      case RELOCATION_TY_LOCAL_PAIR:
      case RELOCATION_TY_UNDEF_PAIR:
        num_instrs += 2;
      }
    }
  }

  return num_instrs;
}

void write_relocations(FILE *file, const struct build_object_args *args,
                       const size_t *entry_offsets) {
  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *entry = &args->entries[i];
    size_t offset = entry_offsets[i];

    for (size_t j = 0; j < entry->num_relocations; j++) {
      const struct relocation *reloc = &entry->relocations[j];

      size_t index = reloc->symbol_index;
      size_t addr = offset + reloc->address;

      switch (reloc->ty) {
      case RELOCATION_TY_SINGLE: {

        struct relocation_info info = {.r_address = addr,
                                       .r_length = reloc->size,
                                       .r_pcrel = 1,
                                       .r_symbolnum = index,
                                       .r_extern = 1,
                                       .r_type = ARM64_RELOC_BRANCH26};

        fwrite(&info, sizeof(info), 1, file);

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

        fwrite(infos, sizeof(infos), 1, file);

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

        fwrite(infos, sizeof(infos), 1, file);

        break;
      }
      }
    }
  }
}

void write_segment_command(FILE *file, const struct build_object_args *args) {
  size_t str_align = 0;
  size_t func_align = 0;
  size_t const_align = 0;
  size_t data_align = 0;

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

  // finally add offsets to the sections
  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *entry = &args->entries[i];

    switch (entry->ty) {
    case OBJECT_ENTRY_TY_FUNC:
      break;
    case OBJECT_ENTRY_TY_C_STRING:
      entry_offsets[i] += total_func_size;
      break;
    case OBJECT_ENTRY_TY_CONST_DATA:
      entry_offsets[i] += total_func_size + total_str_size;
      break;
    case OBJECT_ENTRY_TY_MUT_DATA:
      entry_offsets[i] += total_func_size + total_str_size + total_const_size;
      break;
    case OBJECT_ENTRY_TY_DECL:
      break;
    }
  }

  size_t total_size =
      total_func_size + total_str_size + total_const_size + total_data_size;

  // FIXME: alignments need to be log2(align) not align

#define LOG2(x) ((sizeof((x)) * 8 - lzcnt(x)) - 1)

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

  struct section_64 text;
  memset(&text, 0, sizeof(text));
  strcpy(text.sectname, "__text");
  strcpy(text.segname, "__TEXT");
  text.addr = 0;
  text.size = total_func_size;
  text.offset = segment.fileoff;
  text.align = LOG2(func_align);
  text.reloff = segment.fileoff + total_size;
  text.nreloc = count_relocation_instrs(args);
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
  cstrings.align = LOG2(str_align);
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
  const_data.align = LOG2(const_align);
  const_data.reloff = 0; // no relocs
  const_data.nreloc = 0;
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
  data.align = LOG2(data_align);
  data.reloff = 0; // no relocs
  data.nreloc = 0;
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
  symtab.symoff = segment.fileoff + text.size + cstrings.size +
                  const_data.size + data.size +
                  sizeof(struct relocation_info) * text.nreloc;
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

    if (entry->ty == OBJECT_ENTRY_TY_MUT_DATA) {
      fwrite(entry->data, 1, entry->len_data, file);
      size_t align = ROUND_UP(entry->len_data, data_align) - entry->len_data;
      for (size_t j = 0; j < align; j++) {
        fputc(0, file);
      }
    }
  }

  /* Relocations */

  fseek(file, text.reloff, SEEK_SET);
  write_relocations(file, args, entry_offsets);

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
        bug("DECL symbol must be VISIBILITY_UNDEF");
        break;
      }
    }

    switch (symbol->visibility) {
    case SYMBOL_VISIBILITY_GLOBAL:
      n_type = N_SECT | N_EXT;
      break;
    case SYMBOL_VISIBILITY_PRIVATE:
      n_type = N_SECT;
      break;
    case SYMBOL_VISIBILITY_UNDEF:
      n_type = N_EXT | N_UNDF;
      break;
    }

    struct nlist_64 nlist;
    nlist.n_un.n_strx = str_off;
    nlist.n_type = n_type;
    nlist.n_sect = n_sect;
    nlist.n_desc = 0;
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
}

void write_macho(const struct build_object_args *args) {
  FILE *file = fopen(args->output, "wb");
  if (file == NULL) {
    perror("fopen");
    exit(EXIT_FAILURE);
  }

  write_mach_header(file, args->compile_args);
  write_segment_command(file, args);

  fclose(file);
}
