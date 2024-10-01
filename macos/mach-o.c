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
      sizeof(struct segment_command_64) + sizeof(struct section_64) +
      sizeof(struct section_64) + sizeof(struct build_version_command) +
      sizeof(struct symtab_command) + sizeof(struct dysymtab_command);
  header.flags = 0;

  fwrite(&header, sizeof(header), 1, file);
}

#define ENCODE(x, y, z) ((x & 0xFF) << 16) | ((y & 0x0F) << 8) | (z & 0x0F)
#define ENCODE_MINOS(x, y, z)                                                  \
  ((x & 0xFF) << 16) | ((y & 0x0F) << 8) | (z & 0x0F)
#define ENCODE_SDK(x, y, z) ((x & 0xFF) << 16) | ((y & 0x0F) << 8) | (z & 0x0F)

size_t count_relocation_instrs(const struct build_object_args *args) {
  size_t num_instrs = 0;

  for (size_t i = 0; i < args->num_relocations; i++) {
    struct relocation *reloc = &args->relocations[i];

    switch (reloc->ty) {
    case RELOCATION_TY_SINGLE:
      num_instrs += 1;
      break;
    case RELOCATION_TY_PAIR:
      num_instrs += 2;
    }
  }

  return num_instrs;
}

void write_relocations(FILE *file, const struct build_object_args *args) {
  // FIXME: currently str relocs are based on hardcoded indices that link the
  // compiler/emitter/object builder they should probably go to a hashmap
  // against id like how the other relocs work in current state, it is
  // _required_ that you see all the SINGLE relocs before the PAIR relocs it
  // goes symbols -> strings -> external symbols

  size_t num_non_str_syms = 0;
  for (size_t i = 0; i < args->num_symbols; i++) {
    struct symbol *symbol = &args->symbols[i];
    if (symbol->ty != SYMBOL_TY_STRING) {
      num_non_str_syms++;
    }
  }

  for (size_t i = 0; i < args->num_relocations; i++) {
    struct relocation *reloc = &args->relocations[i];

    switch (reloc->ty) {
    case RELOCATION_TY_SINGLE: {
      size_t index = SIZE_T_MAX;

      // FIXME: lookup instead of this mess lolz
      for (size_t i = 0; i < args->num_symbols; i++) {
        struct symbol *symbol = &args->symbols[i];
        if (strcmp(symbol->name, reloc->sym.symbol_name) == 0) {
          index = i;
        }
      }

      for (size_t i = 0; i < args->num_extern_symbols; i++) {
        struct external_symbol *symbol = &args->extern_symbols[i];
        if (strcmp(symbol->name, reloc->sym.symbol_name) == 0) {
          index = args->num_symbols + i;
        }
      }

      invariant_assert(index != SIZE_T_MAX, "could not find symbol for reloc");

      struct relocation_info info = {.r_address = reloc->address,
                                     .r_length = reloc->size,
                                     .r_pcrel = 1,
                                     .r_symbolnum = index,
                                     .r_extern = 1,
                                     .r_type = ARM64_RELOC_BRANCH26};

      fwrite(&info, sizeof(info), 1, file);

      break;
    }
    case RELOCATION_TY_PAIR: {
      size_t index = num_non_str_syms + reloc->str.str_index;

      struct relocation_info infos[2] = {
          {.r_address = reloc->address,
           .r_length = reloc->size,
           .r_pcrel = 1,
           .r_symbolnum = index,
           .r_extern = 1,
           .r_type = ARM64_RELOC_PAGE21},
          {.r_address = reloc->address + 4,
           .r_length = reloc->size,
           .r_pcrel = 0,
           .r_symbolnum = index,
           .r_extern = 1,
           .r_type = ARM64_RELOC_PAGEOFF12},
      };

      fwrite(infos, sizeof(infos), 1, file);

      break;
    }
    }
  }
}

void write_segment_command(FILE *file, const struct build_object_args *args) {
  size_t total_str_size = 0;
  for (size_t i = 0; i < args->num_strings; i++) {
    total_str_size += strlen(args->strings[i]) + 1 /* null */;
  }

  total_str_size = ROUND_UP(total_str_size, 4);

  struct segment_command_64 segment;
  memset(&segment, 0, sizeof(segment));
  segment.cmd = LC_SEGMENT_64;
  segment.cmdsize = sizeof(segment) + sizeof(struct section_64) * 2;
  strcpy(segment.segname, ""); // name can just be blank "__TEXT");
  segment.vmaddr = 0;
  segment.vmsize = args->len_data + total_str_size;
  segment.fileoff =
      sizeof(struct mach_header_64) + sizeof(struct segment_command_64) +
      sizeof(struct section_64) + sizeof(struct section_64) +
      sizeof(struct symtab_command) + sizeof(struct dysymtab_command) +
      sizeof(struct build_version_command);
  segment.filesize = args->len_data + total_str_size;
  segment.maxprot = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE;
  segment.initprot = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE;
  segment.nsects = 2;
  segment.flags = 0;

  struct section_64 text;
  memset(&text, 0, sizeof(text));
  strcpy(text.sectname, "__text");
  strcpy(text.segname, "__TEXT");
  text.addr = 0;
  text.size = args->len_data;
  text.offset = segment.fileoff;
  text.align = 4;
  text.reloff = segment.fileoff + text.size + total_str_size;
  text.nreloc = count_relocation_instrs(args);
  text.flags = S_REGULAR | S_ATTR_PURE_INSTRUCTIONS; /*| S_ATTR_EXT_RELOC;*/
  text.reserved1 = 0;
  text.reserved2 = 0;
  text.reserved3 = 0;

  struct section_64 cstrings;
  memset(&cstrings, 0, sizeof(cstrings));
  strcpy(cstrings.sectname, "__cstring");
  strcpy(cstrings.segname, "__TEXT");
  cstrings.addr = text.size;
  cstrings.size = total_str_size;
  cstrings.offset = segment.fileoff + text.size;
  cstrings.align = 0;
  cstrings.reloff = 0; // no relocs
  cstrings.nreloc = 0;
  cstrings.flags = S_CSTRING_LITERALS; // | S_ATTR_EXT_RELOC;
  cstrings.reserved1 = 0;
  cstrings.reserved2 = 0;
  cstrings.reserved3 = 0;

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
                  sizeof(struct relocation_info) * text.nreloc;
  debug("num symbols %d", args->num_symbols);
  symtab.nsyms = args->num_symbols + args->num_extern_symbols;
  symtab.stroff = symtab.symoff + sizeof(struct nlist_64) * symtab.nsyms;

  size_t total_str_len = 0;
  for (size_t i = 0; i < args->num_symbols; i++) {
    debug("symbol %s", args->symbols[i].name);
    total_str_len += strlen(args->symbols[i].name);
    total_str_len++; // null terminator
  }

  for (size_t i = 0; i < args->num_extern_symbols; i++) {
    debug("external symbol %s", args->extern_symbols[i].name);
    total_str_len += strlen(args->extern_symbols[i].name);
    total_str_len++; // null terminator
  }

  symtab.strsize = total_str_len;

  // "local" symbols are debugging
  // "external" symbols are what we call symbols
  // "undefined" symbols are what we call external symbols
  struct dysymtab_command dysymtab;
  memset(&dysymtab, 0, sizeof(dysymtab));
  dysymtab.cmd = LC_DYSYMTAB;
  dysymtab.cmdsize = sizeof(dysymtab);
  dysymtab.iextdefsym = 0;
  dysymtab.nextdefsym = args->num_symbols;
  dysymtab.iundefsym = args->num_symbols;
  dysymtab.nundefsym = args->num_extern_symbols;

  fwrite(&segment, sizeof(segment), 1, file);
  fwrite(&text, sizeof(text), 1, file);
  fwrite(&cstrings, sizeof(cstrings), 1, file);
  fwrite(&version, sizeof(version), 1, file);
  fwrite(&symtab, sizeof(symtab), 1, file);
  fwrite(&dysymtab, sizeof(dysymtab), 1, file);

  /* C Strings */

  fseek(file, cstrings.offset, SEEK_SET);

  for (size_t i = 0; i < args->num_strings; i++) {
    fwrite(args->strings[i], 1, strlen(args->strings[i]) + 1, file);
  }

  /* Data */

  fseek(file, text.offset, SEEK_SET);
  fwrite(args->data, 1, args->len_data, file);

  /* Relocations */

  fseek(file, text.reloff, SEEK_SET);
  write_relocations(file, args);

  /* Symbol table */

  fseek(file, symtab.symoff, SEEK_SET);

  size_t str_off = 1; // first index is just a `null` char
  for (size_t i = 0; i < args->num_symbols; i++) {
    struct symbol *symbol = &args->symbols[i];

    uint8_t n_sect;
    uint64_t n_value;
    uint8_t n_type;
    switch (symbol->ty) {
    case SYMBOL_TY_FUNC:
      n_sect = 1;
      n_value = symbol->value;
      break;
    case SYMBOL_TY_STRING:
      n_sect = 2;
      n_value = symbol->value + args->len_data;
      break;
    }

    switch (symbol->visibility) {
    case SYMBOL_VISIBILITY_GLOBAL:
      n_type = N_SECT | N_EXT;
      break;
    case SYMBOL_VISIBILITY_PRIVATE:
      n_type = N_SECT;
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

  for (size_t i = 0; i < args->num_extern_symbols; i++) {
    struct external_symbol *symbol = &args->extern_symbols[i];

    struct nlist_64 nlist;
    nlist.n_un.n_strx = str_off;
    nlist.n_type = N_UNDF | N_EXT;
    nlist.n_sect = NO_SECT;
    nlist.n_desc = 0;
    nlist.n_value = 0;

    str_off += strlen(symbol->name) + 1;
    fwrite(&nlist, sizeof(nlist), 1, file);
  }

  char null = 0;
  fwrite(&null, sizeof(null), 1, file);

  /* String table */

  for (size_t i = 0; i < args->num_symbols; i++) {
    struct symbol *symbol = &args->symbols[i];
    fwrite(symbol->name, strlen(symbol->name) + 1, 1, file);
  }
  for (size_t i = 0; i < args->num_extern_symbols; i++) {
    struct external_symbol *symbol = &args->extern_symbols[i];
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
