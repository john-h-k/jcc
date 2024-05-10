#include "mach-o.h"

#include "../compiler.h"
#include "../log.h"
#include "../util.h"

#include <mach-o/loader.h>
#include <mach/machine.h>
#include <mach-o/reloc.h>
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
      sizeof(struct symtab_command) +  sizeof(struct dysymtab_command) +
      sizeof(struct build_version_command);
  header.flags = 0;

  fwrite(&header, sizeof(header), 1, file);
}

#define ENCODE(x, y, z) ((x & 0xFF) << 16) | ((y & 0x0F) << 8) | (z & 0x0F)
#define ENCODE_MINOS(x, y, z)                                                  \
  ((x & 0xFF) << 16) | ((y & 0x0F) << 8) | (z & 0x0F)
#define ENCODE_SDK(x, y, z) ((x & 0xFF) << 16) | ((y & 0x0F) << 8) | (z & 0x0F)

void write_segment_command(FILE *file, const struct build_object_args *args) {
  struct segment_command_64 segment;
  memset(&segment, 0, sizeof(segment));
  segment.cmd = LC_SEGMENT_64;
  segment.cmdsize = sizeof(segment) + sizeof(struct section_64);
  strcpy(segment.segname, ""); // name can just be blank "__TEXT");
  segment.vmaddr = 0;
  segment.vmsize = args->len_data;
  segment.fileoff =
      sizeof(struct mach_header_64) + sizeof(struct segment_command_64) +
      sizeof(struct section_64) + sizeof(struct symtab_command) +
      sizeof(struct dysymtab_command) + sizeof(struct build_version_command);
  segment.filesize = args->len_data;
  segment.maxprot = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE;
  segment.initprot = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE;
  segment.nsects = 1;
  segment.flags = 0;

  struct section_64 section;
  memset(&section, 0, sizeof(section));
  strcpy(section.sectname, "__text");
  strcpy(section.segname, "__TEXT");
  section.addr = 0;
  section.size = args->len_data;
  section.offset =
      segment.fileoff; // sizeof(struct mach_header_64) + segment.cmdsize;
  section.align = 4;
  section.reloff = segment.fileoff + section.size;
  section.nreloc = args->num_relocations;
  section.flags = S_REGULAR | S_ATTR_PURE_INSTRUCTIONS;
  section.reserved1 = 0;
  section.reserved2 = 0;
  section.reserved3 = 0;

  struct symtab_command symtab;
  memset(&symtab, 0, sizeof(symtab));
  symtab.cmd = LC_SYMTAB;
  symtab.cmdsize = sizeof(symtab);
  symtab.symoff = segment.fileoff + section.size + sizeof(struct relocation_info) * args->num_relocations;
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

  struct build_version_command version;
  version.cmd = LC_BUILD_VERSION;
  version.cmdsize = sizeof(version);
  version.platform = PLATFORM_MACOS;
  version.minos = ENCODE_MINOS(12, 0, 0);
  version.sdk = ENCODE_SDK(0, 0, 0);
  version.ntools = 0;

  fwrite(&segment, sizeof(segment), 1, file);
  fwrite(&section, sizeof(section), 1, file);
  fwrite(&symtab, sizeof(symtab), 1, file);
  fwrite(&dysymtab, sizeof(dysymtab), 1, file);
  fwrite(&version, sizeof(version), 1, file);

  fwrite(args->data, 1, args->len_data, file);

  fseek(file, section.reloff, SEEK_SET);

  /* Relocations */

  for (size_t i = 0; i < args->num_relocations; i++) {
    struct relocation *reloc = &args->relocations[i];
    size_t index;
    int external = -1;

    // FIXME: lookup instead of this mess lolz
    for (size_t i = 0; i < args->num_symbols; i++) {
      struct symbol *symbol = &args->symbols[i];
      if (strcmp(symbol->name, reloc->symbol_name) == 0) {
        index = i;
        external = 0;
      }
    }

    for (size_t i = 0; i < args->num_extern_symbols; i++) {
      struct external_symbol *symbol = &args->extern_symbols[i];
      if (strcmp(symbol->name, reloc->symbol_name) == 0) {
        index = args->num_symbols + i;
        external = 1;
      }
    }

    invariant_assert(external != -1, "could not find symbol for reloc");

    printf("RELOC ADDRESS %zu, EXTERN %d, SYM %zu\n", reloc->address, external, index);
    struct relocation_info info = {
      .r_address = reloc->address,
      .r_length = reloc->size,
      .r_pcrel = 1, // always true ARM64
      .r_symbolnum = index,
      .r_extern = 1,//external,
      // HACK: not always correct
      .r_type = GENERIC_RELOC_SECTDIFF // ARM64_RELOC_BRANCH26
    };

    fwrite(&info, sizeof(info), 1, file);
  }

  /* Symbol table */
  
  size_t str_off = 1; // first index is just a `null` char
  for (size_t i = 0; i < args->num_symbols; i++) {
    struct symbol *symbol = &args->symbols[i];

    struct nlist_64 nlist;
    nlist.n_un.n_strx = str_off;
    nlist.n_type = N_SECT | N_EXT;
    nlist.n_sect = symbol->section;
    nlist.n_desc =
        REFERENCE_FLAG_PRIVATE_UNDEFINED_LAZY;
    nlist.n_value = symbol->value;

    str_off += strlen(symbol->name) + 1;
    fwrite(&nlist, sizeof(nlist), 1, file);
  }

  for (size_t i = 0; i < args->num_extern_symbols; i++) {
    struct external_symbol *symbol = &args->extern_symbols[i];

    struct nlist_64 nlist;
    nlist.n_un.n_strx = str_off;
    nlist.n_type = N_UNDF;
    nlist.n_sect = NO_SECT;
    // should this be lazy or non lazy i am unsure
    nlist.n_desc =
        REFERENCE_FLAG_UNDEFINED_LAZY | REFERENCED_DYNAMICALLY;
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
