#include "mach-o.h"
#include "../util.h"
#include "../log.h"
#include <mach-o/loader.h>
#include <mach/machine.h>
#include <stdio.h>
#include <sys/_types/_seek_set.h>

void write_mach_header(FILE *file, const struct compile_args *args) {
    struct mach_header_64 header;
    memset(&header, 0, sizeof(header));

    header.magic = MH_MAGIC_64;

    switch (args->target_arch) {
    case COMPILE_TARGET_ARCH_MACOS_ARM64:
      err("hi");
      header.cputype = CPU_TYPE_ARM64;
      header.cpusubtype = CPU_SUBTYPE_ARM64_ALL;
      break;
    case COMPILE_TARGET_ARCH_MACOS_X86_64:
      header.cputype = CPU_TYPE_X86_64;
      header.cpusubtype = CPU_SUBTYPE_X86_64_ALL;
      todo("unsupported arch x86_64");
    }

    header.filetype = MH_OBJECT;
    header.ncmds = 3;
    header.sizeofcmds = sizeof(struct segment_command_64) + sizeof(struct section_64) + sizeof(struct symtab_command) + sizeof(struct build_version_command);
    header.flags = 0;

    fwrite(&header, sizeof(header), 1, file);
}

#define ENCODE(x, y, z) ((x & 0xFF) << 16) | ((y & 0x0F) << 8) | (z & 0x0F)
#define ENCODE_MINOS(x, y, z) ((x & 0xFF) << 16) | ((y & 0x0F) << 8) | (z & 0x0F)
#define ENCODE_SDK(x, y, z) ((x & 0xFF) << 16) | ((y & 0x0F) << 8) | (z & 0x0F)

void write_segment_command(FILE *file, const struct macho_args* args) {    
    struct segment_command_64 segment;
    memset(&segment, 0, sizeof(segment));
    segment.cmd = LC_SEGMENT_64;
    segment.cmdsize = sizeof(segment) + sizeof(struct section_64);
    strcpy(segment.segname, ""); // name can just be blank "__TEXT");
    segment.vmaddr = 0;
    segment.vmsize = args->len_data;
    segment.fileoff = sizeof(struct mach_header_64)
        + sizeof(struct segment_command_64)
        + sizeof(struct section_64)
        + sizeof(struct symtab_command)
        + sizeof(struct build_version_command);
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
    section.offset = segment.fileoff;//sizeof(struct mach_header_64) + segment.cmdsize;
    section.align = 4;
    section.reloff = 0;
    section.nreloc = 0;
    section.flags = S_REGULAR | S_ATTR_PURE_INSTRUCTIONS;
    section.reserved1 = 0;
    section.reserved2 = 0;
    section.reserved3 = 0;

    err("sec size %d", section.size);

    struct symtab_command symtab;
    memset(&symtab, 0, sizeof(symtab));
    symtab.cmd = LC_SYMTAB;
    symtab.cmdsize = sizeof(symtab);
    symtab.symoff = segment.fileoff + section.size;
    symtab.nsyms = args->num_symbols;
    symtab.stroff = symtab.symoff + sizeof(struct nlist_64) * args->num_symbols;

    size_t total_str_len = 0;
    for (size_t i = 0; i < args->num_symbols; i++) {
        total_str_len += strlen(args->symbols[i].name);
        total_str_len++; // null terminator
    }
    
    symtab.strsize = total_str_len;

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
    fwrite(&version, sizeof(version), 1, file);

    // size_t pos = ftell(file);
    // err("pos is %zu and expected is %zu", pos, section.offset);
    // fseek(file, section.offset, SEEK_SET);
    fwrite(args->data, 1, args->len_data, file);

    // err("pos is %zu and expected is %zu", ftell(file), symtab.symoff);
    fseek(file, symtab.symoff, SEEK_SET);
    for (size_t i = 0; i < args->num_symbols; i++) {
        struct symbol *symbol = &args->symbols[i];

        struct nlist_64 nlist;
        nlist.n_un.n_strx = i + 1;
        nlist.n_type = N_SECT | N_EXT;
        nlist.n_sect = symbol->section;
        nlist.n_desc = REFERENCE_FLAG_PRIVATE_UNDEFINED_LAZY | REFERENCED_DYNAMICALLY;
        nlist.n_value = symbol->value;

        fwrite(&nlist, sizeof(nlist), 1, file);
    }

    char null = 0;
    fwrite(&null, sizeof(null), 1, file);

    // err("pos is %zu and expected is %zu", ftell(file), symtab.stroff);
    for (size_t i = 0; i < args->num_symbols; i++) {
        struct symbol *symbol = &args->symbols[i];
        err("writing symbol %s", symbol->name);
        fwrite(symbol->name, strlen(symbol->name) + 1, 1, file);
    }    
}

void write_macho(const struct macho_args* args) {
    FILE *file = fopen(args->output, "wb");
    if (file == NULL) {
        perror("fopen");
        exit(EXIT_FAILURE);
    }

    write_mach_header(file, args->compile_args);
    write_segment_command(file, args);

    fclose(file);
}
