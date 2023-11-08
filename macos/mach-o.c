#include "mach-o.h"
#include "../util.h"
#include <mach/machine.h>

void write_mach_header(FILE *file, const struct compile_args *args) {
    struct mach_header_64 header;
    memset(&header, 0, sizeof(header));

    header.magic = MH_MAGIC_64;

    switch (args->target_arch) {
    case COMPILE_TARGET_ARCH_MACOS_ARM64:
      header.cputype = CPU_TYPE_ARM64;
      header.cpusubtype = CPU_SUBTYPE_ARM64_ALL;
      break;
    case COMPILE_TARGET_ARCH_MACOS_X86_64:
      header.cputype = CPU_TYPE_X86_64;
      header.cpusubtype = CPU_SUBTYPE_X86_64_ALL;
      todo("unsupported arch x86_64");
    }

    header.filetype = MH_OBJECT;
    header.ncmds = 1;
    header.sizeofcmds = sizeof(struct segment_command_64) + sizeof(struct section_64);
    header.flags = 0;

    fwrite(&header, sizeof(header), 1, file);
}

void write_segment_command(FILE *file, const char *sect_name, const char *seg_name, uint64_t sect_size) {
    struct segment_command_64 segment;
    struct section_64 section;
    memset(&segment, 0, sizeof(segment));
    memset(&section, 0, sizeof(section));

    segment.cmd = LC_SEGMENT_64;
    segment.cmdsize = sizeof(segment) + sizeof(section);
    strcpy(segment.segname, "__TEXT");
    segment.vmaddr = 0;
    segment.vmsize = sect_size;
    segment.fileoff = sizeof(struct mach_header_64) + segment.cmdsize;
    segment.filesize = sect_size;
    segment.maxprot = VM_PROT_READ | VM_PROT_EXECUTE;
    segment.initprot = VM_PROT_READ | VM_PROT_EXECUTE;
    segment.nsects = 1;
    segment.flags = 0;

    strcpy(section.sectname, sect_name);
    strcpy(section.segname, seg_name);
    section.addr = 0;
    section.size = sect_size;
    section.offset = sizeof(struct mach_header_64) + segment.cmdsize;
    section.align = 0;
    section.reloff = 0;
    section.nreloc = 0;
    section.flags = S_REGULAR | S_ATTR_PURE_INSTRUCTIONS;
    section.reserved1 = 0;
    section.reserved2 = 0;

    fwrite(&segment, sizeof(segment), 1, file);
    fwrite(&section, sizeof(section), 1, file);
}

void write_macho(const struct compile_args *args, const char *filename, const char *machine_code, size_t code_size) {
    FILE *file = fopen(filename, "wb");
    if (file == NULL) {
        perror("fopen");
        exit(EXIT_FAILURE);
    }

    write_mach_header(file, args);
    write_segment_command(file, "__text", "__TEXT", code_size);
    fwrite(machine_code, 1, code_size, file);

    fclose(file);
}
