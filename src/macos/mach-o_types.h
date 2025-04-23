#ifndef MACOS_MACH_O_TYPES_H
#define MACOS_MACH_O_TYPES_H

#include <stdint.h>

typedef int integer_t;
typedef int vm_prot_t;
typedef integer_t cpu_type_t;
typedef integer_t cpu_subtype_t;
typedef integer_t cpu_threadtype_t;

#define MH_MAGIC_64 0xfeedfacf
#define MH_CIGAM_64 0xcffaedfe

#define CPU_ARCH_MASK 0xff000000
#define CPU_ARCH_ABI64 0x01000000
#define CPU_ARCH_ABI64_32 0x02000000

#define CPU_TYPE_X86 ((cpu_type_t)7)
#define CPU_TYPE_I386 CPU_TYPE_X86
#define CPU_TYPE_X86_64 (CPU_TYPE_X86 | CPU_ARCH_ABI64)

#define CPU_TYPE_ARM ((cpu_type_t)12)
#define CPU_TYPE_ARM64 (CPU_TYPE_ARM | CPU_ARCH_ABI64)
#define CPU_TYPE_ARM64_32 (CPU_TYPE_ARM | CPU_ARCH_ABI64_32)
#define CPU_TYPE_POWERPC ((cpu_type_t)18)
#define CPU_TYPE_POWERPC64 (CPU_TYPE_POWERPC | CPU_ARCH_ABI64)

#define CPU_SUBTYPE_ARM64_ALL ((cpu_subtype_t)0)
#define CPU_SUBTYPE_ARM64_V8 ((cpu_subtype_t)1)
#define CPU_SUBTYPE_ARM64E ((cpu_subtype_t)2)

#define CPU_SUBTYPE_X86_ALL ((cpu_subtype_t)3)
#define CPU_SUBTYPE_X86_64_ALL ((cpu_subtype_t)3)
#define CPU_SUBTYPE_X86_ARCH1 ((cpu_subtype_t)4)
#define CPU_SUBTYPE_X86_64_H ((cpu_subtype_t)8)

#define CPU_THREADTYPE_INTEL_HTT ((cpu_threadtype_t)1)

#define MH_OBJECT 0x1
#define MH_EXECUTE 0x2
#define MH_FVMLIB 0x3
#define MH_CORE 0x4
#define MH_PRELOAD 0x5
#define MH_DYLIB 0x6
#define MH_DYLINKER 0x7
#define MH_BUNDLE 0x8
#define MH_DYLIB_STUB 0x9
#define MH_DSYM 0xa
#define MH_KEXT_BUNDLE 0xb
#define MH_FILESET 0xc
#define MH_GPU_EXECUTE 0xd
#define MH_GPU_DYLIB 0xe

/***** VM_PROT *****/
#define VM_PROT_NONE ((vm_prot_t)0x00)

#define VM_PROT_READ ((vm_prot_t)0x01)
#define VM_PROT_WRITE ((vm_prot_t)0x02)
#define VM_PROT_EXECUTE ((vm_prot_t)0x04)

#define VM_PROT_DEFAULT (VM_PROT_READ | VM_PROT_WRITE)
#define VM_PROT_ALL (VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE)
#define VM_PROT_RORW_TP (VM_PROT_EXECUTE)

/***** S_ATTR *****/

#define SECTION_ATTRIBUTES_USR 0xff000000
#define S_ATTR_PURE_INSTRUCTIONS 0x80000000
#define S_ATTR_NO_TOC 0x40000000
#define S_ATTR_STRIP_STATIC_SYMS 0x20000000
#define S_ATTR_NO_DEAD_STRIP 0x10000000
#define S_ATTR_LIVE_SUPPORT 0x08000000
#define S_ATTR_SELF_MODIFYING_CODE 0x04000000
#define S_ATTR_DEBUG 0x02000000
#define SECTION_ATTRIBUTES_SYS 0x00ffff00
#define S_ATTR_SOME_INSTRUCTIONS 0x00000400
#define S_ATTR_EXT_RELOC 0x00000200
#define S_ATTR_LOC_RELOC 0x00000100

#define SECTION_TYPE 0x000000ff
#define SECTION_ATTRIBUTES 0xffffff00

#define S_REGULAR 0x0
#define S_ZEROFILL 0x1
#define S_CSTRING_LITERALS 0x2
#define S_4BYTE_LITERALS 0x3
#define S_8BYTE_LITERALS 0x4
#define S_LITERAL_POINTERS 0x5

/***** Relocations *****/

enum reloc_type_arm64 {
  ARM64_RELOC_UNSIGNED,
  ARM64_RELOC_SUBTRACTOR,
  ARM64_RELOC_BRANCH26,
  ARM64_RELOC_PAGE21,
  ARM64_RELOC_PAGEOFF12,
  ARM64_RELOC_GOT_LOAD_PAGE21,
  ARM64_RELOC_GOT_LOAD_PAGEOFF12,
  ARM64_RELOC_POINTER_TO_GOT,
  ARM64_RELOC_TLVP_LOAD_PAGE21,
  ARM64_RELOC_TLVP_LOAD_PAGEOFF12,
  ARM64_RELOC_ADDEND,
  ARM64_RELOC_AUTHENTICATED_POINTER,
};

enum reloc_type_x86_64 {
  X86_64_RELOC_UNSIGNED,
  X86_64_RELOC_SIGNED,
  X86_64_RELOC_BRANCH,
  X86_64_RELOC_GOT_LOAD,
  X86_64_RELOC_GOT,
  X86_64_RELOC_SUBTRACTOR,
  X86_64_RELOC_SIGNED_1,
  X86_64_RELOC_SIGNED_2,
  X86_64_RELOC_SIGNED_4,
  X86_64_RELOC_TLV,
};

struct relocation_info {
  int32_t r_address;
  uint32_t r_symbolnum : 24;
  uint32_t r_pcrel : 1;
  uint32_t r_length : 2;
  uint32_t r_extern : 1;
  uint32_t r_type : 4;
};

/***** Commands *****/

struct segment_command_64 {
  uint32_t cmd;
  uint32_t cmdsize;
  char segname[16];
  uint64_t vmaddr;
  uint64_t vmsize;
  uint64_t fileoff;
  uint64_t filesize;
  int32_t maxprot;
  int32_t initprot;
  uint32_t nsects;
  uint32_t flags;
};

struct mach_header_64 {
  uint32_t magic;
  int32_t cputype;
  int32_t cpusubtype;
  uint32_t filetype;
  uint32_t ncmds;
  uint32_t sizeofcmds;
  uint32_t flags;
  uint32_t reserved;
};

struct section_64 {
  char sectname[16];
  char segname[16];
  uint64_t addr;
  uint64_t size;
  uint32_t offset;
  uint32_t align;
  uint32_t reloff;
  uint32_t nreloc;
  uint32_t flags;
  uint32_t reserved1;
  uint32_t reserved2;
  uint32_t reserved3;
};

struct symtab_command {
  uint32_t cmd;
  uint32_t cmdsize;
  uint32_t symoff;
  uint32_t nsyms;
  uint32_t stroff;
  uint32_t strsize;
};

struct dysymtab_command {
  uint32_t cmd;
  uint32_t cmdsize;

  uint32_t ilocalsym;
  uint32_t nlocalsym;

  uint32_t iextdefsym;
  uint32_t nextdefsym;

  uint32_t iundefsym;
  uint32_t nundefsym;

  uint32_t tocoff;
  uint32_t ntoc;

  uint32_t modtaboff;
  uint32_t nmodtab;

  uint32_t extrefsymoff;
  uint32_t nextrefsyms;

  uint32_t indirectsymoff;
  uint32_t nindirectsyms;

  uint32_t extreloff;
  uint32_t nextrel;

  uint32_t locreloff;
  uint32_t nlocrel;
};

struct build_version_command {
  uint32_t cmd;
  uint32_t cmdsize;
  uint32_t platform;
  uint32_t minos;
  uint32_t sdk;
  uint32_t ntools;
};

#define LC_REQ_DYLD 0x80000000

#define LC_SEGMENT 0x1
#define LC_SYMTAB 0x2
#define LC_SYMSEG 0x3
#define LC_THREAD 0x4
#define LC_UNIXTHREAD 0x5
#define LC_LOADFVMLIB 0x6
#define LC_IDFVMLIB 0x7
#define LC_IDENT 0x8
#define LC_FVMFILE 0x9
#define LC_PREPAGE 0xa
#define LC_DYSYMTAB 0xb
#define LC_LOAD_DYLIB 0xc
#define LC_ID_DYLIB 0xd
#define LC_LOAD_DYLINKER 0xe
#define LC_ID_DYLINKER 0xf
#define LC_PREBOUND_DYLIB 0x10

#define LC_ROUTINES 0x11
#define LC_SUB_FRAMEWORK 0x12
#define LC_SUB_UMBRELLA 0x13
#define LC_SUB_CLIENT 0x14
#define LC_SUB_LIBRARY 0x15
#define LC_TWOLEVEL_HINTS 0x16
#define LC_PREBIND_CKSUM 0x17

#define LC_LOAD_WEAK_DYLIB (0x18 | LC_REQ_DYLD)

#define LC_SEGMENT_64 0x19
#define LC_ROUTINES_64 0x1a
#define LC_UUID 0x1b
#define LC_RPATH (0x1c | LC_REQ_DYLD)
#define LC_CODE_SIGNATURE 0x1d
#define LC_SEGMENT_SPLIT_INFO 0x1e
#define LC_REEXPORT_DYLIB (0x1f | LC_REQ_DYLD)
#define LC_LAZY_LOAD_DYLIB 0x20
#define LC_ENCRYPTION_INFO 0x21
#define LC_DYLD_INFO 0x22
#define LC_DYLD_INFO_ONLY (0x22 | LC_REQ_DYLD)
#define LC_LOAD_UPWARD_DYLIB (0x23 | LC_REQ_DYLD)
#define LC_VERSION_MIN_MACOSX 0x24
#define LC_VERSION_MIN_IPHONEOS 0x25
#define LC_FUNCTION_STARTS 0x26
#define LC_DYLD_ENVIRONMENT 0x27
#define LC_MAIN (0x28 | LC_REQ_DYLD)
#define LC_DATA_IN_CODE 0x29
#define LC_SOURCE_VERSION 0x2A
#define LC_DYLIB_CODE_SIGN_DRS 0x2B
#define LC_ENCRYPTION_INFO_64 0x2C
#define LC_LINKER_OPTION 0x2D
#define LC_LINKER_OPTIMIZATION_HINT 0x2E
#define LC_VERSION_MIN_TVOS 0x2F
#define LC_VERSION_MIN_WATCHOS 0x30
#define LC_NOTE 0x31
#define LC_BUILD_VERSION 0x32
#define LC_DYLD_EXPORTS_TRIE (0x33 | LC_REQ_DYLD)
#define LC_DYLD_CHAINED_FIXUPS (0x34 | LC_REQ_DYLD)
#define LC_FILESET_ENTRY (0x35 | LC_REQ_DYLD)
#define LC_ATOM_INFO 0x36
#define LC_FUNCTION_VARIANTS 0x37
#define LC_FUNCTION_VARIANT_FIXUPS 0x38
#define LC_TARGET_TRIPLE 0x39

/***** Symbols *****/

#define N_STAB 0xe0
#define N_PEXT 0x10
#define N_TYPE 0x0e
#define N_EXT 0x01
#define N_UNDF 0x0
#define N_ABS 0x2
#define N_SECT 0xe
#define N_PBUD 0xc
#define N_INDR 0xa

#define N_NO_DEAD_STRIP 0x0020
#define N_DESC_DISCARDED 0x0020
#define N_WEAK_REF 0x0040
#define N_WEAK_DEF 0x0080
#define N_REF_TO_WEAK 0x0080
#define N_ARM_THUMB_DEF 0x0008
#define N_SYMBOL_RESOLVER 0x0100
#define N_ALT_ENTRY 0x0200k
#define N_COLD_FUNC 0x0400

#define NO_SECT 0    /* symbol is not in any section */
#define MAX_SECT 255 /* 1 thru 255 inclusive */

struct nlist_64 {
  union {
    uint32_t n_strx;
  } n_un;
  uint8_t n_type;
  uint8_t n_sect;
  uint16_t n_desc;
  uint64_t n_value;
};

/***** Platforms *****/

#define PLATFORM_UNKNOWN 0
#define PLATFORM_ANY 0xFFFFFFFF
#define PLATFORM_MACOS 1
#define PLATFORM_IOS 2
#define PLATFORM_TVOS 3
#define PLATFORM_WATCHOS 4
#define PLATFORM_BRIDGEOS 5
#define PLATFORM_MACCATALYST 6
#define PLATFORM_IOSSIMULATOR 7
#define PLATFORM_TVOSSIMULATOR 8
#define PLATFORM_WATCHOSSIMULATOR 9
#define PLATFORM_DRIVERKIT 10
#define PLATFORM_VISIONOS 11
#define PLATFORM_VISIONOSSIMULATOR 12

#endif
