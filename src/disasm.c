#include "disasm.h"

#include "alloc.h"
#include "compiler.h"
#include "log.h"
#include "syscmd.h"
#include "util.h"

#include <stdlib.h>

void objdump_debug_disasm(enum compile_target target, const char *filename,
                          const char *output) {
  // tmp arena
  struct arena_allocator *arena;
  arena_allocator_create("disasm", &arena);

  struct syscmd *cmd;
  if (target == COMPILE_TARGET_LINUX_RV32I) {
    cmd = syscmd_create(arena, "riscv64-unknown-elf-objdump");
  } else {
    cmd = syscmd_create(arena, "objdump");
  }

  syscmd_add_arg(cmd, "--disassemble");
  syscmd_add_arg(cmd, "--reloc");

  enum compile_arch arch;
  enum compile_platform platform;
  compile_target_decomp(target, &arch, &platform);

  switch (arch) {
  case COMPILE_ARCH_X86_64:
    // intel syntax
    syscmd_add_arg_val(cmd, "-M", "intel");
    break;
  case COMPILE_ARCH_ARM64:
  case COMPILE_ARCH_RV32I:
    break;
  case COMPILE_ARCH_EEP:
    BUG("eep");
  case COMPILE_ARCH_NATIVE:
    unreachable();
  }

  syscmd_add_arg(cmd, filename);

  if (output) {
    syscmd_set_stdout_path(cmd, SYSCMD_BUF_FLAG_NONE, output);
  }

  if (syscmd_exec(&cmd)) {
    warn("`debug_disasm` failed");
  }

  arena_allocator_free(&arena);
}
