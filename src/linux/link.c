#include "link.h"

#include "../alloc.h"
#include "../compiler.h"
#include "../syscmd.h"
#include "../util.h"

#include <stdlib.h>
#include <string.h>

enum link_result linux_link_objects(const struct link_args *args) {
  struct arena_allocator *arena;
  arena_allocator_create("linux_link", &arena);

  struct syscmd *cmd;

  switch (args->args->target) {
  case COMPILE_TARGET_LINUX_ARM64:
    cmd = syscmd_create(arena, "ld");

    syscmd_add_arg_val(cmd, "-dynamic-linker", "/lib/ld-linux-aarch64.so.1");
    syscmd_add_arg(cmd, "-L/usr/lib/gcc/aarch64-linux-gnu/12");
    syscmd_add_arg(cmd, "-L/lib/aarch64-linux-gnu");
    syscmd_add_arg(cmd, "-L/usr/lib/aarch64-linux-gnu");
    syscmd_add_arg(cmd, "-L/lib");
    syscmd_add_arg(cmd, "-L/usr/lib");
    syscmd_add_arg_val(cmd, "--no-as-needed", "/lib/aarch64-linux-gnu/crt1.o");
    syscmd_add_arg_val(cmd, "--no-as-needed", "/lib/aarch64-linux-gnu/crtn.o");
    break;
  case COMPILE_TARGET_LINUX_X86_64:
    cmd = syscmd_create(arena, "ld");

    syscmd_add_arg_val(cmd, "-dynamic-linker", "/lib64/ld-linux-x86-64.so.2");
    syscmd_add_arg(cmd, "-L/usr/lib/gcc/x86_64-linux-gnu/12");
    syscmd_add_arg(cmd, "-L/lib64/x86-64-linux-gnu");
    syscmd_add_arg(cmd, "-L/usr/lib/x86_64-linux-gnu");
    syscmd_add_arg(cmd, "-L/lib");
    syscmd_add_arg(cmd, "-L/usr/lib");
    syscmd_add_arg_val(cmd, "--no-as-needed",
                       "/usr/lib/x86_64-linux-gnu/crt1.o");
    syscmd_add_arg_val(cmd, "--no-as-needed",
                       "/usr/lib/x86_64-linux-gnu/crtn.o");
    break;
  case COMPILE_TARGET_LINUX_RV32I:
    // FIXME: this will only work on mac
    // need a way to get the riscv paths xplat
    cmd = syscmd_create(arena, "riscv64-unknown-elf-ld");

    syscmd_add_arg_val(
        cmd, "-plugin",
        "/opt/riscv/libexec/gcc/riscv64-unknown-elf/12.2.0/liblto_plugin.so");
    syscmd_add_arg(cmd, "-plugin-opt=/opt/riscv/libexec/gcc/"
                        "riscv64-unknown-elf/12.2.0/lto-wrapper");
    syscmd_add_arg(cmd, "-plugin-opt=-fresolution=/tmp/ccI4xWdp.res");
    syscmd_add_arg(cmd, "-plugin-opt=-pass-through=-lgcc");
    syscmd_add_arg(cmd, "-plugin-opt=-pass-through=-lc");
    syscmd_add_arg(cmd, "-plugin-opt=-pass-through=-lgloss");
    syscmd_add_arg(cmd, "-plugin-opt=-pass-through=-lgcc");
    syscmd_add_arg(cmd, "--sysroot=/opt/riscv/bin/../riscv64-unknown-elf");
    syscmd_add_arg(cmd, "-melf32lriscv");
    syscmd_add_arg(cmd, "/opt/riscv/lib/gcc/riscv64-unknown-elf/12.2.0/../../"
                        "../../riscv64-unknown-elf/lib/rv32imfd/ilp32d/crt0.o");
    syscmd_add_arg(cmd, "/opt/riscv/lib/gcc/riscv64-unknown-elf/12.2.0/"
                        "rv32imfd/ilp32d/crtbegin.o");
    syscmd_add_arg(
        cmd, "-L/opt/riscv/lib/gcc/riscv64-unknown-elf/12.2.0/rv32imfd/ilp32d");
    syscmd_add_arg(cmd, "-L/opt/riscv/lib/gcc/riscv64-unknown-elf/12.2.0/../../"
                        "../../riscv64-unknown-elf/lib/rv32imfd/ilp32d");
    syscmd_add_arg(cmd, "-L/opt/riscv/riscv64-unknown-elf/lib/rv32imfd/ilp32d");
    syscmd_add_arg(cmd, "-L/opt/riscv/lib/gcc/riscv64-unknown-elf/12.2.0");
    syscmd_add_arg(cmd, "-L/opt/riscv/lib/gcc");
    syscmd_add_arg(cmd, "-L/opt/riscv/lib/gcc/riscv64-unknown-elf/12.2.0/../../"
                        "../../riscv64-unknown-elf/lib");
    syscmd_add_arg(cmd, "-L/opt/riscv/riscv64-unknown-elf/lib");
    break;
  default:
    unsupported("bad target for linux linker");
  }

  syscmd_add_arg_val(cmd, "-o", args->output);

  for (size_t i = 0; i < args->num_objects; i++) {
    syscmd_add_arg(cmd, args->objects[i]);
  }

  for (size_t i = 0; i < args->num_linker_args; i++) {
    syscmd_add_arg(cmd, args->linker_args[i]);
  }

  switch (args->args->target) {
  case COMPILE_TARGET_LINUX_ARM64:
  case COMPILE_TARGET_LINUX_X86_64:
    syscmd_add_arg(cmd, "-lc");
    syscmd_add_arg(cmd, "-lm");
    break;
  case COMPILE_TARGET_LINUX_RV32I:
    syscmd_add_arg(cmd, "-lgcc");

    syscmd_add_arg(cmd, "--start-group");
    syscmd_add_arg(cmd, "-lc");
    syscmd_add_arg(cmd, "-lm");
    syscmd_add_arg(cmd, "-lgloss");
    syscmd_add_arg(cmd, "--end-group");

    syscmd_add_arg(cmd, "-lgcc");
    syscmd_add_arg(cmd, "/opt/riscv/bin/../lib/gcc/riscv64-unknown-elf/12.2.0/"
                        "rv32imfd/ilp32d/crtend.o");
    break;
  default:
    unreachable();
  }

  if (args->args->verbose) {
    fprintf(stderr, "link command (platform=linux):\n");
    syscmd_write_cmd(cmd, stderr);
  } else {
    syscmd_set_stdout_path(cmd, SYSCMD_BUF_FLAG_NONE, "/dev/null");
  }

  int ret_code = syscmd_exec(&cmd);

  arena_allocator_free(&arena);

  return ret_code == 0 ? LINK_RESULT_SUCCESS : LINK_RESULT_FAILURE;
}
