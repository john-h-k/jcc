#include "link.h"

#include "../alloc.h"
#include "../compiler.h"
#include "../util.h"

#include <stdlib.h>
#include <string.h>

enum link_result linux_link_objects(const struct link_args *args) {
  struct arena_allocator *arena;
  arena_allocator_create(&arena);

  // using ld for elf linking (linux x86_64, adjust as needed)
  // ld -o output_binary your_object.o
  const char *template_prefix, *template_suffix;
  switch (args->args->target) {
  case COMPILE_TARGET_LINUX_ARM64:
    template_prefix =
        "ld -dynamic-linker /lib/ld-linux-aarch64.so.1 "
        "-L/usr/lib/gcc/aarch64-linux-gnu/12 "
        "-L/lib/aarch64-linux-gnu -L/usr/lib/aarch64-linux-gnu -L/lib "
        "-L/usr/lib --no-as-needed /lib/aarch64-linux-gnu/crt1.o "
        "--no-as-needed /lib/aarch64-linux-gnu/crtn.o";
    template_suffix = " -lc -lm";
    break;
  case COMPILE_TARGET_LINUX_X86_64:
    template_prefix =
        "ld -dynamic-linker /lib64/ld-linux-x86-64.so.2 "
        "-L/usr/lib/gcc/x86_64-linux-gnu/12 "
        "-L/lib64/x86-64-linux-gnu -L/usr/lib/x86_64-linux-gnu -L/lib "
        "-L/usr/lib --no-as-needed /usr/lib/x86_64-linux-gnu/crt1.o "
        "--no-as-needed /usr/lib/x86_64-linux-gnu/crtn.o";
    template_suffix = " -lc -lm";
    break;
  case COMPILE_TARGET_LINUX_RV32I:
    // FIXME: this will only work on mac
    // need a way to get the riscv paths xplat

    template_prefix =
        "riscv64-unknown-elf-ld "
        "-plugin "
        "/opt/riscv/bin/../libexec/gcc/riscv64-unknown-elf/12.2.0/"
        "liblto_plugin.so "
        "-plugin-opt=/opt/riscv/bin/../libexec/gcc/riscv64-unknown-elf/12.2.0/"
        "lto-wrapper -plugin-opt=-fresolution=/tmp/ccI4xWdp.res "
        "-plugin-opt=-pass-through=-lgcc -plugin-opt=-pass-through=-lc "
        "-plugin-opt=-pass-through=-lgloss -plugin-opt=-pass-through=-lgcc "
        "--sysroot=/opt/riscv/bin/../riscv64-unknown-elf -melf32lriscv "
        "/opt/riscv/bin/../lib/gcc/riscv64-unknown-elf/12.2.0/../../../../"
        "riscv64-unknown-elf/lib/rv32imfd/ilp32d/crt0.o "
        "/opt/riscv/bin/../lib/gcc/riscv64-unknown-elf/12.2.0/rv32imfd/ilp32d/"
        "crtbegin.o "
        "-L/opt/riscv/bin/../lib/gcc/riscv64-unknown-elf/12.2.0/rv32imfd/"
        "ilp32d "
        "-L/opt/riscv/bin/../lib/gcc/riscv64-unknown-elf/12.2.0/../../../../"
        "riscv64-unknown-elf/lib/rv32imfd/ilp32d "
        "-L/opt/riscv/bin/../riscv64-unknown-elf/lib/rv32imfd/ilp32d "
        "-L/opt/riscv/bin/../lib/gcc/riscv64-unknown-elf/12.2.0 "
        "-L/opt/riscv/bin/../lib/gcc "
        "-L/opt/riscv/bin/../lib/gcc/riscv64-unknown-elf/12.2.0/../../../../"
        "riscv64-unknown-elf/lib -L/opt/riscv/bin/../riscv64-unknown-elf/lib ";

    template_suffix =
        " -lgcc --start-group -lc -lm -lgloss --end-group -lgcc "
        "/opt/riscv/bin/../lib/gcc/riscv64-unknown-elf/12.2.0/rv32imfd/ilp32d/"
        "crtend.o";
    // template_prefix = "riscv64-unknown-elf-ld "
    //                   "-melf32lriscv "
    //                   "--sysroot=/opt/riscv/bin/../riscv64-unknown-elf "
    //                   "-plugin "
    //                   "/opt/riscv/bin/../libexec/gcc/riscv64-unknown-elf/"
    //                   "12.2.0/liblto_plugin.so "
    //                   "-plugin-opt=/opt/riscv/bin/../libexec/gcc/"
    //                   "riscv64-unknown-elf/12.2.0/lto-wrapper "
    //                   "/opt/riscv/bin/../lib/gcc/riscv64-unknown-elf/12.2.0/../"
    //                   "../../../riscv64-unknown-elf/lib/crt0.o "
    //                   "-L/opt/riscv/bin/../lib/gcc/riscv64-unknown-elf/12.2.0
    //                   "
    //                   "-L/opt/riscv/bin/../lib/gcc "
    //                   "-L/opt/riscv/bin/../lib/gcc/riscv64-unknown-elf/12.2.0/"
    //                   "../../../../riscv64-unknown-elf/lib "
    //                   "-L/opt/riscv/bin/../riscv64-unknown-elf/lib";

    // template_suffix = " -lgcc "
    //                   "--start-group "
    //                   "-lc "
    //                   "-lgloss "
    //                   " --end-group ";

    break;
  default:
    unsupported("bad target for linux linker");
  }

  size_t template_size = strlen(template_prefix);
  size_t total_size = template_size;
  total_size++; // for space/null
  for (size_t i = 0; i < args->num_objects; i++) {
    total_size += 5; // separator space + quotes
    total_size += strlen(args->objects[i]);
  }
  total_size +=
      3 + 2 +
      strlen(
          args->output); // for "-o " and output filename surrounded by quotes

  total_size += strlen(template_suffix);

  const char *stderr_null = " 2>/dev/null";

  if (!args->args->verbose) {
    total_size += strlen(stderr_null);
  }

  total_size++; // null terminator

  char *buff = arena_alloc(arena, total_size);

  size_t head = 0;
  strcpy(&buff[head], template_prefix);
  head += template_size;
  buff[head++] = ' ';

  for (size_t i = 0; i < args->num_objects; i++) {
    if (args->objects[i][0] != '/' && args->objects[i][0] != '.') {
      buff[head++] = '.';
      buff[head++] = '/';
    } else {
      buff[head++] = ' ';
      buff[head++] = ' ';
    }
    buff[head++] = '\'';
    strcpy(&buff[head], args->objects[i]);
    head += strlen(args->objects[i]);
    buff[head++] = '\'';
    buff[head++] = ' ';
  }

  strcpy(&buff[head], "-o '");
  head += 4;
  strcpy(&buff[head], args->output);
  head += strlen(args->output);
  buff[head++] = '\'';

  strcpy(&buff[head], template_suffix);
  head += strlen(template_suffix);

  if (!args->args->verbose) {
    strcpy(&buff[head], stderr_null);
    head += strlen(stderr_null);
  }

  buff[head++] = 0;

  DEBUG_ASSERT(head == total_size, "string buffer calculations went wrong!");

  if (args->args->verbose) {
    fprintf(stderr, "Linux link:\n%s\n", buff);
  }

  int ret_code = system(buff);

  arena_allocator_free(&arena);

  return ret_code == 0 ? LINK_RESULT_SUCCESS : LINK_RESULT_FAILURE;
}
