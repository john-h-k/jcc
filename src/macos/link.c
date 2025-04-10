#include "link.h"

#include "../alloc.h"
#include "../compiler.h"
#include "../syscmd.h"

#include <stdlib.h>
#include <string.h>

static enum link_result
macos_link_objects_with_ld(const struct link_args *args) {
  struct arena_allocator *arena;
  arena_allocator_create("macos_link", &arena);

  const char *sdk_path = args->args->sys_root;

  // FIXME: support non `ld_classic` - requires arch and platform_version
  // -arch arm64 -platform_version macos 14.0.0 14.4"
  struct syscmd *cmd = syscmd_create(arena, "ld");
  syscmd_add_arg(cmd, "-lSystem");
  syscmd_add_arg(cmd, "-lc");
  syscmd_add_arg(cmd, "-dynamic");
  syscmd_add_arg_val(cmd, "-syslibroot", sdk_path);
  syscmd_add_arg(cmd, "-ld_classic");

  syscmd_add_arg_val(cmd, "-o", args->output);

  for (size_t i = 0; i < args->num_objects; i++) {
    syscmd_add_arg(cmd, args->objects[i]);
  }

  for (size_t i = 0; i < args->num_linker_args; i++) {
    syscmd_add_arg(cmd, args->linker_args[i]);
  }

  if (args->args->verbose) {
    fprintf(stderr, "link command (platform=macos):\n");
    syscmd_write_cmd(cmd, stderr);
  } else {
    syscmd_set_stdout_path(cmd, SYSCMD_BUF_FLAG_NONE, "/dev/null");
  }

  int ret_code = syscmd_exec(&cmd);

  arena_allocator_free(&arena);

  return ret_code == 0 ? LINK_RESULT_SUCCESS : LINK_RESULT_FAILURE;
}

enum link_result macos_link_objects(const struct link_args *args) {
  return macos_link_objects_with_ld(args);
}
