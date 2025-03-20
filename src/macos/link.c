#include "link.h"

#include "../alloc.h"
#include "../compiler.h"
#include "../util.h"

#include <stdlib.h>
#include <string.h>

static enum link_result
macos_link_objects_with_ld(const struct link_args *args) {
  struct arena_allocator *arena;
  arena_allocator_create(&arena);

  // FIXME: support non `ld_classic` - requires arch and platform_version
  // -arch arm64 -platform_version macos 14.0.0 14.4"
  const char *template =
      "ld -lSystem -lc -dynamic -syslibroot $(xcrun -sdk macosx "
      "--show-sdk-path) -ld_classic";

  // super inefficient here

  size_t template_size = strlen(template);
  size_t total_size = template_size;
  total_size++;
  for (size_t i = 0; i < args->num_objects; i++) {
    // seperator plus quotes
    total_size += 5;
    total_size += strlen(args->objects[i]);
  }

  total_size += /* " -o " */ 4 + 2 + strlen(args->output);
  total_size++; // null terminator

  char *buff = arena_alloc(arena, total_size);

  size_t head = 0;
  strcpy(&buff[head], template);
  head += template_size;

  strcpy(&buff[head], " -o '");
  head += 5;
  strcpy(&buff[head], args->output);
  head += strlen(args->output);
  buff[head++] = '\'';

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

  buff[head++] = '\0';

  DEBUG_ASSERT(head == total_size, "string buffer calculations went wrong!");

  if (args->args->verbose) {
    fprintf(stderr, "Linux link:\n%s\n", buff);
  }
  int ret_code = system(buff);

  arena_allocator_free(&arena);

  if (!ret_code) {
    return LINK_RESULT_SUCCESS;
  } else {
    return LINK_RESULT_FAILURE;
  }
}

enum link_result macos_link_objects(const struct link_args *args) {
  return macos_link_objects_with_ld(args);
}
