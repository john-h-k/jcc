#include "link.h"

#include "alloc.h"
#include "log.h"
#include "util.h"

#include <stdlib.h>
#include <string.h>

enum link_result link_objects(const struct link_args *args) {
  struct arena_allocator *arena;
  create_arena_allocator(&arena);

  // FIXME: support non `ld_classic`
  const char *template = "ld -lSystem -syslibroot $(xcrun -sdk macosx "
                         "--show-sdk-path) -ld_classic";

  // super inefficient here

  size_t template_size = strlen(template);
  size_t total_size = template_size;
  total_size++;
  for (size_t i = 0; i < args->num_objects; i++) {
    // seperator
    total_size++;
    total_size += strlen(args->objects[i]);
  }

  total_size += /* "-o " */ 3 + strlen(args->output);
  total_size++; // null terminator

  char *buff = arena_alloc(arena, total_size);

  size_t head = 0;
  strcpy(&buff[head], template);
  head += template_size;

  buff[head++] = ' ';

  for (size_t i = 0; i < args->num_objects; i++) {
    strcpy(&buff[head], args->objects[i]);
    head += strlen(args->objects[i]);
    buff[head++] = ' ';
  }

  strcpy(&buff[head], "-o ");
  head += 3;
  strcpy(&buff[head], args->output);
  head += strlen(args->output);
  buff[head++] = '\0';

  debug_assert(head == total_size, "string buffer calculations went wrong!");

  int ret_code = system(buff);

  if (!ret_code) {
    return LINK_RESULT_SUCCESS;
  } else {
    return LINK_RESULT_FAILURE;
  }
}
