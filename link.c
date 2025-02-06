#include "link.h"

#include "alloc.h"
#include "util.h"

#include <stdlib.h>
#include <string.h>

#ifdef MACOS
enum link_result link_objects(const struct link_args *args) {
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

  DEBUG_ASSERT(head == total_size, "string buffer calculations went wrong!");

  int ret_code = system(buff);

  arena_allocator_free(&arena);

  if (!ret_code) {
    return LINK_RESULT_SUCCESS;
  } else {
    return LINK_RESULT_FAILURE;
  }
}
#else

#include "link.h"
#include "alloc.h"
#include "util.h"

#include <stdlib.h>
#include <string.h>

enum link_result link_objects(const struct link_args *args) {
    struct arena_allocator *arena;
    arena_allocator_create(&arena);

    // using ld for elf linking (linux x86_64, adjust as needed)
    // ld -o output_binary your_object.o 

    
    const char *template = "ld -m aarch64linux -dynamic-linker /lib/ld-linux-aarch64.so.1 /lib/aarch64-linux-gnu/Scrt1.o /lib/aarch64-linux-gnu/crti.o /usr/bin/../lib/gcc/aarch64-linux-gnu/12/crtbeginS.o -L/usr/bin/../lib/gcc/aarch64-linux-gnu/12 -L/lib/aarch64-linux-gnu -L/usr/lib/aarch64-linux-gnu -L/lib -L/usr/lib -lgcc --as-needed -lgcc_s --no-as-needed -lgcc --as-needed -lgcc_s --no-as-needed /usr/bin/../lib/gcc/aarch64-linux-gnu/12/crtendS.o /lib/aarch64-linux-gnu/crtn.o";
  

    size_t template_size = strlen(template);
    size_t total_size = template_size;
    total_size++; // for space/null
    for (size_t i = 0; i < args->num_objects; i++) {
        total_size++; // separator space
        total_size += strlen(args->objects[i]);
    }
    total_size += 3 + strlen(args->output); // for "-o " and output filename
    total_size++; // null terminator

    char *buff = arena_alloc(arena, total_size + 4);

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

    strcpy(&buff[head], " -lc");
    head += strlen(" -lc");
    buff[head++] = '\0';

    // DEBUG_ASSERT(head == total_size, "string buffer calculations went wrong!");

    int ret_code = system(buff);

    arena_allocator_free(&arena);

    return ret_code == 0 ? LINK_RESULT_SUCCESS : LINK_RESULT_FAILURE;
}


#endif
