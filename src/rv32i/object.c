#include "object.h"

#include "../util.h"

void rv32i_write_object(const struct build_object_args *args) {
  FILE *file = args->output;

  if (file == NULL) {
    perror("fopen");
    exit(EXIT_FAILURE);
  }

  for (size_t i = 0; i < args->num_entries; i++) {
    const struct object_entry *entry = &args->entries[i];

    if (entry->ty != OBJECT_ENTRY_TY_FUNC) {
      TODO("non funcs in rv32i");
    }

    const unsigned char *const data = (const unsigned char *)entry->data;

    // fwrite(data, 1, entry->len_data, file);
    for (size_t j = 0; j < entry->len_data; j++) {
      fprintf(file, "%02x ", (unsigned)data[j]);

      if (j && j % 32 == 0) {
        fprintf(file, "\n");
      }
    }
  }

  fclose(file);
}
