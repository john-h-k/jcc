#if 0

#include "object.h"

#include "isa.h"

void write_eep(const struct build_object_args *args) {
  FILE *file = fopen(args->output, "wb");
  if (file == NULL) {
    perror("fopen");
    exit(EXIT_FAILURE);
  }

  const unsigned short *const instr = (unsigned short *)args->data;
  size_t num_instrs = args->len_data / EEP_INSTR_SIZE;

  for (size_t i = 0; i < num_instrs; i++) {
    fprintf(file, "0x%02zx ", i);
    fprintf(file, "0x%04x", instr[i]);
    fprintf(file, "\n");
  }

  fclose(file);
}

#endif
