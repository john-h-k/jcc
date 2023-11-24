#include "disasm.h"

#include "log.h"
#include "util.h"

#include <stdlib.h>

void debug_disasm(const char *filename) {
  const char COMMAND[] = "objdump -d ";
  char *command = nonnull_malloc(sizeof COMMAND + strlen(filename));
  strcpy(command, COMMAND);
  strcat(command, filename);
  if (system(command)) {
    warn("`debug_disasm` failed");
  }
}
