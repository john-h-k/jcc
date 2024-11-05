#include "disasm.h"

#include "log.h"
#include "util.h"

#include <stdlib.h>

void objdump_debug_disasm(const char *filename) {
  // const char COMMAND[] = "objdump --macho --no-show-raw-insn -d ";
  // macho mode causes some instructions to show wrongly (half instructions)
  const char COMMAND[] = "objdump --no-show-raw-insn -d ";
  char *command = nonnull_malloc(sizeof COMMAND + strlen(filename) + 1);
  strcpy(command, COMMAND);
  strcat(command, filename);
  if (system(command)) {
    warn("`debug_disasm` failed");
  }
}
