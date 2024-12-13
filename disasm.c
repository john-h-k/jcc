#include "disasm.h"

#include "log.h"
#include "util.h"

#include <stdlib.h>

void objdump_debug_disasm(const char *filename, const char *output) {
  // static const char COMMAND[] = "objdump --macho --no-show-raw-insn -d ";
  // macho mode causes some instructions to show wrongly (half instructions)
  static const char COMMAND[] = "objdump --no-show-raw-insn -d ";

  char *command = nonnull_malloc(sizeof COMMAND + strlen(filename) + strlen(" > ") + strlen(output) + 1);
  strcpy(command, COMMAND);
  strcat(command, filename);

  if (output) {
    strcat(command, " > ");
    strcat(command, output);
  }

  if (system(command)) {
    warn("`debug_disasm` failed");
  }
}
