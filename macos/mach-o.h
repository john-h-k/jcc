#ifndef MACOS_MACH_O_H
#define MACOS_MACH_O_H

#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../compiler.h"

struct symbol {
  const char *name;
  size_t section;
  size_t value;
};

struct macho_args {
  const struct compile_args* compile_args;

  const char *output;

  const char *data;
  size_t len_data;
  
  struct symbol* symbols;
  size_t num_symbols;
};

void write_macho(const struct macho_args* args);

#endif
