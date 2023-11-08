#ifndef __MACOS_MACH_O_H__
#define __MACOS_MACH_O_H__

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

void write_macho(const struct compile_args *args, const char *filename, const char *machine_code, size_t code_size);

#endif
