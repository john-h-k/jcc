#ifndef MACOS_MACH_O_H
#define MACOS_MACH_O_H

#include "../target.h"

#include <fcntl.h>
#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

void write_macho(const struct build_object_args *args);

#endif
