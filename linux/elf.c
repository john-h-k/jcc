#include "elf.h"

#include "../compiler.h"
#include "../log.h"
#include "../util.h"
#include "../vector.h"

#if __has_include(<elf.h>)
#include <elf.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#error "TODO: Implement ELF"

#else

void write_elf(const struct build_object_args *args) {
  unsupported("ELF not supported target for this system");
}

#endif
