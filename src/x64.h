#ifndef X64_H
#define X64_H

#include "target.h"

#define X64_FUNCTION_ALIGNMENT (4)
#define X64_STACK_ALIGNMENT (16)
#define MAX_IMM_SIZE (4095)

extern const struct target X64_MACOS_TARGET;
extern const struct target X64_LINUX_TARGET;

#endif
