#ifndef DISASM_H
#define DISASM_H

#include "compiler.h"

// dumps using `objdump -d `
void objdump_debug_disasm(enum compile_target target, const char *filename, const char *output);

#endif
