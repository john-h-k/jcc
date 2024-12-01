#ifndef RISCV_H
#define RISCV_H

#include "rv32i/disasm.h"
#include "rv32i/emit.h"
#include "rv32i/lower.h"
#include "rv32i/object.h"
#include "target.h"

#define RISCV_FUNCTION_ALIGNMENT (8)

extern const struct target RISCV_TARGET;

#endif
