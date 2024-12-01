#ifndef RISCV_H
#define RISCV_H

#include "riscv/disasm.h"
#include "riscv/emit.h"
#include "riscv/lower.h"
#include "riscv/object.h"
#include "target.h"

#define RISCV_FUNCTION_ALIGNMENT (8)

extern const struct target RISCV_TARGET;

#endif
