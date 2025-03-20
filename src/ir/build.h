#ifndef IR_BUILD_H
#define IR_BUILD_H

#include "../parse.h"
#include "../typechk.h"
#include "ir.h"

enum ir_build_flags {
  IR_BUILD_FLAG_NONE = 0,

  // for debugging. does not generate phis
  IR_BUILD_FLAG_SPILL_ALL = 1 << 0,
};

struct ir_unit *build_ir_for_translationunit(
    const struct target *target, struct typechk *typechk,
    struct arena_allocator *arena, struct td_translationunit *translation_unit,
    enum ir_build_flags flags);

#endif
