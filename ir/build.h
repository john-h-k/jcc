#ifndef IR_BUILD_H
#define IR_BUILD_H

#include "ir.h"
#include "../parse.h"
#include "../typechk.h"

struct ir_unit *build_ir_for_translationunit(
    struct typechk *typechk,
    struct arena_allocator *arena,
    struct td_translationunit *translation_unit);

#endif
