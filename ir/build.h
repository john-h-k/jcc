#ifndef IR_BUILD_H
#define IR_BUILD_H

#include "ir.h"
#include "../parse.h"

struct ir_unit *build_ir_for_translationunit(
    /* needed for `associated_text */ struct parser *parser,
    struct arena_allocator *arena,
    struct ast_translationunit *translation_unit);

#endif
