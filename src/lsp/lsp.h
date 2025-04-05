#ifndef LSP_LSP_H
#define LSP_LSP_H

#include "../alloc.h"
#include "../args.h"
#include "../compiler.h"
#include "../fcache.h"

int lsp_run(struct arena_allocator *arena, struct fcache *fcache,
            struct parsed_args args, struct compile_args compile_args,
            const struct target *target);

#endif
