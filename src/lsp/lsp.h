#ifndef LSP_LSP_H
#define LSP_LSP_H

#include "../alloc.h"
#include "../args.h"
#include "../compiler.h"
#include "../fs.h"

int lsp_run(struct arena_allocator *arena, struct fs *fs,
            struct parsed_args args, struct compile_args compile_args,
            const struct target *target);

#endif
