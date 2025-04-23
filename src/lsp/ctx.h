#ifndef LSP_CTX_H
#define LSP_CTX_H

#include <stdbool.h>
#include <stdio.h>
#include "../compiler.h"
#include "../args.h"
#include "../vector.h"

#include "lsp_types.h"

struct lsp_ctx {
  struct arena_allocator *arena;
  struct vector *read_buf;
  struct json_writer *writer;

  // hmm, ideally we would use fs for all the file caching stuff but it
  // doesn't currently have the capabilities for modifying docs
  struct fs *fs;

  // sized_str (uri) : lsp_doc
  struct hashtbl *docs;

  struct parsed_args args;
  struct compile_args compile_args;
  const struct target *target;

  struct init_params init_params;

  bool shutdown_recv;

  FILE *in;
  FILE *out;
  FILE *log;
};

struct lsp_doc {
  struct text_doc doc;
  struct compiler *compiler;
};

#endif
