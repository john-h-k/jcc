#include "compiler.h"
#include "alloc.h"
#include "parse.h"
#include "util.h"
#include "log.h"
#include "lex.h"
#include "parse.h"
#include "macos/mach-o.h"
#include <stdio.h>

struct compiler {
  struct arena_allocator* arena;

  struct compile_args args;
  struct parser* parser;
};



enum compiler_create_result create_compiler(const char *program, const struct compile_args *args, struct compiler **compiler) {
  *compiler = nonnull_malloc(sizeof(**compiler));

  (*compiler)->args = *args;

  if (create_parser(program, &(*compiler)->parser) != PARSER_CREATE_RESULT_SUCCESS) {
    err("failed to create parser");
    return COMPILER_CREATE_RESULT_FAILURE;
  }

  create_arena_allocator(&(*compiler)->arena);

  return COMPILER_CREATE_RESULT_SUCCESS;
}

enum compile_result compile(struct compiler* compiler) {
  struct parse_result result = parse(compiler->parser);
  (void)result;

  // write_macho(&args, get_object_path(source_path), object->ptr, object->len);

  return COMPILE_RESULT_SUCCESS;
}

void free_compiler(struct compiler **compiler) {
  free_arena_allocator(&(*compiler)->arena);
  free_parser(&(*compiler)->parser);

  free(*compiler);
  *compiler = NULL;
}
