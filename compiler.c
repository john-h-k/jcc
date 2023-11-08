#include "compiler.h"
#include "alloc.h"
#include "parse.h"
#include "util.h"
#include "log.h"
#include "lex.h"
#include "parse.h"
#include "ir.h"
#include "arm64_lower.h"
#include "macos/mach-o.h"
#include <stdio.h>

struct compiler {
  struct arena_allocator* arena;

  struct compile_args args;
  struct parser* parser;

  char *output;
};

enum compiler_create_result create_compiler(const char *program, const char *output, const struct compile_args *args, struct compiler **compiler) {
  *compiler = nonnull_malloc(sizeof(**compiler));

  (*compiler)->args = *args;

  if (create_parser(program, &(*compiler)->parser) != PARSER_CREATE_RESULT_SUCCESS) {
    err("failed to create parser");
    return COMPILER_CREATE_RESULT_FAILURE;
  }

  create_arena_allocator(&(*compiler)->arena);

  (*compiler)->output = alloc((*compiler)->arena, strlen(output) + 1);
  strcpy((*compiler)->output, output);

  return COMPILER_CREATE_RESULT_SUCCESS;
}

enum compile_result compile(struct compiler* compiler) {
  struct parse_result result = parse(compiler->parser);

  // for (size_t i = 0; i < result.translation_unit.num_func_defs; i++) {
  if (result.translation_unit.num_func_defs > 1) {
    todo("multiple funcs");
  }

  struct ir_function ir = build_ir_for_function(compiler->arena, &result.translation_unit.func_defs[0]);

  debug_print_ir(ir.start);
  struct lower_result r = lower(compiler->arena, &ir);

  struct symbol symbol = {
    .name = "_main",
    .section = 1,
    .value = 0
  };

  struct macho_args args = {
    .compile_args = &compiler->args,
    .output = compiler->output,
    .data = r.code,
    .len_data = r.len_code,
    .symbols = &symbol,
    .num_symbols = 1,  
  };

  write_macho(&args);

  return COMPILE_RESULT_SUCCESS;
}

void free_compiler(struct compiler **compiler) {
  free_arena_allocator(&(*compiler)->arena);
  free_parser(&(*compiler)->parser);

  free(*compiler);
  *compiler = NULL;
}
