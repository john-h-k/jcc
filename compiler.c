#include "compiler.h"
#include "alloc.h"
#include "parse.h"
#include "util.h"
#include "log.h"
#include "lex.h"
#include "parse.h"
#include "ir.h"
#include "vector.h"
#include "aarch64/lower.h"
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

#define AARCH64_FUNCTION_ALIGNMENT (16)

enum compile_result compile(struct compiler* compiler) {
  BEGIN_STAGE("LEX + PARSE");

  struct parse_result result = parse(compiler->parser);

  BEGIN_STAGE("AST");

  debug_print_ast(compiler->parser, &result.translation_unit);

  struct vector *lower_results = vector_create(sizeof(struct lower_result));
  struct vector *symbols = vector_create(sizeof(struct symbol));
  size_t total_size = 0;
  
  for (size_t i = 0; i < result.translation_unit.num_func_defs; i++) {
    BEGIN_STAGE("IR BUILD");

    struct ir_function ir = build_ir_for_function(compiler->parser, compiler->arena, &result.translation_unit.func_defs[0]);

    BEGIN_STAGE("IR");

    debug_print_ir(ir.start);

    BEGIN_STAGE("LOWERING");

    struct lower_result result = lower(compiler->arena, &ir);

    struct symbol symbol = {
      .name = result.name,
      .section = 1,
      .value = total_size
    };

    total_size += ROUND_UP(result.len_code, AARCH64_FUNCTION_ALIGNMENT);
    vector_push_back(lower_results, &result);

    vector_push_back(symbols, &symbol);
  }
  
  char *code = alloc(compiler->arena, total_size);
  char *head = code;

  size_t num_results = vector_length(lower_results);
  for (size_t i = 0; i < num_results; i++) {
    struct lower_result *result = vector_get(lower_results, i);
    memcpy(head, result->code, result->len_code);
    head += ROUND_UP(result->len_code, AARCH64_FUNCTION_ALIGNMENT);
  }

  struct macho_args args = {
    .compile_args = &compiler->args,
    .output = compiler->output,
    .data = code,
    .len_data = total_size,
    .symbols = vector_get(symbols, 0),
    .num_symbols = vector_length(symbols),  
  };

  BEGIN_STAGE("OBJECT FILE");

  write_macho(&args);

  vector_free(&lower_results);
  vector_free(&symbols);

  return COMPILE_RESULT_SUCCESS;
}

void free_compiler(struct compiler **compiler) {
  free_arena_allocator(&(*compiler)->arena);
  free_parser(&(*compiler)->parser);

  free(*compiler);
  *compiler = NULL;
}
