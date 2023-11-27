#include "compiler.h"

#include "aarch64/emit.h"
#include "aarch64/lower.h"
#include "alloc.h"
#include "disasm.h"
#include "ir/build.h"
#include "ir/eliminate_phi.h"
#include "ir/prettyprint.h"
#include "lex.h"
#include "log.h"
#include "lsra.h"
#include "macos/mach-o.h"
#include "parse.h"
#include "util.h"
#include "vector.h"

#include <stdio.h>

struct compiler {
  struct arena_allocator *arena;

  struct compile_args args;
  struct parser *parser;

  char *output;
};

enum compiler_create_result create_compiler(const char *program,
                                            const char *output,
                                            const struct compile_args *args,
                                            struct compiler **compiler) {
  *compiler = nonnull_malloc(sizeof(**compiler));

  (*compiler)->args = *args;

  if (create_parser(program, &(*compiler)->parser) !=
      PARSER_CREATE_RESULT_SUCCESS) {
    err("failed to create parser");
    return COMPILER_CREATE_RESULT_FAILURE;
  }

  create_arena_allocator(&(*compiler)->arena);

  size_t sz = strlen(output) + 1;
  (*compiler)->output = arena_alloc((*compiler)->arena, sz);
  strcpy((*compiler)->output, output);
  (*compiler)->output[sz] = '\0';


  return COMPILER_CREATE_RESULT_SUCCESS;
}

#define AARCH64_FUNCTION_ALIGNMENT (16)

enum compile_result compile(struct compiler *compiler) {
  if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_PARSE)) {
    enable_log();
  }
  
  BEGIN_STAGE("LEX + PARSE");

  struct parse_result result = parse(compiler->parser);

  BEGIN_STAGE("AST");

  debug_print_ast(compiler->parser, &result.translation_unit);

  disable_log();

  struct vector *compiled_functions =
      vector_create(sizeof(struct compiled_function));
  struct vector *symbols = vector_create(sizeof(struct symbol));
  size_t total_size = 0;

  for (size_t i = 0; i < result.translation_unit.num_func_defs; i++) {
    struct ast_funcdef *def = &result.translation_unit.func_defs[i];

    struct ir_builder *ir;
    {
      if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_IR)) {
        enable_log();
      }

      BEGIN_STAGE("IR BUILD");

      info("compiling function %s",
           identifier_str(compiler->parser, &def->sig.name));

      ir = build_ir_for_function(compiler->parser, compiler->arena, def);

      FILE *ir_graph = fopen("cfg.gv", "w");
      debug_print_ir_graph(ir_graph, ir);
      fclose(ir_graph);

      BEGIN_STAGE("IR");

      debug_print_ir(stderr, ir, NULL, NULL);

      BEGIN_STAGE("LOWERING");

      lower(ir);

      BEGIN_STAGE("POST-LOWER IR");

      debug_print_ir(stderr, ir, NULL, NULL);

      disable_log();
    }

    {
      if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_REGALLOC)) {
        enable_log();
      }

      BEGIN_STAGE("REGALLOC");

      BEGIN_STAGE("ELIM PHI");
      eliminate_phi(ir);

      struct reg_info aarch64_reg_info = {.num_regs = 18};
      register_alloc(ir, aarch64_reg_info);

      // rebuild_ids(ir);

      debug_print_ir(stderr, ir, print_ir_intervals, NULL);

      disable_log();
    }

    struct compiled_function func;
    {
      if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_EMIT)) {
        enable_log();
      }

      BEGIN_STAGE("EMITTING");
      func = emit(ir);

      disable_log();
    }

    struct symbol symbol = {
        .name = func.name, .section = 1, .value = total_size};

    total_size += ROUND_UP(func.len_code, AARCH64_FUNCTION_ALIGNMENT);
    vector_push_back(compiled_functions, &func);

    vector_push_back(symbols, &symbol);
  }

  char *code = arena_alloc(compiler->arena, total_size);
  char *head = code;

  size_t num_results = vector_length(compiled_functions);
  for (size_t i = 0; i < num_results; i++) {
    struct compiled_function *func = vector_get(compiled_functions, i);
    memcpy(head, func->code, func->len_code);
    head += ROUND_UP(func->len_code, AARCH64_FUNCTION_ALIGNMENT);
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

  vector_free(&compiled_functions);
  vector_free(&symbols);

  if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_ASM)) {
    enable_log();

    BEGIN_STAGE("DISASSEMBLY");
    debug_disasm(args.output);

    disable_log();
  }

  return COMPILE_RESULT_SUCCESS;
}

void free_compiler(struct compiler **compiler) {
  free_arena_allocator(&(*compiler)->arena);
  free_parser(&(*compiler)->parser);

  free(*compiler);
  *compiler = NULL;
}
