#include "compiler.h"

#include "aarch64.h"
#include "eep.h"

#include "emit.h"
#include "alloc.h"
#include "ir/build.h"
#include "ir/eliminate_phi.h"
#include "ir/prettyprint.h"
#include "lex.h"
#include "parse.h"
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

  if (parser_create(program, &(*compiler)->parser) !=
      PARSER_CREATE_RESULT_SUCCESS) {
    err("failed to create parser");
    return COMPILER_CREATE_RESULT_FAILURE;
  }

  arena_allocator_create(&(*compiler)->arena);

  size_t sz = strlen(output) + 1;
  (*compiler)->output = arena_alloc((*compiler)->arena, sz);
  strcpy((*compiler)->output, output);
  (*compiler)->output[sz] = '\0';

  return COMPILER_CREATE_RESULT_SUCCESS;
}

void debug_print_stage(struct ir_builder *irb, const char *name) {
  debug_print_ir(stderr, irb, NULL, NULL);

  char *buff = nonnull_malloc(strlen(name) + sizeof(".gv"));
  strcpy(buff, name);
  strcat(buff, ".gv");
  
  FILE *ir_graph = fopen(buff, "w");
  debug_print_ir_graph(ir_graph, irb);
  fclose(ir_graph);
}

const struct target *get_target(const struct compile_args *args) {
  switch (args->target_arch) {
  case COMPILE_TARGET_ARCH_NATIVE:
    bug("hit COMPILE_TARGET_ARCH_NATIVE in compiler! should have been chosen earlier");
    break;
  case COMPILE_TARGET_ARCH_MACOS_X86_64:
    todo("macOS x64 target not yet implemented");
  case COMPILE_TARGET_ARCH_MACOS_ARM64:
    return &AARCH64_TARGET;
  case COMPILE_TARGET_ARCH_EEP:
    return &EEP_TARGET;
  }

  bug("unexpected target in `get_target`");
}

enum compile_result compile(struct compiler *compiler) {
  if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_PARSE)) {
    enable_log();
  }

  BEGIN_STAGE("LEX + PARSE");

  struct parse_result result = parse(compiler->parser);

  BEGIN_STAGE("AST");

  if (compiler->args.log_flags & COMPILE_LOG_FLAGS_PARSE) {
    debug_print_ast(compiler->parser, &result.translation_unit);
  }

  disable_log();

  struct vector *compiled_functions =
      vector_create(sizeof(struct compiled_function));
  struct vector *symbols = vector_create(sizeof(struct symbol));
  size_t total_size = 0;

  const struct target *target = get_target(&compiler->args);

  if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_IR)) {
    enable_log();
  }

  BEGIN_STAGE("IR BUILD");

  struct ir_unit *ir = build_ir_for_translationunit(compiler->parser, compiler->arena, &result.translation_unit);
  
  for (size_t i = 0; i < ir->num_funcs; i++) {
    struct ir_builder *irb = ir->funcs[i];

    {
      BEGIN_STAGE("IR");

      if (compiler->args.log_flags & COMPILE_LOG_FLAGS_IR) {
        debug_print_stage(irb, "ir");
      }

      BEGIN_STAGE("LOWERING");

      target->lower(irb);

      BEGIN_STAGE("POST-LOWER IR");

      if (compiler->args.log_flags & COMPILE_LOG_FLAGS_IR) {
        debug_print_stage(irb, "lower");
      }

      disable_log();
    }

    {
      if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_REGALLOC)) {
        enable_log();
      }

      BEGIN_STAGE("REGALLOC");

      register_alloc(irb, target->reg_info);

      if (compiler->args.log_flags & COMPILE_LOG_FLAGS_REGALLOC) {
        debug_print_stage(irb, "reg_alloc");
      }

      BEGIN_STAGE("ELIM PHI");
      eliminate_phi(irb);

      if (compiler->args.log_flags & COMPILE_LOG_FLAGS_REGALLOC) {
        debug_print_stage(irb, "elim_phi");
      }
      

      disable_log();
    }

    struct compiled_function func;
    {
      if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_EMIT)) {
        enable_log();
      }

      BEGIN_STAGE("EMITTING");

      func = target->emit_function(irb);

      disable_log();
    }

    struct symbol symbol = {
        .name = func.name, .section = 1, .value = total_size};

    total_size += ROUND_UP(func.len_code, target->function_alignment);
    vector_push_back(compiled_functions, &func);

    vector_push_back(symbols, &symbol);
  }

  char *code = arena_alloc(compiler->arena, total_size);
  char *head = code;

  size_t num_results = vector_length(compiled_functions);
  for (size_t i = 0; i < num_results; i++) {
    struct compiled_function *func = vector_get(compiled_functions, i);
    memcpy(head, func->code, func->len_code);
    head += ROUND_UP(func->len_code, target->function_alignment);
  }

  struct build_object_args args = {
      .compile_args = &compiler->args,
      .output = compiler->output,
      .data = code,
      .len_data = total_size,
      .symbols = vector_get(symbols, 0),
      .num_symbols = vector_length(symbols),
  };

  BEGIN_STAGE("OBJECT FILE");

  target->build_object(&args);

  vector_free(&compiled_functions);
  vector_free(&symbols);

  if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_ASM)) {
    enable_log();

    if (!target->debug_disasm) {
      warn("DISASM not supported for current target");
    } else {
      BEGIN_STAGE("DISASSEMBLY");
      target->debug_disasm(args.output);
    }
    disable_log();
  }

  return COMPILE_RESULT_SUCCESS;
}

void free_compiler(struct compiler **compiler) {
  arena_allocator_free(&(*compiler)->arena);
  parser_free(&(*compiler)->parser);

  free(*compiler);
  *compiler = NULL;
}
