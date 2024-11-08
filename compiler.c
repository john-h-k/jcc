#include "compiler.h"

#include "aarch64.h"
#include "alloc.h"
#include "codegen.h"
#include "eep.h"
#include "emit.h"
#include "ir/build.h"
#include "ir/eliminate_phi.h"
#include "ir/ir.h"
#include "ir/prettyprint.h"
#include "lex.h"
#include "log.h"
#include "lower.h"
#include "lsra.h"
#include "macos/mach-o.h"
#include "parse.h"
#include "preproc.h"
#include "target.h"
#include "util.h"
#include "vector.h"

#include <stdio.h>

struct compiler {
  struct arena_allocator *arena;

  struct compile_args args;
  struct parser *parser;
  struct typechk *typechk;

  char *output;
};

enum compiler_create_result create_compiler(struct program *program,
                                            const char *output,
                                            const struct compile_args *args,
                                            struct compiler **compiler) {
  *compiler = nonnull_malloc(sizeof(**compiler));

  (*compiler)->args = *args;

  // preproc is kept local as it is seperate to other stages
  struct preproc *preproc;

  if (preproc_create(program, args->num_include_paths, args->include_paths,
                     &preproc) != PREPROC_CREATE_RESULT_SUCCESS) {
    err("failed to create preproc");
    return COMPILER_CREATE_RESULT_FAILURE;
  }

  BEGIN_STAGE("PREPROC");

  if (args->log_flags & COMPILE_LOG_FLAGS_PREPROC) {
    enable_log();
  }

  struct preprocessed_program preprocessed_program = preproc_process(preproc);

  slog("%s\n", preprocessed_program.text);

  disable_log();

  if (parser_create(&preprocessed_program, &(*compiler)->parser) !=
      PARSER_CREATE_RESULT_SUCCESS) {
    err("failed to create parser");
    return COMPILER_CREATE_RESULT_FAILURE;
  }

  if (typechk_create((*compiler)->parser, &(*compiler)->typechk) !=
      TYPECHK_CREATE_RESULT_SUCCESS) {
    err("failed to create typechk");
    return COMPILER_CREATE_RESULT_FAILURE;
  }

  arena_allocator_create(&(*compiler)->arena);

  size_t sz = strlen(output) + 1;
  (*compiler)->output = arena_alloc((*compiler)->arena, sz);
  strcpy((*compiler)->output, output);
  (*compiler)->output[sz] = '\0';

  return COMPILER_CREATE_RESULT_SUCCESS;
}

static void debug_print_stage(struct ir_unit *ir, UNUSED_ARG(const char *name)) {
  debug_print_ir(stderr, ir, NULL, NULL);
}

static const struct target *get_target(const struct compile_args *args) {
  switch (args->target_arch) {
  case COMPILE_TARGET_ARCH_NATIVE:
    bug("hit COMPILE_TARGET_ARCH_NATIVE in compiler! should have been chosen "
        "earlier");
  case COMPILE_TARGET_ARCH_MACOS_X86_64:
    todo("macOS x64 target not yet implemented");
  case COMPILE_TARGET_ARCH_MACOS_ARM64:
    return &AARCH64_TARGET;
  case COMPILE_TARGET_ARCH_EEP:
    bug("redo eep");
    // return &EEP_TARGET;
  }

  bug("unexpected target in `get_target`");
}

enum compile_result compile(struct compiler *compiler) {
  if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_PARSE)) {
    enable_log();
  }

  BEGIN_STAGE("LEX + PARSE");

  struct parse_result parse_result = parse(compiler->parser);

  BEGIN_STAGE("AST");

  if (compiler->args.log_flags & COMPILE_LOG_FLAGS_PARSE) {
    debug_print_ast(compiler->parser, &parse_result.translation_unit);
  }

  disable_log();

  
  if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_TYPECHK)) {
    enable_log();
  }

  BEGIN_STAGE("TYPECHK");

  struct typechk_result typechk_result = td_typechk(compiler->typechk, &parse_result.translation_unit);

  if (compiler->args.log_flags & COMPILE_LOG_FLAGS_TYPECHK) {
    debug_print_td(compiler->typechk, &typechk_result.translation_unit);
  }

  disable_log();

  const struct target *target = get_target(&compiler->args);

  if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_IR)) {
    enable_log();
  }

  BEGIN_STAGE("IR BUILD");

  struct ir_unit *ir = build_ir_for_translationunit(
      compiler->typechk, compiler->arena, &typechk_result.translation_unit);

  {
    BEGIN_STAGE("IR");

    if (compiler->args.log_flags & COMPILE_LOG_FLAGS_IR) {
      debug_print_stage(ir, "ir");
    }

    BEGIN_STAGE("LOWERING");

    lower(ir, target);

    BEGIN_STAGE("POST PRE REG LOWER IR");

    if (compiler->args.log_flags & COMPILE_LOG_FLAGS_PRE_EMIT) {
      debug_print_stage(ir, "lower");
    }

    disable_log();
  }

  {
    if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_REGALLOC)) {
      enable_log();
    }

    BEGIN_STAGE("REGALLOC");

    struct ir_glb *glb = ir->first_global;

    while (glb) {
      if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
        glb = glb->succ;
        continue;
      }

      switch (glb->ty) {
      case IR_GLB_TY_DATA:
        break;
      case IR_GLB_TY_FUNC:
        lsra_register_alloc(glb->func, target->reg_info);
        break;
      }

      glb = glb->succ;
    }

    if (compiler->args.log_flags & COMPILE_LOG_FLAGS_REGALLOC) {
      debug_print_stage(ir, "regalloc");
    }

    BEGIN_STAGE("ELIM PHI");
    glb = ir->first_global;

    while (glb) {
      if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
        glb = glb->succ;
        continue;
      }

      switch (glb->ty) {
      case IR_GLB_TY_DATA:
        break;
      case IR_GLB_TY_FUNC:
        eliminate_phi(glb->func);
        break;
      }

      glb = glb->succ;
    }

    if (compiler->args.log_flags & COMPILE_LOG_FLAGS_REGALLOC) {
      debug_print_stage(ir, "elim_phi");
    }

    glb = ir->first_global;

    while (glb) {
      if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
        glb = glb->succ;
        continue;
      }

      switch (glb->ty) {
      case IR_GLB_TY_DATA:
        break;
      case IR_GLB_TY_FUNC:
        // eliminate_phi(glb->func);
        break;
      }

      glb = glb->succ;
    }

    if (compiler->args.log_flags & COMPILE_LOG_FLAGS_REGALLOC) {
      debug_print_stage(ir, "elim_phi");
    }
  }

  if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_EMIT)) {
    enable_log();
  }

  BEGIN_STAGE("PRE-EMIT");

  if (compiler->args.log_flags & COMPILE_LOG_FLAGS_EMIT) {
    debug_print_stage(ir, "pre_emit");
  }

  BEGIN_STAGE("CODEGEN");

  struct codegen_unit *codegen = target->codegen(ir);

  BEGIN_STAGE("EMITTING");

  if (compiler->args.log_flags & COMPILE_LOG_FLAGS_EMIT) {
    target->debug_print_codegen(stderr, codegen);
  }

  struct emitted_unit unit = target->emit_function(codegen);

  disable_log();

  struct build_object_args args = {.compile_args = &compiler->args,
                                   .output = compiler->output,
                                   .entries = unit.entries,
                                   .num_entries = unit.num_entries};

  BEGIN_STAGE("OBJECT FILE");

  target->build_object(&args);

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
