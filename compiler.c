#include "compiler.h"

#include "aarch64.h"
#include "alloc.h"
#include "codegen.h"
#include "eep.h"
#include "emit.h"
#include "io.h"
#include "ir/build.h"
#include "ir/eliminate_phi.h"
#include "ir/ir.h"
#include "ir/prettyprint.h"
#include "ir/validate.h"
#include "lex.h"
#include "log.h"
#include "lower.h"
#include "lsra.h"
#include "macos/mach-o.h"
#include "parse.h"
#include "preproc.h"
#include "rv32i.h"
#include "target.h"
#include "util.h"
#include "vector.h"

#include <stdio.h>

struct compiler {
  struct arena_allocator *arena;

  struct compile_args args;
  struct preproc *preproc;
  struct parser *parser;
  struct typechk *typechk;

  char *output;
};

static const struct target *get_target(const struct compile_args *args) {
  switch (args->target_arch) {
  case COMPILE_TARGET_ARCH_NATIVE:
    BUG("hit COMPILE_TARGET_ARCH_NATIVE in compiler! should have been chosen "
        "earlier");
  case COMPILE_TARGET_ARCH_MACOS_X86_64:
    TODO("macOS x64 target not yet implemented");
  // FIXME: linux is actually subtly different in register usage and calling conv
  case COMPILE_TARGET_ARCH_LINUX_ARM64:
    return &AARCH64_LINUX_TARGET;
  case COMPILE_TARGET_ARCH_MACOS_ARM64:
    return &AARCH64_MACOS_TARGET;
  case COMPILE_TARGET_ARCH_RV32I:
    return &RV32I_TARGET;
  case COMPILE_TARGET_ARCH_EEP:
    BUG("redo eep");
    // return &EEP_TARGET;
  }

  BUG("unexpected target in `get_target`");
}

enum compiler_create_result
create_compiler(struct program *program, const char *output, const char *path,
                const struct compile_args *args, struct compiler **compiler) {
  *compiler = nonnull_malloc(sizeof(**compiler));

  (*compiler)->args = *args;

  if (preproc_create(program, path, args->num_include_paths,
                     args->include_paths, args->fixed_timestamp,
                     &(*compiler)->preproc) != PREPROC_CREATE_RESULT_SUCCESS) {
    err("failed to create preproc");
    return COMPILER_CREATE_RESULT_FAILURE;
  }

  if (parser_create(program, (*compiler)->preproc, &(*compiler)->parser) !=
      PARSER_CREATE_RESULT_SUCCESS) {
    err("failed to create parser");
    return COMPILER_CREATE_RESULT_FAILURE;
  }

  const struct target *target = get_target(args);
  if (typechk_create(target, args, (*compiler)->parser,
                     &(*compiler)->typechk) != TYPECHK_CREATE_RESULT_SUCCESS) {
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

static void debug_print_stage(struct ir_unit *ir,
                              UNUSED_ARG(const char *name)) {
  debug_print_ir(stderr, ir, NULL, NULL);
}

#define COMPILER_STAGE(stage)                                                  \
  {                                                                            \
    disable_log();                                                             \
                                                                               \
    if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_##stage)) {           \
      enable_log();                                                            \
      BEGIN_STAGE(#stage);                                                     \
    }                                                                          \
  }

enum compile_result compile(struct compiler *compiler) {
  struct parse_result parse_result;

  if (compiler->args.preproc_only) {
    // preproc is kept local as it is seperate to other stages

    BEGIN_STAGE("PREPROC");

    if (compiler->args.log_flags & COMPILE_LOG_FLAGS_PREPROC) {
      enable_log();
    }

    FILE *file;
    // FIXME: hacky (and in main.c)
    if (strcmp(compiler->output, "stdout") == 0) {
      file = stdout;
    } else {
      file = fopen(compiler->output, "w");
    }

    if (!file) {
      return COMPILE_RESULT_BAD_FILE;
    }

    preproc_process(compiler->preproc, file);

    disable_log();

    return COMPILE_RESULT_SUCCESS;
  }

  {
    COMPILER_STAGE(PARSE);

    parse_result = parse(compiler->parser);

    BEGIN_STAGE("AST");

    if (log_enabled()) {
      debug_print_ast(compiler->parser, &parse_result.translation_unit);
    }
  }

  struct typechk_result typechk_result;
  {
    COMPILER_STAGE(TYPECHK);

    typechk_result =
        td_typechk(compiler->typechk, &parse_result.translation_unit);

    if (log_enabled()) {
      debug_print_td(compiler->typechk, &typechk_result.translation_unit);
    }
  }

  const struct target *target = get_target(&compiler->args);
  struct ir_unit *ir;
  {
    COMPILER_STAGE(IR);

    ir =
        build_ir_for_translationunit(target, compiler->typechk, compiler->arena,
                                     &typechk_result.translation_unit);


    if (log_enabled()) {
      debug_print_stage(ir, "ir");
    }

    ir_validate(ir);
  }

  {
    COMPILER_STAGE(LOWER);

    lower(ir, target);

    if (log_enabled()) {
      debug_print_stage(ir, "lower");
    }

    ir_validate(ir);
  }

  {
    COMPILER_STAGE(REGALLOC);

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

    if (log_enabled()) {
      debug_print_stage(ir, "regalloc");
    }

    ir_validate(ir);

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
        rebuild_ids(glb->func);
        break;
      }

      glb = glb->succ;
    }

    if (log_enabled()) {
      debug_print_stage(ir, "elim_phi");
    }

    ir_validate(ir);
  }

  {
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
        rebuild_ids(glb->func);
        break;
      }

      glb = glb->succ;
    }
  }

  struct emitted_unit unit;
  {
    COMPILER_STAGE(EMIT);

    struct codegen_unit *codegen = target->codegen(ir);

    if (log_enabled() && target->debug_print_codegen) {
      debug_print_stage(ir, "emit");
      target->debug_print_codegen(stderr, codegen);
    }

    unit = target->emit_function(codegen);
  }

  struct build_object_args args = {.compile_args = &compiler->args,
                                   .output = compiler->output,
                                   .entries = unit.entries,
                                   .num_entries = unit.num_entries};

  {
    COMPILER_STAGE(ASM);

    target->build_object(&args);

    if (log_enabled()) {
      if (!target->debug_disasm) {
        warn("DISASM not supported for current target");
      } else {
        BEGIN_STAGE("DISASSEMBLY");
        target->debug_disasm(args.output);
      }
    }
  }

  return COMPILE_RESULT_SUCCESS;
}

void free_compiler(struct compiler **compiler) {
  arena_allocator_free(&(*compiler)->arena);
  parser_free(&(*compiler)->parser);

  free(*compiler);
  *compiler = NULL;
}
