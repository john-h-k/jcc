#include "compiler.h"

#include "alloc.h"
#include "codegen.h"
#include "graphcol.h"
#include "ir/build.h"
#include "ir/eliminate_phi.h"
#include "ir/ir.h"
#include "ir/prettyprint.h"
#include "ir/validate.h"
#include "log.h"
#include "lower.h"
#include "lsra.h"
#include "opts/cnst_fold.h"
#include "opts/instr_comb.h"
#include "opts/promote.h"
#include "parse.h"
#include "preproc.h"
#include "profile.h"
#include "target.h"
#include "util.h"

#include <stdio.h>

struct compiler {
  struct arena_allocator *arena;

  struct compile_args args;
  struct preproc *preproc;
  struct parser *parser;
  struct typechk *typechk;
  const struct target *target;

  char *output;
};

enum compiler_create_result
create_compiler(struct program *program, const struct target *target,
                const char *output, const char *path,
                const struct compile_args *args, struct compiler **compiler) {
  *compiler = nonnull_malloc(sizeof(**compiler));

  (*compiler)->args = *args;
  (*compiler)->target = target;

  if (preproc_create(program, args->target, path, args->num_include_paths,
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

  if (typechk_create(target, args, (*compiler)->parser,
                     &(*compiler)->typechk) != TYPECHK_CREATE_RESULT_SUCCESS) {
    err("failed to create typechk");
    return COMPILER_CREATE_RESULT_FAILURE;
  }

  arena_allocator_create(&(*compiler)->arena);

  size_t sz = strlen(output) + 1;
  (*compiler)->output = arena_alloc((*compiler)->arena, sz);
  strcpy((*compiler)->output, output);
  (*compiler)->output[sz - 1] = '\0';

  return COMPILER_CREATE_RESULT_SUCCESS;
}

static void debug_print_stage(struct ir_unit *ir, const char *name) {
  slog("\n\n----------  %s  ----------\n", name);
  debug_print_ir(stderr, ir, NULL, NULL);
}

static enum compile_result compile_stage_preproc(struct compiler *compiler,
                                                 ...) {
  // variadic dummy because takes no args but macro needs args

  // this is only run in preproc only mode (`-E/--preprocess`), else it is part
  // of parsing

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

  // TODO: preproc can fail
  preproc_process(compiler->preproc, file);

  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result
compile_stage_parse(struct compiler *compiler,
                    struct parse_result *parse_result) {
  *parse_result = parse(compiler->parser);

  if (log_enabled()) {
    debug_print_ast(compiler->parser, &parse_result->translation_unit);
  }

  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result
compile_stage_typechk(struct compiler *compiler,
                      struct parse_result *parse_result,
                      struct typechk_result *typechk_result) {
  *typechk_result =
      td_typechk(compiler->typechk, &parse_result->translation_unit);

  if (log_enabled()) {
    debug_print_td(compiler->typechk, &typechk_result->translation_unit);
  }

  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result
compile_stage_ir(struct compiler *compiler, const struct target *target,
                 struct typechk_result *typechk_result, struct ir_unit **ir) {
  *ir = build_ir_for_translationunit(target, compiler->typechk, compiler->arena,
                                     &typechk_result->translation_unit);

  if (log_enabled()) {
    debug_print_stage(*ir, "ir");
  }

  ir_validate(*ir, IR_VALIDATE_FLAG_NONE);

  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result compile_stage_opts(struct compiler *compiler,
                                              struct ir_unit *ir) {
  switch (compiler->args.opts_level) {
  case COMPILE_OPTS_LEVEL_0:
    unreachable();
  case COMPILE_OPTS_LEVEL_1:
    opts_cnst_fold(ir);
    break;
  case COMPILE_OPTS_LEVEL_2:
    opts_cnst_fold(ir);
    opts_instr_comb(ir);
    break;
  case COMPILE_OPTS_LEVEL_3:
    opts_cnst_fold(ir);
    opts_promote(ir);
    opts_cnst_fold(ir);
    opts_instr_comb(ir);
    break;
  }

  if (log_enabled()) {
    debug_print_stage(ir, "opts");
  }

  ir_validate(ir, IR_VALIDATE_FLAG_NONE);

  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result compile_stage_lower(UNUSED struct compiler *compiler,
                                               struct ir_unit *ir) {
  lower(ir);

  if (log_enabled()) {
    debug_print_stage(ir, "lower");
  }

  ir_validate(ir, IR_VALIDATE_FLAG_LOWERED_POINTERS);

  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result compile_stage_regalloc(struct compiler *compiler,
                                                  struct ir_unit *ir) {

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
      if (compiler->args.use_graphcol_regalloc) {
        graphcol_register_alloc(glb->func, compiler->target->reg_info);
      } else {
        lsra_register_alloc(glb->func, ir->target->reg_info);
      }
      break;
    }

    glb = glb->succ;
  }

  if (log_enabled()) {
    debug_print_stage(ir, "regalloc");
  }

  ir_validate(ir, IR_VALIDATE_FLAG_LOWERED_POINTERS);

  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result
compile_stage_elim_phi(UNUSED struct compiler *compiler, struct ir_unit *ir) {

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
      eliminate_phi(glb->func);
      rebuild_ids(glb->func);
      break;
    }

    glb = glb->succ;
  }

  if (log_enabled()) {
    debug_print_stage(ir, "elim_phi");
  }

  ir_validate(ir, IR_VALIDATE_FLAG_LOWERED_POINTERS | IR_VALIDATE_FLAG_ALLOW_MIXED_PHIS);

  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result
compile_stage_codegen_prepare(UNUSED struct compiler *compiler,
                              struct ir_unit *ir) {
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
      ir_order_basicblocks(glb->func);
      eliminate_redundant_ops(glb->func,
                              ELIMINATE_REDUNDANT_OPS_FLAG_ELIM_BRANCHES |
                                  ELIMINATE_REDUNDANT_OPS_FLAG_ELIM_MOVS);
      rebuild_flags(glb->func);
      rebuild_ids(glb->func);
      break;
    }

    glb = glb->succ;
  }

  if (log_enabled()) {
    debug_print_stage(ir, "codegen_prepare");
  }

  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result
compile_stage_codegen(struct compiler *compiler, struct ir_unit *ir,
                      struct codegen_unit **codegen_unit) {
  const struct target *target = ir->target;

  *codegen_unit = codegen(ir);

  if (log_enabled() && target->debug_print_codegen) {
    debug_print_stage(ir, "emit");
    target->debug_print_codegen(stderr, *codegen_unit);
  }

  if (compiler->args.build_asm_file) {
    if (target->debug_print_codegen) {
      FILE *file = fopen(compiler->output, "w");

      if (!file) {
        return COMPILE_RESULT_BAD_FILE;
      }

      target->debug_print_codegen(file, *codegen_unit);

      fclose(file);

      return COMPILE_RESULT_SUCCESS;
    } else {
      err("assembly not supported for this architecture");
      return COMPILE_RESULT_FAILURE;
    }
  }

  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result
compile_stage_emit(UNUSED struct compiler *compiler, struct codegen_unit *unit,
                   struct emitted_unit *emitted_unit) {
  *emitted_unit = unit->target->emit_function(unit);

  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result
compile_stage_build_object(struct compiler *compiler,
                           struct emitted_unit *emitted_unit) {

  struct build_object_args args = {.compile_args = &compiler->args,
                                   .output = compiler->output,
                                   .entries = emitted_unit->entries,
                                   .num_entries = emitted_unit->num_entries};

  compiler->target->build_object(&args);

  if (log_enabled()) {
    if (!compiler->target->debug_disasm) {
      warn("DISASM not supported for current target");
    } else {
      compiler->target->debug_disasm(args.output, NULL);
    }
  }

  return COMPILE_RESULT_SUCCESS;
}

#define COMPILER_STAGE(stage, lo, ...)                                         \
  {                                                                            \
    disable_log();                                                             \
                                                                               \
    if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_##stage)) {           \
      enable_log();                                                            \
      BEGIN_STAGE(#stage);                                                     \
    }                                                                          \
                                                                               \
    PROFILE_BEGIN(lo);                                                         \
    enum compile_result result = compile_stage_##lo(compiler, __VA_ARGS__);    \
    PROFILE_END(lo);                                                           \
    if (result != COMPILE_RESULT_SUCCESS) {                                    \
      return result;                                                           \
    }                                                                          \
  }

enum compile_result compile(struct compiler *compiler) {
  if (compiler->args.preproc_only) {
    // preproc is kept local as it is seperate to other stages

    COMPILER_STAGE(PREPROC, preproc, NULL);

    return COMPILE_RESULT_SUCCESS;
  }

  struct parse_result parse_result;
  COMPILER_STAGE(PARSE, parse, &parse_result);

  struct typechk_result typechk_result;
  COMPILER_STAGE(TYPECHK, typechk, &parse_result, &typechk_result);

  const struct target *target = compiler->target;
  struct ir_unit *ir;

  COMPILER_STAGE(IR, ir, target, &typechk_result, &ir);

  if (compiler->args.opts_level != COMPILE_OPTS_LEVEL_0) {
    COMPILER_STAGE(OPTS, opts, ir);
  }

  COMPILER_STAGE(LOWER, lower, ir);
  COMPILER_STAGE(REGALLOC, regalloc, ir);
  COMPILER_STAGE(ELIM_PHI, elim_phi, ir);
  COMPILER_STAGE(CODEGEN_PREPARE, codegen_prepare, ir);

  struct codegen_unit *codegen_unit;
  COMPILER_STAGE(CODEGEN, codegen, ir, &codegen_unit);

  if (compiler->args.build_asm_file) {
    // finished, only needed object file
    return COMPILE_RESULT_SUCCESS;
  }

  struct emitted_unit unit;
  COMPILER_STAGE(EMIT, emit, codegen_unit, &unit);

  COMPILER_STAGE(BUILD_OBJECT, build_object, &unit);

  // TODO: we should neaten all the lifetimes to make freeing more clear
  codegen_free(&codegen_unit);

  return COMPILE_RESULT_SUCCESS;
}

void free_compiler(struct compiler **compiler) {
  arena_allocator_free(&(*compiler)->arena);
  preproc_free(&(*compiler)->preproc);
  parser_free(&(*compiler)->parser);
  typechk_free(&(*compiler)->typechk);

  free(*compiler);
  *compiler = NULL;
}
