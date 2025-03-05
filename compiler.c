#include "compiler.h"

#include "alloc.h"
#include "codegen.h"
#include "graphcol.h"
#include "hashtbl.h"
#include "ir/build.h"
#include "ir/eliminate_phi.h"
#include "ir/ir.h"
#include "ir/prettyprint.h"
#include "ir/validate.h"
#include "log.h"
#include "lower.h"
#include "lsra.h"
#include "opts/cnst_fold.h"
#include "opts/inline.h"
#include "opts/instr_comb.h"
#include "opts/promote.h"
#include "parse.h"
#include "preproc.h"
#include "profile.h"
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
  const struct target *target;

  struct compile_file output;
};

enum compiler_create_result
create_compiler(struct program *program, const struct target *target,
                struct compile_file output, const char *path,
                const struct compile_args *args, struct compiler **compiler) {
  *compiler = nonnull_malloc(sizeof(**compiler));

  (*compiler)->args = *args;
  (*compiler)->target = target;
  (*compiler)->output = output;

  struct preproc_create_args preproc_args = {
      .target = args->target,
      .path = path,
      .num_include_paths = args->num_include_paths,
      .include_paths = args->include_paths,
      .verbose = args->verbose,
      .fixed_timestamp = args->fixed_timestamp,
  };

  if (preproc_create(program, preproc_args, &(*compiler)->preproc) !=
      PREPROC_CREATE_RESULT_SUCCESS) {
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

  return COMPILER_CREATE_RESULT_SUCCESS;
}

static void debug_print_stage(struct compiler *compiler, struct ir_unit *ir,
                              const char *name) {
  slog("\n\n----------  %s  ----------\n", name);

  // TODO: we should also respect log symbols in parse/typechk, but less
  // pressing at the moment
  if (compiler->args.log_symbols) {
    struct ir_glb *glb = ir->first_global;

    while (glb) {
      if (glb->name && hashtbl_lookup(compiler->args.log_symbols, &glb->name)) {
        debug_print_glb(stderr, glb, NULL, NULL);
      }

      glb = glb->succ;
    }
  } else {
    debug_print_ir(stderr, ir, NULL, NULL);
  }
}

FILE *compiler_open_file(struct compile_file file) {
  switch (file.ty) {
  case COMPILE_FILE_TY_NONE:
    BUG("can't open file ty NONE");
  case COMPILE_FILE_TY_PATH:
    return fopen(file.path, "w");
  case COMPILE_FILE_TY_STDOUT:
    return stdout;
  case COMPILE_FILE_TY_STDERR:
    return stderr;
  }
}

static enum compile_result compile_stage_preproc(struct compiler *compiler,
                                                 ...) {
  // variadic dummy because takes no args but macro needs args

  // this is only run in preproc only mode (`-E/--preprocess`), else it is
  // part of parsing

  FILE *file = compiler_open_file(compiler->output);
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
    debug_print_stage(compiler, *ir, "ir");
  }

  ir_validate(*ir, IR_VALIDATE_FLAG_NONE);

  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result compile_stage_inline(struct compiler *compiler,
                                                struct ir_unit *ir) {
  if (compiler->args.opts_level < COMPILE_OPTS_LEVEL_2) {
    return COMPILE_RESULT_SUCCESS;
  }

  opts_inline(ir);

  if (log_enabled()) {
    debug_print_stage(compiler, ir, "inline");
  }

  ir_validate(ir, IR_VALIDATE_FLAG_NONE);

  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result compile_stage_lower_abi(struct compiler *compiler,
                                                   struct ir_unit *ir) {
  lower_abi(ir);

  if (log_enabled()) {
    debug_print_stage(compiler, ir, "lower_abi");
  }

  ir_validate(ir, IR_VALIDATE_FLAG_NONE);

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
    debug_print_stage(compiler, ir, "opts");
  }

  ir_validate(ir, IR_VALIDATE_FLAG_NONE);

  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result compile_stage_lower(struct compiler *compiler,
                                               struct ir_unit *ir) {
  lower(ir);

  if (log_enabled()) {
    debug_print_stage(compiler, ir, "lower");
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
    debug_print_stage(compiler, ir, "regalloc");
  }

  ir_validate(ir, IR_VALIDATE_FLAG_LOWERED_POINTERS);

  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result compile_stage_elim_phi(struct compiler *compiler,
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
      eliminate_phi(glb->func);
      ir_rebuild_ids(glb->func);
      break;
    }

    glb = glb->succ;
  }

  if (log_enabled()) {
    debug_print_stage(compiler, ir, "elim_phi");
  }

  ir_validate(ir, IR_VALIDATE_FLAG_LOWERED_POINTERS |
                      IR_VALIDATE_FLAG_ALLOW_MIXED_PHIS);

  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result
compile_stage_codegen_prepare(struct compiler *compiler, struct ir_unit *ir) {
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
      ir_eliminate_redundant_ops(
          glb->func, IR_ELIMINATE_REDUNDANT_OPS_FLAG_ELIM_BRANCHES |
                         IR_ELIMINATE_REDUNDANT_OPS_FLAG_ELIM_MOVS |
                         IR_ELIMINATE_REDUNDANT_OPS_FLAG_DONT_ELIM_LCLS);

      // now we remove all gathers as they are pointless
      // important we do this last as otherwise other instructions may get
      // wrongly remopved

      struct ir_func_iter iter =
          ir_func_iter(glb->func, IR_FUNC_ITER_FLAG_NONE);

      struct vector *gathers = vector_create(sizeof(struct ir_op *));

      struct ir_op *op;
      size_t num_gathers = 0;
      while (ir_func_iter_next(&iter, &op)) {
        if (op->ty == IR_OP_TY_GATHER) {
          num_gathers++;
          vector_push_back(gathers, &op);
        }

        if (op->ty == IR_OP_TY_RET && op->ret.value &&
            op->ret.value->ty == IR_OP_TY_GATHER) {
          op->ret.value = NULL;
        }
      }

      for (size_t i = 0; i < num_gathers; i++) {
        ir_detach_op(glb->func, *(struct ir_op **)vector_get(gathers, i));
      }
      vector_free(&gathers);

      ir_rebuild_flags(glb->func);
      ir_rebuild_ids(glb->func);
      break;
    }

    glb = glb->succ;
  }

  if (log_enabled()) {
    debug_print_stage(compiler, ir, "codegen_prepare");
  }

  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result
compile_stage_codegen(struct compiler *compiler, struct ir_unit *ir,
                      struct cg_unit **codegen_unit) {
  const struct target *target = ir->target;

  *codegen_unit = codegen(ir, compiler->args.codegen_flags);

  if (log_enabled() && target->debug_print_codegen) {
    debug_print_stage(compiler, ir, "emit");
    target->debug_print_codegen(stderr, *codegen_unit);
  }

  if (compiler->args.build_asm_file) {
    if (target->emit_asm) {
      FILE *file = compiler_open_file(compiler->output);

      if (!file) {
        return COMPILE_RESULT_BAD_FILE;
      }

      target->emit_asm(file, *codegen_unit, compiler->args.codegen_flags);

      fclose(file);

      return COMPILE_RESULT_SUCCESS;
    } else if (target->debug_print_codegen) {
      warn("using debug codegen output; not valid assembler input");
      FILE *file = compiler_open_file(compiler->output);

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
compile_stage_emit(UNUSED struct compiler *compiler, struct cg_unit *unit,
                   struct emitted_unit *emitted_unit) {
  *emitted_unit = unit->target->emit_function(unit);

  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result
compile_stage_build_object(struct compiler *compiler,
                           struct emitted_unit *emitted_unit) {

  invariant_assert(compiler->output.ty == COMPILE_FILE_TY_PATH, "can't build object to stdout/stderr");

  struct build_object_args args = {.compile_args = &compiler->args,
                                   .output = compiler->output.path,
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

  COMPILER_STAGE(INLINE, inline, ir);

  // lower ABI happens before opts, and handles transforming calls into their
  // actual types (e.g large structs to pointers) we do this early because it
  // helps with opts
  COMPILER_STAGE(LOWER_ABI, lower_abi, ir);

  if (compiler->args.opts_level != COMPILE_OPTS_LEVEL_0) {
    COMPILER_STAGE(OPTS, opts, ir);
  }

  COMPILER_STAGE(LOWER, lower, ir);
  COMPILER_STAGE(REGALLOC, regalloc, ir);
  COMPILER_STAGE(ELIM_PHI, elim_phi, ir);
  COMPILER_STAGE(CODEGEN_PREPARE, codegen_prepare, ir);

  struct cg_unit *codegen_unit;
  COMPILER_STAGE(CODEGEN, codegen, ir, &codegen_unit);

  if (compiler->args.build_asm_file) {
    // finished, only needed object file
    codegen_free(&codegen_unit);

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
