#include "compiler.h"

#include "alloc.h"
#include "codegen.h"
#include "diagnostics.h"
#include "fs.h"
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
#include "opts/cnst_branches.h"
#include "opts/cnst_fold.h"
#include "opts/inline.h"
#include "opts/instr_comb.h"
#include "opts/promote.h"
#include "parse.h"
#include "preproc.h"
#include "profile.h"
#include "program.h"
#include "target.h"
#include "util.h"
#include "vector.h"

#include <stdio.h>

struct compiler {
  struct arena_allocator *arena;
  struct fs *fs;

  struct preproc *preproc;
  struct program program;
  struct compile_args args;
  struct parser *parser;
  struct typechk *typechk;
  struct compiler_diagnostics *diagnostics;
  enum compile_preproc_mode mode;
  const struct target *target;

  bool typechk_success;
  struct typechk_result typechk_result;

  struct compile_file output;
};

enum compiler_create_result
compiler_create(const struct compiler_create_args *args,
                struct compiler **compiler) {
  *compiler = nonnull_malloc(sizeof(**compiler));

  (*compiler)->program = args->program;
  (*compiler)->fs = args->fs;
  (*compiler)->args = args->args;
  (*compiler)->target = args->target;
  (*compiler)->output = args->output;
  (*compiler)->mode = args->mode;
  (*compiler)->typechk_success = false;

  struct compiler_diagnostics_args diag_args = {
      .warnings_as_errors = args->args.warnings_as_errors};

  (*compiler)->diagnostics = compiler_diagnostics_create(diag_args);

  struct preproc_create_args preproc_args = {
      .target = args->args.target,
      .path = args->working_dir,
      .c_standard = args->args.c_standard,
      .num_sys_include_paths = args->args.num_sys_include_paths,
      .sys_include_paths = args->args.sys_include_paths,
      .num_include_paths = args->args.num_include_paths,
      .include_paths = args->args.include_paths,
      .num_defines = args->args.num_defines,
      .defines = args->args.defines,
      .verbose = args->args.verbose,
      .fixed_timestamp = args->args.fixed_timestamp,
  };

  if (preproc_create(args->program, args->fs, preproc_args,
                     (*compiler)->diagnostics,
                     (*compiler)->mode,
                     &(*compiler)->preproc) != PREPROC_CREATE_RESULT_SUCCESS) {
    err("failed to create preproc");
    return COMPILER_CREATE_RESULT_FAILURE;
  }

  if (parser_create(args->program, (*compiler)->preproc, args->args.c_standard, args->mode,
                    (*compiler)->diagnostics,
                    &(*compiler)->parser) != PARSER_CREATE_RESULT_SUCCESS) {
    err("failed to create parser");
    return COMPILER_CREATE_RESULT_FAILURE;
  }

  if (typechk_create(args->target, &args->args, (*compiler)->parser,
                     (*compiler)->diagnostics,
                     &(*compiler)->typechk) != TYPECHK_CREATE_RESULT_SUCCESS) {
    err("failed to create typechk");
    return COMPILER_CREATE_RESULT_FAILURE;
  }

  arena_allocator_create("compiler", &(*compiler)->arena);

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

void compiler_open_file(struct compile_file file, FILE **f, const char **path) {
  switch (file.ty) {
  case COMPILE_FILE_TY_NONE:
    BUG("can't open file ty NONE");
  case COMPILE_FILE_TY_PATH:
    *f = fopen(file.path, "w");
    *path = file.path;
    break;
  case COMPILE_FILE_TY_FILE:
    *f = file.file;
    *path = file.path;
    break;
  case COMPILE_FILE_TY_STDOUT:
    *f = stdout;
    *path = NULL;
    break;
  }
}

static void compiler_print_diagnostics_context(struct compiler *compiler,
                                               FILE *sink,
                                               struct text_span span,
                                               struct text_pos point) {
  // FIXME: super inefficient

  struct text_pos start = span.start;
  struct text_pos end = span.end;

  struct fs_file file;

  // TODO: handle span that crosses file

  if (!span.start.file ||
      !fs_read_path(compiler->fs, MK_USTR(span.start.file), &file)) {
    fprintf(sink, "(could not read function file %s)", span.start.file);
    return;
  }

  const char *text = file.data;
  size_t len = file.len;

  DEBUG_ASSERT(end.line >= start.line, "end line %zu was before start line %zu",
               end.line, start.line);

  size_t start_len;
  size_t end_len;
  bool has_point;

  if (TEXT_POS_INVALID(point)) {
    has_point = false;
    start_len = end.idx - start.idx;
    end_len = 0;
  } else {
    has_point = true;
    start_len = point.idx - start.idx;
    if (start_len) {
      start_len -= 1 /* bc inclusive ranges */;
    }

    end_len = end.idx - point.idx;
  }

#define DIAG_LINE_LIM 50

  if (start.line == TEXT_POS_INVALID_LINE ||
      end.line == TEXT_POS_INVALID_LINE) {
    fprintf(sink, "(unable to print due to invalid line pos, likely from "
                    "macro expansion)");
    return;
  }

  if (end.line - start.line > DIAG_LINE_LIM) {
    fprintf(sink,
            "(unable to print due to line lim %d, from line %zu to line %zu)",
            DIAG_LINE_LIM, start.line, end.line);
    return;
  }

  size_t line = 0;
  for (size_t i = 0; i < len; i++) {
    if (text[i] != '\n') {
      continue;
    }

    line++;
    if (line > end.line) {
      return;
    }

    if (i + 1 >= len) {
      fprintf(sink, "    %zu | <eof>\n", line + 1);
      return;
    }

    if (line >= start.line) {
      const char *next = strchr(&text[i + 1], '\n');
      size_t line_len = (size_t)(next - &text[i + 1]);

      size_t offset = 7 + num_digits(line + 1);
      fprintf(sink, "    %zu | %.*s\n", line + 1, (int)line_len,
              &text[i + 1]);

      fprintf(sink, "%*s", (int)offset, "");

      if (line == start.line) {
        size_t rel_idx_start = start.idx - (i + 1);
        fprintf(sink, "%*s", (int)rel_idx_start, "");

        line_len -= rel_idx_start;
      }

      fprintf(sink, PR_GREEN);

      if (line_len == 0) {
        fprintf(sink, "~");
        fprintf(sink, "\n");
        fprintf(sink, PR_RESET);
        continue;
      }

      size_t start_print_len = MIN(start_len, line_len);
      for (size_t j = 0; j < start_print_len; j++) {
        fprintf(sink, "~");
      }
      start_len -= start_print_len;
      line_len -= start_print_len;

      if (has_point && point.line == line) {
        fprintf(sink, "^");
      }

      if (!start_len) {
        size_t end_print_len = MIN(end_len, line_len);
        for (size_t j = 0; j < end_print_len; j++) {
          fprintf(sink, "~");
        }
        end_len -= end_print_len;
      }

      fprintf(sink, PR_RESET);
      fprintf(sink, "\n");
    }
  }
}

static void compiler_print_diagnostics(struct compiler *compiler) {
  if (!compiler->args.print_diagnostics) {
    return;
  }

  FILE *file;
  bool close;
  if (compiler->args.diagnostics_sink) {
    file = fopen(compiler->args.diagnostics_sink, "w");
    close = true;

    if (!file) {
      err("opening diagnostic sink '%s' failed!", compiler->args.diagnostics_sink);
      return;
    }
  } else {
    file = stderr;
    close = false;
  }

  struct compiler_diagnostics *diagnostics = compiler->diagnostics;

  struct compiler_diagnostics_iter iter =
      compiler_diagnostics_iter(diagnostics);
  struct compiler_diagnostic diagnostic;

  while (compiler_diagnostics_iter_next(&iter, &diagnostic)) {
    struct text_span span = diagnostic.span;
    struct text_pos point = diagnostic.point;
    const char *message = diagnostic.message;
    bool has_pos;

    switch (diagnostic.ty.class) {
    case COMPILER_DIAGNOSTIC_CLASS_PREPROC:
    case COMPILER_DIAGNOSTIC_CLASS_PARSE:
    case COMPILER_DIAGNOSTIC_CLASS_SEMANTIC:
      has_pos = true;
      break;
    case COMPILER_DIAGNOSTIC_CLASS_INTERNAL:
      has_pos = false;
      break;
    }

    if (diagnostic.ty.severity == COMPILER_DIAGNOSTIC_SEVERITY_WARN  && compiler->args.no_warnings) {
      continue;
    }

    fprintf(file, PR_BOLD PR_WHITE "%s:%zu:%zu: " PR_RESET, span.start.file,
            span.start.line, span.start.col);

    // this is currently never set to true, because diagnostics.c sets level to
    // ERROR when -Werror is enabled so LSP also sees errors
    bool werror = false;
    const char *prefix = "";

    switch (diagnostic.ty.severity) {
    case COMPILER_DIAGNOSTIC_SEVERITY_ERROR:
      fprintf(file, PR_BOLD PR_RED "error: " PR_RESET);
      break;
    case COMPILER_DIAGNOSTIC_SEVERITY_WARN:
      prefix = "-W";
      if (compiler->args.warnings_as_errors) {
        fprintf(file, PR_BOLD PR_RED "error: " PR_RESET);
        werror = true;
      } else {
        fprintf(file, PR_BOLD PR_MAGENTA "warning: " PR_RESET);
      }
      break;
    case COMPILER_DIAGNOSTIC_SEVERITY_INFO:
      fprintf(file, PR_BOLD PR_WHITE "info: " PR_RESET);
      break;
    }

    fprintf(file, PR_BOLD PR_WHITE "%s", message);

    fprintf(file, " [");
    if (werror) {
      fprintf(file, "-Werror,");
    }

    fprintf(file, "%s%s", prefix, diagnostic.ty.name);
    fprintf(file, "]\n" PR_RESET);

    if (has_pos) {
      compiler_print_diagnostics_context(compiler, file, span, point);
    }

    fprintf(file, "\n");
  }

  fflush(file);

  if (close) {
    fclose(file);
  }
}

static enum compile_result compile_stage_preproc(struct compiler *compiler,
                                                 ...) {
  // variadic dummy because takes no args but macro needs args

  // this is only run in preproc only mode (`-E/--preprocess`), else it is
  // part of parsing

  FILE *file;
  const char *path;
  compiler_open_file(compiler->output, &file, &path);
  if (!file) {
    return COMPILE_RESULT_BAD_FILE;
  }

  // TODO: preproc can fail
  preproc_process(compiler->preproc, file);

  compiler_print_diagnostics(compiler);

  fclose(file);
  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result compile_stage_lex(struct compiler *compiler,
                                             enum compile_preproc_mode mode) {
  // variadic dummy because takes no args but macro needs args

  FILE *file;
  const char *path;
  compiler_open_file(compiler->output, &file, &path);
  if (!file) {
    return COMPILE_RESULT_BAD_FILE;
  }

  // TODO: lex can fail, also this doesn't match structure of rest of compiler (where create and free are outside of these functions)

  struct lexer *lexer;
  lexer_create(compiler->program, compiler->preproc, compiler->args.c_standard, mode, &lexer);

  lex_all(lexer);

  lexer_free(&lexer);

  fclose(file);
  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result
compile_stage_parse(struct compiler *compiler,
                    struct parse_result *parse_result) {
  compiler->typechk_success = false;

  *parse_result = parse(compiler->parser);

  if (log_enabled()) {
    debug_print_ast(compiler->parser, &parse_result->translation_unit);
  }

  // TODO: err out if preproc fails
  compiler_print_diagnostics(compiler);

  if (parse_result->ty == PARSE_RESULT_TY_FAILURE) {
    return COMPILE_RESULT_FAILURE;
  }

  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result
compile_stage_typechk(struct compiler *compiler,
                      struct parse_result *parse_result,
                      struct typechk_result *typechk_result) {
  *typechk_result =
      td_typechk(compiler->typechk, &parse_result->translation_unit);

  compiler->typechk_result = *typechk_result;

  if (log_enabled()) {
    debug_print_td(compiler->typechk, compiler->args.log_symbols,
                   &typechk_result->translation_unit);
  }

  compiler_print_diagnostics(compiler);

  // do this unconditionally, it really means "was typechk reached (ie no syntax errors)"
  compiler->typechk_success = true;

  if (typechk_result->ty == TYPECHK_RESULT_TY_FAILURE) {
    return COMPILE_RESULT_FAILURE;
  }

  return COMPILE_RESULT_SUCCESS;
}

static enum compile_result
compile_stage_ir(struct compiler *compiler, const struct target *target,
                 struct typechk_result *typechk_result, struct ir_unit **ir) {
  enum ir_build_flags flags = IR_BUILD_FLAG_NONE;

  if (compiler->args.opts_level == COMPILE_OPTS_LEVEL_0) {
    flags |= IR_BUILD_FLAG_SPILL_ALL;
  }

  *ir = build_ir_for_translationunit(target, compiler->typechk, compiler->arena,
                                     &typechk_result->translation_unit, flags);

  if (log_enabled()) {
    debug_print_stage(compiler, *ir, "ir");
  }

  ir_validate(*ir, IR_VALIDATE_FLAG_NONE);

  ir_prune_globals(*ir);

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

    opts_cnst_branches(ir);

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
      ir_rebuild_func_ids(glb->func);
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
  ir_rebuild_glb_ids(ir);

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
      ir_rebuild_func_ids(glb->func);
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
      FILE *file;
      const char *path;
      compiler_open_file(compiler->output, &file, &path);

      if (!file) {
        return COMPILE_RESULT_BAD_FILE;
      }

      target->emit_asm(file, *codegen_unit, compiler->args.codegen_flags);

      fclose(file);

      return COMPILE_RESULT_SUCCESS;
    } else if (target->debug_print_codegen) {
      warn("using debug codegen output; not valid assembler input");

      FILE *file;
      const char *path;
      compiler_open_file(compiler->output, &file, &path);

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

  invariant_assert(compiler->output.ty == COMPILE_FILE_TY_PATH || compiler->output.ty == COMPILE_FILE_TY_FILE,
                   "can't build object to stdout/stderr");

  FILE *file;
  const char *path;
  compiler_open_file(compiler->output, &file, &path);

  struct build_object_args args = {.compile_args = &compiler->args,
                                   .output = file,
                                   .entries = emitted_unit->entries,
                                   .num_entries = emitted_unit->num_entries};

  compiler->target->build_object(&args);

  fflush(file);
  fclose(file);

  if (log_enabled()) {
    if (!compiler->target->debug_disasm) {
      warn("DISASM not supported for current target");
    } else {
      if (path) {
        compiler->target->debug_disasm(compiler->args.target, path, NULL);
      } else {
        err("output type did not have path associated, cannot disasm");
      }
    }
  }

  return COMPILE_RESULT_SUCCESS;
}

#define COMPILER_STAGE_NO_LOG(stage, lo, ...)                                  \
  {                                                                            \
    PROFILE_BEGIN(lo);                                                         \
    enum compile_result result = compile_stage_##lo(compiler, __VA_ARGS__);    \
    PROFILE_END(lo);                                                           \
    if (result != COMPILE_RESULT_SUCCESS) {                                    \
      return result;                                                           \
    }                                                                          \
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
    COMPILER_STAGE_NO_LOG(stage, lo, __VA_ARGS__);                             \
  }

enum compile_result compile(struct compiler *compiler) {
  if (compiler->args.preproc_only) {
    // preproc is kept local as it is seperate to other stages

    COMPILER_STAGE(PREPROC, preproc, NULL);

    return COMPILE_RESULT_SUCCESS;
  }

  if (compiler->args.lex_only) {
    // only really used for profiling

    COMPILER_STAGE_NO_LOG(LEX, lex, compiler->mode);

    return COMPILE_RESULT_SUCCESS;
  }

  struct parse_result parse_result;
  COMPILER_STAGE(PARSE, parse, &parse_result);

  if (compiler->args.parse_only) {
    return COMPILE_RESULT_SUCCESS;
  }

  struct typechk_result typechk_result;
  COMPILER_STAGE(TYPECHK, typechk, &parse_result, &typechk_result);

  if (compiler->args.syntax_only) {
    // if diagnostics occurred, exit will of already occurred via COMPILER_STAGE
    // macro
    return COMPILE_RESULT_SUCCESS;
  }

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

void compiler_get_tchk(struct compiler *compiler, struct typechk **tchk, struct typechk_result *result) {
  if (!compiler->typechk_success) {
    *tchk = NULL;
    return;
  }

  *tchk = compiler->typechk;
  *result = compiler->typechk_result;
}

struct compiler_diagnostics *
compiler_get_diagnostics(struct compiler *compiler) {
  return compiler->diagnostics;
}

struct preproc *compiler_get_preproc(struct compiler *compiler) {
  return compiler->preproc;
}

void free_compiler(struct compiler **compiler) {
  preproc_free(&(*compiler)->preproc);
  parser_free(&(*compiler)->parser);
  typechk_free(&(*compiler)->typechk);
  compiler_diagnostics_free(&(*compiler)->diagnostics);
  arena_allocator_free(&(*compiler)->arena);

  free(*compiler);
  *compiler = NULL;
}
