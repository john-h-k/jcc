#include "compiler.h"

#include "aarch64.h"
#include "alloc.h"
#include "codegen.h"
#include "eep.h"
#include "emit.h"
#include "ir/build.h"
#include "ir/eliminate_phi.h"
#include "ir/prettyprint.h"
#include "lex.h"
#include "log.h"
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

  char *output;
};

const char *mangle_str_cnst_name(struct arena_allocator *arena,
                                 const char *func_name, size_t id) {
  // TODO: this should all really be handled by the mach-o file
  func_name = "str";
  size_t func_name_len = strlen(func_name);

  size_t len = 0;
  len += func_name_len;
  len += 2; // strlen("l_"), required for local symbols
  len += 1; // surround function name with `.` so it cannot conflict with real
            // names

  if (id) {
    len += 1; // extra "." before id
  }

  size_t id_len = id ? num_digits(id) : 0;
  len += id_len;

  len += 1; // null char
  char *buff = arena_alloc(arena, len);
  size_t head = 0;

  strcpy(&buff[head], "l_");
  head += strlen("l_");

  buff[head++] = '.';
  strcpy(&buff[head], func_name);
  head += func_name_len;

  if (id) {
    buff[head++] = '.';

    size_t tail = head + id_len - 1;
    while (tail >= head) {
      buff[tail--] = (id % 10) + '0';
      id /= 10;
    }
  }

  head += id_len;
  buff[head++] = 0;

  debug_assert(head == len, "head (%zu) != len (%zu) in mangle_str_cnst_name",
               head, len);

  return buff;
}

enum compiler_create_result create_compiler(struct program *program,
                                            const char *output,
                                            const struct compile_args *args,
                                            struct compiler **compiler) {
  *compiler = nonnull_malloc(sizeof(**compiler));

  (*compiler)->args = *args;

  // preproc is kept local as it is seperate to other stages
  struct preproc *preproc;

  if (preproc_create(program, &preproc) != PREPROC_CREATE_RESULT_SUCCESS) {
    err("failed to create preproc");
    return COMPILER_CREATE_RESULT_FAILURE;
  }

  struct preprocessed_program preprocessed_program = preproc_process(preproc);

  if (parser_create(&preprocessed_program, &(*compiler)->parser) !=
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

void debug_print_stage(const struct ir_unit *ir, const char *name) {
  for (size_t i = 0; i < ir->num_funcs; i++) {
    struct ir_builder *irb = ir->funcs[i];

    debug_print_ir(stderr, irb, NULL, NULL);

    // TODO: fix logic
    char *buff = nonnull_malloc(strlen(name) + sizeof(".gv"));
    strcpy(buff, name);
    strcat(buff, ".gv");

    FILE *ir_graph = fopen(buff, "w");
    debug_print_ir_graph(ir_graph, irb);
    fclose(ir_graph);
  }
}

const struct target *get_target(const struct compile_args *args) {
  switch (args->target_arch) {
  case COMPILE_TARGET_ARCH_NATIVE:
    bug("hit COMPILE_TARGET_ARCH_NATIVE in compiler! should have been chosen "
        "earlier");
    break;
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

  struct parse_result result = parse(compiler->parser);

  BEGIN_STAGE("AST");

  if (compiler->args.log_flags & COMPILE_LOG_FLAGS_PARSE) {
    debug_print_ast(compiler->parser, &result.translation_unit);
  }

  disable_log();

  const struct target *target = get_target(&compiler->args);

  struct vector *external_symbols =
      vector_create(sizeof(struct external_symbol));

  // FIXME: super slow quadratic we really need a hashmap
  for (size_t i = 0; i < result.translation_unit.num_decl_lists; i++) {
    struct ast_decllist *decl_list = &result.translation_unit.decl_lists[i];

    for (size_t j = 0; j < decl_list->num_decls; j++) {
      struct ast_decl *decl = &decl_list->decls[j];
      const char *decl_name =
          identifier_str(compiler->parser, &decl->var.identifier);


      bool defined = decl->var.var_ty.ty != AST_TYREF_TY_FUNC && !(decl_list->storage_class_specifiers & AST_STORAGE_CLASS_SPECIFIER_FLAG_EXTERN);

      if (!defined) {
        // FIXME: hashtbl
        for (size_t k = 0; k < result.translation_unit.num_func_defs; k++) {
          struct ast_funcdef *def = &result.translation_unit.func_defs[k];
          const char *def_name =
              identifier_str(compiler->parser, &def->identifier);

          if (strcmp(decl_name, def_name) == 0) {
            defined = true;
            break;
          }
        }
      }

      // defined symbols are pushed back after creation because only then do we
      // know their position
      if (!defined) {
        const char *mangled = target->mangle(compiler->arena, decl_name);
        struct external_symbol symbol = {.name = mangled};
        vector_push_back(external_symbols, &symbol);
      }
    }
  }

  struct vector *compiled_functions =
      vector_create(sizeof(struct emitted_unit));

  if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_IR)) {
    enable_log();
  }

  BEGIN_STAGE("IR BUILD");

  struct ir_unit *ir = build_ir_for_translationunit(
      compiler->parser, compiler->arena, &result.translation_unit);

  {
    BEGIN_STAGE("IR");

    if (compiler->args.log_flags & COMPILE_LOG_FLAGS_IR) {
      debug_print_stage(ir, "ir");
    }

    BEGIN_STAGE("LOWERING");

    if (target->lower) {
      target->lower(ir);
    }

    BEGIN_STAGE("POST PRE REG LOWER IR");

    if (compiler->args.log_flags & COMPILE_LOG_FLAGS_IR) {
      debug_print_stage(ir, "lower");
    }

    disable_log();
  }

  {
    if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_REGALLOC)) {
      enable_log();
    }


    BEGIN_STAGE("REGALLOC");
    for (size_t i = 0; i < ir->num_funcs; i++) {
      struct ir_builder *irb = ir->funcs[i];

      lsra_register_alloc(irb, target->reg_info);

    }

    if (compiler->args.log_flags & COMPILE_LOG_FLAGS_REGALLOC) {
      debug_print_stage(ir, "regalloc");
    }

    for (size_t i = 0; i < ir->num_funcs; i++) {
      struct ir_builder *irb = ir->funcs[i];

      BEGIN_STAGE("ELIM PHI");

      eliminate_phi(irb);
    }

    if (compiler->args.log_flags & COMPILE_LOG_FLAGS_REGALLOC) {
      debug_print_stage(ir, "elim_phi");
    }
  }

  if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_EMIT)) {
    enable_log();
  }

  BEGIN_STAGE("PRE-EMIT");

  if (compiler->args.log_flags & COMPILE_LOG_FLAGS_PRE_EMIT) {
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

  struct build_object_args args = {
      .compile_args = &compiler->args,
      .output = compiler->output,
      .entries = unit.entries,
      .num_entries = unit.num_entries,
      .extern_symbols = vector_head(external_symbols),
      .num_extern_symbols = vector_length(external_symbols)};

  BEGIN_STAGE("OBJECT FILE");

  target->build_object(&args);

  vector_free(&compiled_functions);

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
