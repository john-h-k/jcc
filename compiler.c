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

const char *mangle_str_cnst_name(struct arena_allocator *arena, const char *func_name, size_t id) {
  size_t func_name_len = strlen(func_name);

  size_t len = 0;
  len += func_name_len;
  len += 2; // surround function name with `<>` so it cannot conflict with real names

  size_t id_len = num_digits(id);
  len += id_len;
  len += 1; // null char

  char *buff = arena_alloc(arena, len);
  size_t head = 0;
  buff[head++] = '<';
  strcpy(&buff[head], func_name);
  head += func_name_len;
  buff[head++] = '>';

  size_t tail = head + id_len - 1;
  while (tail >= head) {
    buff[tail--] = (id % 10) + '0';
    id /= 10;
  }

  head += id_len;
  buff[head++] = 0;

  debug_assert(head == len, "head (%zu) != len (%zu) in mangle_str_cnst_name", head, len);
 
  return buff;
}


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

  const struct target *target = get_target(&compiler->args);

  struct vector *symbols = vector_create(sizeof(struct symbol));
  struct vector *external_symbols = vector_create(sizeof(struct external_symbol));
  struct vector *relocations = vector_create(sizeof(struct relocation));

  // FIXME: super slow quadratic we really need a hashmap
  for (size_t i = 0; i < result.translation_unit.num_func_decls; i++) {
    struct ast_funcdecl *decl = &result.translation_unit.func_decls[i];
    const char *decl_name = identifier_str(compiler->parser, &decl->sig.name);

    bool defined = false;
    for (size_t j = 0; j < result.translation_unit.num_func_defs; j++) {
      struct ast_funcdef *def = &result.translation_unit.func_defs[j];
      const char *def_name = identifier_str(compiler->parser, &def->sig.name);

      if (strcmp(decl_name, def_name) == 0) {
        defined = true;
        break;
      }
    }

    // defined symbols are pushed back after creation because only then do we know their position
    if (!defined) {
      const char *mangled = target->mangle(compiler->arena, decl_name);
      struct external_symbol symbol = { .name = mangled };
      vector_push_back(external_symbols, &symbol);
    }
  }

  struct vector *compiled_functions =
      vector_create(sizeof(struct compiled_function));

  size_t total_size = 0;

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

      BEGIN_STAGE("PRE REG LOWERING");

      if (target->pre_reg_lower) {
        target->pre_reg_lower(irb);
      }

      BEGIN_STAGE("POST PRE REG LOWER IR");

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

      lsra_register_alloc(irb, target->reg_info);

      if (compiler->args.log_flags & COMPILE_LOG_FLAGS_REGALLOC) {
        debug_print_stage(irb, "reg_alloc");
      }

      BEGIN_STAGE("ELIM PHI");
      eliminate_phi(irb);

      if (compiler->args.log_flags & COMPILE_LOG_FLAGS_REGALLOC) {
        debug_print_stage(irb, "elim_phi");
      }

      BEGIN_STAGE("POST REG LOWERING");

      if (target->pre_reg_lower) {
        target->post_reg_lower(irb);
      }

      rebuild_ids(irb);

      BEGIN_STAGE("POST POST REG LOWER IR");
      
      if (compiler->args.log_flags & COMPILE_LOG_FLAGS_REGALLOC) {
        debug_print_stage(irb, "post_lower");
      }

      disable_log();
    }
  }

  // okay, we've now done everything except actually emit
  // here we calculate the offsets for all the functions so we can emit calls correctly
  size_t offset = 0;
  for (size_t i = 0; i < ir->num_funcs; i++) {
    struct ir_builder *irb = ir->funcs[i];

    irb->offset = offset;
    offset += irb->op_count;

    offset = ROUND_UP(offset, target->function_alignment / target->op_size);
  }
  
  struct vector *strings = vector_create(sizeof(const char *));
  for (size_t i = 0; i < ir->num_funcs; i++) {
    struct ir_builder *irb = ir->funcs[i];

    struct compiled_function func;
    {
      if (COMPILER_LOG_ENABLED(compiler, COMPILE_LOG_FLAGS_EMIT)) {
        enable_log();
      }

      BEGIN_STAGE("PRE-EMIT");

      debug_print_stage(irb, "pre_emit");

      BEGIN_STAGE("EMITTING");

      func = target->emit_function(irb);

      disable_log();
    }

    struct symbol symbol = {
        .ty = SYMBOL_TY_FUNC, .name = func.name, .value = total_size};

    total_size += ROUND_UP(func.len_code, target->function_alignment);
    vector_push_back(compiled_functions, &func);

    vector_push_back(symbols, &symbol);

    for (size_t i = 0; i < func.num_relocations; i++) {
      // add function position to relocs
      func.relocations[i].address += symbol.value;
    }

    vector_extend(relocations, func.relocations, func.num_relocations);
    vector_extend(strings, func.strings, func.num_strings);
  }

  char *code = arena_alloc(compiler->arena, total_size);
  char *head = code;

  size_t num_results = vector_length(compiled_functions);
  size_t str_offset = 0;
  for (size_t i = 0; i < num_results; i++) {
    struct compiled_function *func = vector_get(compiled_functions, i);
  
    for (ssize_t j = func->num_strings ? func->num_strings - 1 : -1; j >= 0; j--) {
      const char *str = func->strings[j];
      const char *name = mangle_str_cnst_name(compiler->arena, func->name, j);
      struct symbol symbol = { .ty = SYMBOL_TY_STRING, .name = name, .value = str_offset };
      printf("OFFSET %zu\n", str_offset);

      str_offset += strlen(str);
      vector_push_front(symbols, &symbol);
    }

    memcpy(head, func->code, func->len_code);
    head += ROUND_UP(func->len_code, target->function_alignment);
  }

  size_t l = vector_length(symbols);
  for (size_t i = 0; i < l; i++) {
    struct symbol *s = (struct symbol *)vector_get(symbols, i);
    printf("symbol %s\n", s->name);
  }

  struct build_object_args args = {
      .compile_args = &compiler->args,
      .output = compiler->output,
      .data = code,
      .len_data = total_size,
      .strings = vector_head(strings),
      .num_strings = vector_length(strings),
      .symbols = vector_head(symbols),
      .num_symbols = vector_length(symbols),
      .extern_symbols = vector_head(external_symbols),
      .num_extern_symbols = vector_length(external_symbols),
      .relocations = vector_head(relocations),
      .num_relocations = vector_length(relocations)
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
