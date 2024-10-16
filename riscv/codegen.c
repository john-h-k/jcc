#include "codegen.h"
#include "../vector.h"

const char *eep_mangle(struct arena_allocator *arena, const char *name) {
  UNUSED_ARG(arena);
  return name;
}


struct eep_prologue_info {
  bool prologue_generated;
  size_t stack_size;
  size_t lr_offset;
  size_t save_start;
  unsigned long long saved_registers;
};

struct codegen_state {
  struct codegen_function *func;
  struct ir_builder *ir;
  struct eep_prologue_info prologue_info;

  size_t call_saves_start;
  size_t total_call_saves_size;

  size_t max_variadic_args;

  struct vector *strings;
  struct vector *datas;
};


struct codegen_unit *eep_codegen(struct ir_unit *ir) {
  struct codegen_unit *unit = arena_alloc(ir->arena, sizeof(*unit));
  *unit = (struct codegen_unit){
      .ty = CODEGEN_UNIT_TY_EEP,
      .instr_size = sizeof(struct eep_instr),
      .num_entries = ir->num_globals,
      .entries = arena_alloc(ir->arena,
                             ir->num_globals * sizeof(struct codeen_entry *))};

  arena_allocator_create(&unit->arena);

  struct ir_glb *glb = ir->first_global;
  size_t i = 0;
  while (glb) {
    if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
      unit->entries[i] =
          (struct codegen_entry){.ty = CODEGEN_ENTRY_TY_DECL,
                                 .glb_id = glb->id,
                                 .name = eep_mangle(ir->arena, glb->name)};

      i++;
      glb = glb->succ;
      continue;
    }

    switch (glb->ty) {
    case IR_GLB_TY_DATA: {
      // TODO: non string literals

      const char *name = glb->name
                             ? eep_mangle(ir->arena, glb->name)
                             : mangle_str_cnst_name(ir->arena, "todo", glb->id);
      switch (glb->var->ty) {
      case IR_VAR_TY_STRING_LITERAL:
        unit->entries[i] =
            (struct codegen_entry){.ty = CODEGEN_ENTRY_TY_STRING,
                                   .glb_id = glb->id,
                                   .name = name,
                                   .str = glb->var->value.str_value};
        break;
      case IR_VAR_TY_CONST_DATA:
        unit->entries[i] =
            (struct codegen_entry){.ty = CODEGEN_ENTRY_TY_CONST_DATA,
                                   .glb_id = glb->id,
                                   .name = name,
                                   .data = codegen_var_data(ir, glb->var)};
        break;
      case IR_VAR_TY_DATA:
        unit->entries[i] =
            (struct codegen_entry){.ty = CODEGEN_ENTRY_TY_DATA,
                                   .glb_id = glb->id,
                                   .name = name,
                                   .data = codegen_var_data(ir, glb->var)};
        break;
      }
      break;
    }
    case IR_GLB_TY_FUNC: {
      struct ir_builder *ir_func = glb->func;

      clear_metadata(ir_func);

      size_t total_call_saves_size = 0;
      size_t max_variadic_args = 0;

      struct ir_basicblock *basicblock = ir_func->first;
      while (basicblock) {
        struct ir_stmt *stmt = basicblock->first;

        while (stmt) {
          struct ir_op *op = stmt->first;

          while (op) {
            if (op->ty == IR_OP_TY_CALL) {
              if (is_func_variadic(&op->call.func_ty.func)) {
                size_t num_variadic_args =
                    op->call.num_args -
                    op->call.target->var_ty.func.num_params + 1;

                max_variadic_args = MAX(max_variadic_args, num_variadic_args);
              }

              // we need to save registers in-use _after_ call
              struct ir_op *succ = op->succ;
              if (!succ && op->stmt->succ) {
                // call is end of stmt, get live from next stmt
                // a call can not be the final op of the final stmt of a
                // basicblock as that must be a br/ret
                succ = op->stmt->succ->first;
              }

              size_t num_live =
                  popcntl(succ->live_fp_regs) + popcntl(succ->live_gp_regs);
              // FIXME: we naively assume we save 8 bytes
              // this over saves for smaller data types and breaks with 128 bit
              // vectors/floats
              total_call_saves_size = MAX(total_call_saves_size, num_live * 8);
            }

            op = op->succ;
          }

          stmt = stmt->succ;
        }

        basicblock = basicblock->succ;
      }

      unit->entries[i] = (struct codegen_entry){
          .ty = CODEGEN_ENTRY_TY_FUNC,
          .glb_id = glb->id,
          .name = eep_mangle(ir->arena, ir_func->name),
          .func = {
              .unit = unit, .first = NULL, .last = NULL, .instr_count = 0}};

      struct codegen_function *func = &unit->entries[i].func;
      struct codegen_state state = {.func = func,
                                    .ir = ir_func,
                                    .max_variadic_args = max_variadic_args,
                                    .total_call_saves_size =
                                        total_call_saves_size,
                                    .strings = vector_create(sizeof(char *)),
                                    .datas = vector_create(sizeof(char *))};

      insert_prologue(&state);

      basicblock = ir_func->first;
      while (basicblock) {
        struct instr *first_pred = func->last;

        struct ir_stmt *stmt = basicblock->first;

        while (stmt) {
          codegen_stmt(&state, stmt);

          stmt = stmt->succ;
        }

        basicblock->first_instr = first_pred ? first_pred->succ : func->first;
        basicblock->last_instr = func->last;

        basicblock = basicblock->succ;
      }

      // codegen is now done
      // do some basic sanity checks
      struct check_reg_type_data data = {.last = NULL, .reg_ty = 0};
      walk_regs(state.func, check_reg_type_callback, &data);

      break;
    }
    }

    i++;
    glb = glb->succ;
  }

  qsort(unit->entries, unit->num_entries, sizeof(struct codegen_entry),
        sort_entries_by_id);

  if (log_enabled()) {
    eep_debug_print_codegen(stderr, unit);
  }

  return unit;
}
