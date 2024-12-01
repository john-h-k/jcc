#include "codegen.h"

#include "../alloc.h"
#include "../log.h"
#include "../vector.h"

#define MOV_ALIAS(dest_reg, source_reg)                                        \
  (struct rv32i_instr) {                                                       \
    .ty = RV32I_INSTR_TY_ADD, .add = {                                         \
      .lhs = zero_reg_for_ty(dest_reg.ty),                                     \
      .rhs = (source_reg),                                                     \
      .dest = (dest_reg),                                                      \
    }                                                                          \
  }

static const char *rv32i_mangle(struct arena_allocator *arena,
                                const char *name) {
  char *dest =
      arena_alloc(arena, strlen(name) + /* null terminator + '_' char */ 2);

  dest[0] = '_';
  strcpy(dest + 1, name);

  return dest;
}

static struct rv32i_reg zero_reg_for_ty(enum rv32i_reg_ty reg_ty) {
  return (struct rv32i_reg){.ty = reg_ty, .idx = 0};
}

struct rv32i_prologue_info {
  bool prologue_generated;
  size_t stack_size;
  size_t lr_offset;
  size_t save_start;
  unsigned long long saved_gp_registers;
  unsigned long long saved_fp_registers;
};

struct codegen_state {
  struct arena_allocator *arena;

  struct codegen_function *func;
  struct ir_func *ir;
  struct rv32i_prologue_info prologue_info;

  size_t stack_args_size;
};

static struct rv32i_reg return_reg_for_ty(enum rv32i_reg_ty reg_ty) {
  return (struct rv32i_reg){.ty = reg_ty, .idx = 10};
}

static enum rv32i_reg_ty reg_ty_for_var_ty(const struct ir_var_ty *var_ty) {
  switch (var_ty->primitive) {
  case IR_VAR_PRIMITIVE_TY_I8:
  case IR_VAR_PRIMITIVE_TY_I16:
  case IR_VAR_PRIMITIVE_TY_I32:
    return RV32I_REG_TY_GP;
  case IR_VAR_PRIMITIVE_TY_I64:
    todo("i64");
  case IR_VAR_PRIMITIVE_TY_F16:
  case IR_VAR_PRIMITIVE_TY_F32:
  case IR_VAR_PRIMITIVE_TY_F64:
    return RV32I_REG_TY_FP;
  }
}

static size_t translate_reg_idx(size_t idx, enum ir_reg_ty ty) {
  switch (ty) {
  case IR_REG_TY_NONE:
  case IR_REG_TY_SPILLED:
  case IR_REG_TY_FLAGS:
    bug("does not make sense for none/spilled/flags");
  case IR_REG_TY_INTEGRAL:
    return idx + 1;
  case IR_REG_TY_FP:
    return idx >= 24 ? (idx - 24 + 8) : idx;
  }
}

static struct rv32i_reg codegen_reg(struct ir_op *op) {
  size_t idx = translate_reg_idx(op->reg.idx, op->reg.ty);

  if (op->var_ty.ty != IR_VAR_TY_TY_PRIMITIVE) {
    todo("non primitives (op %zu)", op->id);
  }

  enum rv32i_reg_ty reg_ty = reg_ty_for_var_ty(&op->var_ty);

  switch (reg_ty) {
  case RV32I_REG_TY_GP:
    invariant_assert(op->reg.ty == IR_REG_TY_INTEGRAL, "expected integral reg");
    break;
  case RV32I_REG_TY_FP:
    invariant_assert(op->reg.ty == IR_REG_TY_FP, "expected fp reg");
    break;
  default:
    todo("other reg tys (Q/V)");
  }

  return (struct rv32i_reg){.ty = reg_ty, .idx = idx};
}

static void codegen_mov_op(struct codegen_state *state, struct ir_op *op) {
  struct rv32i_reg dest = codegen_reg(op);

  struct rv32i_reg source = codegen_reg(op->mov.value);

  struct instr *instr = alloc_instr(state->func);
  // if (rv32i_reg_ty_is_gp(source.ty) && rv32i_reg_ty_is_gp(dest.ty)) {
  *instr->rv32i = MOV_ALIAS(dest, source);
  // } else {
  //   todo("");
  // }
}

static void codegen_br_cond_op(struct codegen_state *state, struct ir_op *op) {
  todo("cond br");
  // struct instr *instr = alloc_instr(state->func);

  // // rv32i requires turning `br.cond <true> <false>` into 2 instructions
  // // we represent this as just the `true` part of the `br.cond`, and then a
  // `br`
  // // after branching to the false target

  // struct ir_basicblock *true_target =
  // op->stmt->basicblock->split.true_target; struct ir_basicblock *false_target
  // = op->stmt->basicblock->split.false_target;

  // if (op->br_cond.cond->reg.ty == IR_REG_TY_FLAGS) {
  //   // emit based on flags
  //   enum rv32i_cond cond = get_cond_for_op(op->br_cond.cond);
  //   instr->rv32i->ty = rv32i_INSTR_TY_B_COND;
  //   instr->rv32i->b_cond = (struct rv32i_conditional_branch){
  //       .cond = cond, .target = true_target};
  // } else {
  //   struct rv32i_reg cmp_reg = codegen_reg(op->br_cond.cond);

  //   instr->rv32i->ty = rv32i_INSTR_TY_CBNZ;
  //   instr->rv32i->cbnz = (struct rv32i_compare_and_branch){
  //       .cmp = cmp_reg, .target = true_target};
  // }

  // // now generate the `br`
  // struct instr *br = alloc_instr(state->func);
  // br->rv32i->ty = rv32i_INSTR_TY_B;
  // br->rv32i->b = (struct rv32i_branch){.target = false_target};
}

static void codegen_br_op(struct codegen_state *state, struct ir_op *op) {
  todo("br");
  // struct instr *instr = alloc_instr(state->func);
}

static void codegen_cnst_op(struct codegen_state *state, struct ir_op *op) {
  debug_assert(op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE,
               "expects primitive type");

  struct rv32i_reg dest = codegen_reg(op);

  switch (op->cnst.ty) {
  case IR_OP_CNST_TY_FLT:
    // currently all constants are lowered to an integer load and `fmov`
    // but lots of constants can be loaded directly, so do that here
    todo("simple float constants (not lowered)");
  case IR_OP_CNST_TY_INT:
    switch (op->var_ty.primitive) {
    case IR_VAR_PRIMITIVE_TY_I8:
    case IR_VAR_PRIMITIVE_TY_I16:
    case IR_VAR_PRIMITIVE_TY_I32: {
      unsigned long long cnst = op->cnst.int_value;
      if (cnst >> 12) {
        struct instr *instr = alloc_instr(state->func);
        instr->rv32i->ty = RV32I_INSTR_TY_LUI;
        instr->rv32i->lui = (struct rv32i_lui){.dest = dest, .imm = cnst >> 12};
      }

      struct instr *instr = alloc_instr(state->func);
      instr->rv32i->ty = RV32I_INSTR_TY_ADDI;
      instr->rv32i->addi =
          (struct rv32i_addsub_imm){.dest = dest,
                                    .source = zero_reg_for_ty(dest.ty),
                                    .imm = cnst & ((1ul << 12) - 1)};

      break;
    }
    case IR_VAR_PRIMITIVE_TY_I64:
      todo("i64");
    case IR_VAR_PRIMITIVE_TY_F16:
    case IR_VAR_PRIMITIVE_TY_F32:
    case IR_VAR_PRIMITIVE_TY_F64:
      unreachable();
    };
  }
}

static void codegen_ret_op(struct codegen_state *state, struct ir_op *op) {
  if (op->ret.value && op->ret.value->ty != IR_OP_TY_CALL) {
    struct rv32i_reg source = codegen_reg(op->ret.value);

    if (source.idx != return_reg_for_ty(source.ty).idx) {
      struct instr *instr = alloc_instr(state->func);
      *instr->rv32i = MOV_ALIAS(return_reg_for_ty(source.ty), source);
    }
  }

  // codegen_epilogue(state);

  struct instr *instr = alloc_instr(state->func);

  instr->rv32i->ty = RV32I_INSTR_TY_JALR;
  instr->rv32i->jalr =
      (struct rv32i_jalr){.ret_addr = zero_reg_for_ty(RV32I_REG_TY_GP),
                          .target = RET_PTR_REG,
                          .imm = 0};
}

static void codegen_op(struct codegen_state *state, struct ir_op *op) {
  trace("lowering op with id %zu, type %d", op->id, op->ty);
  switch (op->ty) {
  case IR_OP_TY_UNDF:
  case IR_OP_TY_PHI:
    break;
  case IR_OP_TY_CUSTOM: {
    bug("custom");
  }
  case IR_OP_TY_MOV: {
    if (op->flags & IR_OP_FLAG_PARAM) {
      // don't need to do anything
    } else {
      codegen_mov_op(state, op);
    }
    break;
  }
  case IR_OP_TY_LOAD_GLB:
  case IR_OP_TY_STORE_GLB: {
    bug("load/store glb should have been lowered");
  }
  case IR_OP_TY_LOAD_LCL: {
    todo("");
    // codegen_load_lcl_op(state, op);
    // break;
  }
  case IR_OP_TY_STORE_LCL: {
    todo("");
    // codegen_store_lcl_op(state, op);
    // break;
  }
  case IR_OP_TY_LOAD_ADDR: {
    todo("");
    // codegen_load_addr_op(state, op);
    // break;
  }
  case IR_OP_TY_STORE_ADDR: {
    todo("");
    // codegen_store_addr_op(state, op);
    // break;
  }
  case IR_OP_TY_ADDR: {
    todo("");
    // codegen_addr_op(state, op);
    // break;
  }
  case IR_OP_TY_BR_COND: {
    codegen_br_cond_op(state, op);
    break;
  }
  case IR_OP_TY_BR: {
    codegen_br_op(state, op);
    break;
  }
  case IR_OP_TY_CNST: {
    codegen_cnst_op(state, op);
    break;
  }
  case IR_OP_TY_UNARY_OP: {
    todo("");
    // codegen_unary_op(state, op);
    // break;
  }
  case IR_OP_TY_BINARY_OP: {
    todo("");
    // codegen_binary_op(state, op);
    // break;
  }
  case IR_OP_TY_CAST_OP: {
    todo("");
    // codegen_cast_op(state, op);
    // break;
  }
  case IR_OP_TY_CALL: {
    todo("");
    // codegen_call_op(state, op);
    // break;
  }
  case IR_OP_TY_RET: {
    codegen_ret_op(state, op);
    break;
  }
  default: {
    todo("unsupported IR OP '%d'", op->ty);
  }
  }
}

static void codegen_stmt(struct codegen_state *state,
                         const struct ir_stmt *stmt) {
  struct ir_op *op = stmt->first;
  while (op) {
    if (!(op->flags & IR_OP_FLAG_CONTAINED)) {
      codegen_op(state, op);
    }

    op = op->succ;
  }
}

static int sort_entries_by_id(const void *a, const void *b) {
  const struct codegen_entry *l = a;
  const struct codegen_entry *r = b;

  if (l->glb_id > r->glb_id) {
    return 1;
  } else if (l->glb_id == r->glb_id) {
    return 0;
  } else {
    return -1;
  }
}

struct codegen_unit *rv32i_codegen(struct ir_unit *ir) {
  struct codegen_unit *unit = arena_alloc(ir->arena, sizeof(*unit));
  *unit = (struct codegen_unit){
      .ty = CODEGEN_UNIT_TY_RV32I,
      .instr_size = sizeof(struct rv32i_instr),
      .num_entries = ir->num_globals,
      .entries = arena_alloc(ir->arena,
                             ir->num_globals * sizeof(struct codeen_entry *))};

  arena_allocator_create(&unit->arena);

  struct ir_glb *glb = ir->first_global;

  {
    size_t i = 0;
    while (glb) {
      if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
        unit->entries[i] =
            (struct codegen_entry){.ty = CODEGEN_ENTRY_TY_DECL,
                                   .glb_id = glb->id,
                                   .name = rv32i_mangle(ir->arena, glb->name)};

        i++;
        glb = glb->succ;
        continue;
      }

      switch (glb->ty) {
      case IR_GLB_TY_DATA: {
        // TODO: non string literals

        const char *name =
            glb->name ? rv32i_mangle(ir->arena, glb->name)
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
          todo("");
          // unit->entries[i] =
          //     (struct codegen_entry){.ty = CODEGEN_ENTRY_TY_CONST_DATA,
          //                            .glb_id = glb->id,
          //                            .name = name,
          //                            .data = codegen_var_data(ir, glb->var)};
          // break;
        case IR_VAR_TY_DATA:
          todo("");
          // unit->entries[i] =
          //     (struct codegen_entry){.ty = CODEGEN_ENTRY_TY_DATA,
          //                            .glb_id = glb->id,
          //                            .name = name,
          //                            .data = codegen_var_data(ir, glb->var)};
          // break;
        }
        break;
      }
      case IR_GLB_TY_FUNC: {
        struct ir_func *ir_func = glb->func;

        clear_metadata(ir_func);

        unit->entries[i] = (struct codegen_entry){
            .ty = CODEGEN_ENTRY_TY_FUNC,
            .glb_id = glb->id,
            .name = rv32i_mangle(ir->arena, ir_func->name),
            .func = {
                .unit = unit, .first = NULL, .last = NULL, .instr_count = 0}};

        struct arena_allocator *arena;
        arena_allocator_create(&arena);

        struct codegen_function *func = &unit->entries[i].func;
        struct codegen_state state = {
            .arena = arena, .func = func, .ir = ir_func};

        state.stack_args_size = 0;

        struct ir_basicblock *basicblock = ir_func->first;
        while (basicblock) {
          struct ir_stmt *stmt = basicblock->first;

          while (stmt) {
            struct ir_op *op = stmt->first;

            while (op) {
              if (op->ty == IR_OP_TY_CALL) {
                todo("");
                // size_t call_args_size =
                //     calc_arg_stack_space(&state, op->call.func_ty.func, op);
                // state.stack_args_size =
                //     MAX(state.stack_args_size, call_args_size);
              }

              op = op->succ;
            }

            stmt = stmt->succ;
          }

          basicblock = basicblock->succ;
        }

        // codegen_prologue(&state);
        // codegen_params(&state);

        func->prologue = state.prologue_info.prologue_generated;
        func->stack_size = state.prologue_info.stack_size;

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

        break;
      }
      }

      i++;
      glb = glb->succ;
    }
  }

  qsort(unit->entries, unit->num_entries, sizeof(struct codegen_entry),
        sort_entries_by_id);

  if (log_enabled()) {
    rv32i_debug_print_codegen(stderr, unit);
  }

  // now do sanity checks
  // for (size_t i = 0; i < unit->num_entries; i++) {
  //   const struct codegen_entry *entry = &unit->entries[i];

  //   if (entry->ty == CODEGEN_ENTRY_TY_FUNC) {
  //     // codegen is now done
  //     // do some basic sanity checks
  //     struct check_reg_type_data data = {
  //         .entry = entry, .last = NULL, .reg_ty = 0};
  //     walk_regs(&entry->func, check_reg_type_callback, &data);
  //   }
  // }

  return unit;
}

static void debug_print_instr(FILE *file,
                              UNUSED_ARG(const struct codegen_function *func),
                              const struct instr *instr) {

  switch (instr->rv32i->ty) {
  case RV32I_INSTR_TY_ADDI:
    fprintf(file, "addi x%zu, x%zu, %llu", instr->rv32i->addsub_imm.dest.idx, instr->rv32i->addsub_imm.source.idx, instr->rv32i->addsub_imm.imm);
    break;
  case RV32I_INSTR_TY_ADD:
    fprintf(file, "add x%zu, x%zu, x%zu", instr->rv32i->addsub_reg.dest.idx, instr->rv32i->addsub_reg.lhs.idx, instr->rv32i->addsub_reg.rhs.idx);
    break;
  case RV32I_INSTR_TY_LUI:
    fprintf(file, "lui x%zu, %llu", instr->rv32i->lui.dest.idx, instr->rv32i->lui.imm);
    break;
  case RV32I_INSTR_TY_JALR:
    fprintf(file, "jalr x%zu, x%zu, %llu", instr->rv32i->jalr.ret_addr.idx, instr->rv32i->jalr.target.idx, instr->rv32i->jalr.imm);
    break;
  }
}

void rv32i_debug_print_codegen(FILE *file, struct codegen_unit *unit) {
  debug_assert(unit->ty == CODEGEN_UNIT_TY_RV32I, "expected rv32i");

  for (size_t i = 0; i < unit->num_entries; i++) {
    struct codegen_entry *entry = &unit->entries[i];

    if (entry->ty != CODEGEN_ENTRY_TY_FUNC) {
      continue;
    }

    struct codegen_function *func = &entry->func;

    fprintf(file, "\nFUNCTION: %s\n", entry->name);
    fprintf(file, "  prologue: %s\n", entry->func.prologue ? "true" : "false");
    fprintf(file, "  stack_size: %zu\n", entry->func.stack_size);
    fprintf(file, "\n");

    size_t offset = 0;
    struct instr *instr = func->first;
    while (instr) {
      fprintf(file, "%04zu: ", offset++);
      debug_print_instr(file, func, instr);
      fprintf(file, "\n");

      instr = instr->succ;
    }

    fprintf(file, "\n");
  }
}
