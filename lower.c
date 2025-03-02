#include "lower.h"

#include "bit_twiddle.h"
#include "ir/ir.h"
#include "ir/prettyprint.h"
#include "util.h"
#include "vector.h"

static void remove_critical_edges(struct ir_func *irb) {
  // FIXME: i believe this doesn't properly propogate phis through the arms of a
  // switch expr. see lower.c

  struct ir_basicblock *basicblock = irb->first;

  while (basicblock) {
    size_t num_preds = basicblock->num_preds;

    struct ir_stmt *phi_stmt = NULL;
    if (basicblock->first && (basicblock->first->flags & IR_STMT_FLAG_PHI)) {
      phi_stmt = basicblock->first;
    }

    if (num_preds > 1) {
      for (size_t i = 0; i < num_preds; i++) {
        struct ir_basicblock *pred = basicblock->preds[i];

        if (pred->ty == IR_BASICBLOCK_TY_MERGE) {
          // not critical edge
          continue;
        }

        // we have a critical edge
        struct ir_basicblock *intermediate =
            insert_before_ir_basicblock(irb, basicblock);
        intermediate->ty = IR_BASICBLOCK_TY_MERGE;
        intermediate->merge =
            (struct ir_basicblock_merge){.target = basicblock};

        struct ir_stmt *br_stmt = alloc_ir_stmt(irb, intermediate);
        struct ir_op *op = alloc_ir_op(irb, br_stmt);
        op->ty = IR_OP_TY_BR;
        op->var_ty = IR_VAR_TY_NONE;

        basicblock->preds[i] = intermediate;

        add_pred_to_basicblock(irb, intermediate, pred);

        switch (pred->ty) {
        case IR_BASICBLOCK_TY_SPLIT:
          if (pred->split.true_target == basicblock) {
            pred->split.true_target = intermediate;
          } else {
            pred->split.false_target = intermediate;
          }
          break;
        case IR_BASICBLOCK_TY_SWITCH:
          for (size_t j = 0; j < pred->switch_case.num_cases; j++) {
            if (pred->switch_case.cases[j].target == basicblock) {
              pred->switch_case.cases[j].target = intermediate;
              break;
            }
          }
          break;
        case IR_BASICBLOCK_TY_MERGE:
          pred->merge.target = intermediate;
          break;
        case IR_BASICBLOCK_TY_RET:
          unreachable();
        }

        DEBUG_ASSERT(intermediate->num_preds == 1, "intermediate has >1 pred");

        if (phi_stmt) {
          struct ir_op *phi = phi_stmt->first;
          while (phi) {
            DEBUG_ASSERT(phi->ty == IR_OP_TY_PHI, "expected phi");

            struct ir_op *int_phi = insert_phi(irb, intermediate, phi->var_ty);
            int_phi->reg = phi->reg;
            int_phi->phi = (struct ir_op_phi){
                .num_values = 1,
                .values =
                    arena_alloc(irb->arena, sizeof(*int_phi->phi.values))};

            bool found = false;
            for (size_t j = 0; j < phi->phi.num_values; j++) {
              struct ir_phi_entry *entry = &phi->phi.values[j];

              if (entry->basicblock == pred) {
                int_phi->phi.values[0] = (struct ir_phi_entry){
                    .basicblock = pred, .value = entry->value};
                *entry = (struct ir_phi_entry){.basicblock = intermediate,
                                               .value = int_phi};
                found = true;
                break;
              }
            }

            DEBUG_ASSERT(found, "failed to gen phi");

            phi = phi->succ;
          }
        }
      }
    }

    basicblock = basicblock->succ;
  }
}

static void lower_call_registers(struct ir_func *func, struct ir_op *op);

// this is carefully chosen so that all types passed on the stack will generate
// inline code because else `lower_call` needs to deal with this function
// potentially generating calls _itself_ biggest thing passed on stack: 4
// element HVA of 16 byte vectors, so 64 but we do 128 anyway
// #define MEMMOVE_THRESHOLD 128
// disabled as we haven't implemented backend
#define MEMMOVE_THRESHOLD 0

void lower_store(struct ir_func *func, struct ir_op *op) {
  struct ir_op *source = op->store.value;

  const struct ir_var_ty *var_ty = &source->var_ty;

  if (var_ty_is_integral(var_ty) || var_ty_is_fp(var_ty)) {
    return;
  }

  struct ir_var_ty_info info = var_ty_info(func->unit, var_ty);

  if (source->ty != IR_OP_TY_LOAD) {
    BUG("non-primitive store occured out of a non-load op?");
  }

  struct ir_op *source_addr = build_addr(func, source);
  struct ir_op *dest_addr = build_addr(func, op);

  if (op->store.value->ty == IR_OP_TY_LOAD) {
    detach_ir_op(func, op->store.value);
  }

  if (info.size <= MEMMOVE_THRESHOLD) {
    op->ty = IR_OP_TY_MEM_COPY;
    op->mem_copy = (struct ir_op_mem_copy){
        .dest = dest_addr, .source = source_addr, .length = info.size};
    return;
  }

  struct ir_op *size =
      insert_before_ir_op(func, op, IR_OP_TY_CNST, IR_VAR_TY_NONE);
  mk_pointer_constant(func->unit, size, info.size);

  struct ir_glb *memmove =
      add_well_known_global(func->unit, IR_WELL_KNOWN_GLB_MEMMOVE);

  struct ir_var_ty ptr_int = var_ty_for_pointer_size(func->unit);
  struct ir_op *memmove_addr =
      insert_before_ir_op(func, op, IR_OP_TY_ADDR, ptr_int);
  memmove_addr->flags |= IR_OP_FLAG_CONTAINED;
  memmove_addr->addr = (struct ir_op_addr){
      .ty = IR_OP_ADDR_TY_GLB,
      .glb = memmove,
  };

  size_t num_args = 3;
  struct ir_op **args =
      arena_alloc(func->arena, sizeof(struct ir_op *) * num_args);

  args[0] = dest_addr;
  args[1] = source_addr;
  args[2] = size;

  func->flags |= IR_FUNC_FLAG_MAKES_CALL;
  op->ty = IR_OP_TY_CALL;
  op->var_ty = *memmove->var_ty.func.ret_ty;
  op->call = (struct ir_op_call){
      .target = memmove_addr,
      .num_args = num_args,
      .args = args,
      .func_ty = memmove->var_ty,
  };

  lower_call(func, op);
  lower_call_registers(func, op);
}

enum load_bitfield {
  LOAD_BITFIELD_MASK_IN,
  LOAD_BITFIELD_MASK_OUT,
};

static struct ir_op *get_unshifted_bitfield(struct ir_func *func,
                                            struct ir_op *op,
                                            struct ir_bitfield bitfield,
                                            enum load_bitfield load_bitfield) {
  unsigned int mask_val;

  switch (load_bitfield) {
  case LOAD_BITFIELD_MASK_IN:
    mask_val =
        ~MASK_OUT(unsigned, bitfield.width + bitfield.offset, bitfield.offset);
    break;
  case LOAD_BITFIELD_MASK_OUT:
    mask_val =
        MASK_OUT(unsigned, bitfield.width + bitfield.offset, bitfield.offset);
    break;
  }

  // printf("mask lo %zu = %u\n", offset, bitfield.width, MASK_HI(unsigned,
  // bitfield.width + bitfield.offset, bitfield.offset)); printf("mask hi %zu =
  // %u\n", bitfield.offset, bitfield.width, MASK_LO(unsigned, bitfield.width +
  // bitfield.offset, bitfield.offset)); bug("mask (%zu, %zu) = %u",
  // bitfield.offset, bitfield.width, mask_val);

  struct ir_op *mask_cnst =
      insert_after_ir_op(func, op, IR_OP_TY_CNST, IR_VAR_TY_I32);
  mask_cnst->cnst =
      (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = mask_val};

  struct ir_op *mask =
      insert_after_ir_op(func, mask_cnst, IR_OP_TY_BINARY_OP, IR_VAR_TY_I32);
  mask->binary_op = (struct ir_op_binary_op){
      .ty = IR_OP_BINARY_OP_TY_AND, .lhs = op, .rhs = mask_cnst};

  return mask;
}

// TODO: signs and stuff are wrong
void lower_bitfield_insert(struct ir_func *func, struct ir_op *op) {
  struct ir_op_bitfield_insert *insert = &op->bitfield_insert;
  struct ir_bitfield bitfield = op->store_bitfield.bitfield;

  struct ir_op *masked_out = get_unshifted_bitfield(
      func, insert->target, bitfield, LOAD_BITFIELD_MASK_OUT);

  struct ir_op *shifted_op;
  if (bitfield.offset) {
    struct ir_op *shift_cnst =
        insert_after_ir_op(func, insert->target, IR_OP_TY_CNST, IR_VAR_TY_I32);
    shift_cnst->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT,
                                           .int_value = bitfield.offset};
    shifted_op =
        insert_after_ir_op(func, shift_cnst, IR_OP_TY_BINARY_OP, IR_VAR_TY_I32);
    shifted_op->binary_op =
        (struct ir_op_binary_op){.ty = IR_OP_BINARY_OP_TY_LSHIFT,
                                 .lhs = insert->value,
                                 .rhs = shift_cnst};
  } else {
    shifted_op = insert->value;
  }

  struct ir_op *mask_in =
      replace_ir_op(func, op, IR_OP_TY_BINARY_OP, IR_VAR_TY_I32);

  mask_in->binary_op = (struct ir_op_binary_op){
      .ty = IR_OP_BINARY_OP_TY_OR, .lhs = masked_out, .rhs = shifted_op};
}

void lower_bitfield_extract(struct ir_func *func, struct ir_op *op) {
  struct ir_op_bitfield_extract *extract = &op->bitfield_extract;
  struct ir_bitfield bitfield = op->load_bitfield.bitfield;

  struct ir_op *masked_in = get_unshifted_bitfield(
      func, extract->value, bitfield, LOAD_BITFIELD_MASK_IN);

  if (bitfield.offset) {
    struct ir_op *shift_cnst =
        insert_after_ir_op(func, masked_in, IR_OP_TY_CNST, IR_VAR_TY_I32);
    shift_cnst->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT,
                                           .int_value = bitfield.offset};
    op->ty = IR_OP_TY_BINARY_OP;
    op->var_ty = IR_VAR_TY_I32;
    op->binary_op = (struct ir_op_binary_op){
        .ty = IR_OP_BINARY_OP_TY_URSHIFT, .lhs = masked_in, .rhs = shift_cnst};
  } else {
    // ugly
    op->ty = IR_OP_TY_MOV;
    op->var_ty = IR_VAR_TY_I32;
    op->mov = (struct ir_op_mov){.value = masked_in};
  }
}

static void propogate_switch_phis(UNUSED struct ir_func *func,
                                  struct ir_basicblock *bb_switch,
                                  struct ir_basicblock *pred_cond,
                                  struct ir_basicblock *basicblock) {
  // FIXME: this does NOT properly propogate
  // it needs to add phis to all intermediates too

  struct ir_stmt *phi_stmt = basicblock->first;

  struct ir_op *phi = phi_stmt ? phi_stmt->first : NULL;
  while (phi && phi->ty == IR_OP_TY_PHI) {
    for (size_t i = 0; i < phi->phi.num_values; i++) {
      if (phi->phi.values[i].basicblock == bb_switch) {
        phi->phi.values[i].basicblock = pred_cond;
      }
    }

    phi = phi->succ;
  }
}

static void lower_br_cond(struct ir_func *func, struct ir_op *op) {
  struct ir_op *cond = op->br_cond.cond;
  if (!var_ty_is_fp(&cond->var_ty)) {
    return;
  }

  if (cond->ty == IR_OP_TY_BINARY_OP &&
      binary_op_is_comparison(cond->binary_op.ty)) {
    return;
  }

  // turn `if (float)` into `if (float != 0.0)`

  struct ir_op *zero =
      insert_before_ir_op(func, op, IR_OP_TY_CNST, cond->var_ty);
  zero->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_FLT, .flt_value = 0};

  struct ir_op *neq =
      insert_before_ir_op(func, op, IR_OP_TY_BINARY_OP, IR_VAR_TY_I32);
  neq->binary_op = (struct ir_op_binary_op){
      .ty = IR_OP_BINARY_OP_TY_FNEQ, .lhs = cond, .rhs = zero};

  op->br_cond.cond = neq;
}

static void lower_mem_set(struct ir_func *func, struct ir_op *op) {
  struct ir_op *addr = op->mem_set.addr;

  struct ir_glb *memset =
      add_well_known_global(func->unit, IR_WELL_KNOWN_GLB_MEMSET);

  struct ir_op *value_cnst =
      insert_before_ir_op(func, op, IR_OP_TY_CNST, IR_VAR_TY_I32);
  value_cnst->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT,
                                         .int_value = op->mem_set.value};

  struct ir_var_ty ptr_int = var_ty_for_pointer_size(func->unit);

  struct ir_op *length_cnst =
      insert_after_ir_op(func, value_cnst, IR_OP_TY_CNST, ptr_int);
  length_cnst->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT,
                                          .int_value = op->mem_set.length};

  struct ir_op *memset_addr =
      insert_after_ir_op(func, length_cnst, IR_OP_TY_ADDR, ptr_int);
  memset_addr->flags |= IR_OP_FLAG_CONTAINED;
  memset_addr->addr = (struct ir_op_addr){
      .ty = IR_OP_ADDR_TY_GLB,
      .glb = memset,
  };

  size_t num_args = 3;
  struct ir_op **args =
      arena_alloc(func->arena, sizeof(struct ir_op *) * num_args);

  args[0] = addr;
  args[1] = value_cnst;
  args[2] = length_cnst;

  func->flags |= IR_FUNC_FLAG_MAKES_CALL;
  op->ty = IR_OP_TY_CALL;
  op->var_ty = *memset->var_ty.func.ret_ty;
  op->call = (struct ir_op_call){
      .target = memset_addr,
      .num_args = num_args,
      .args = args,
      .func_ty = memset->var_ty,
  };

  lower_call(func, op);
}

static void lower_mem_copy(struct ir_func *func, struct ir_op *op) {
  struct ir_op *dest = op->mem_copy.dest;
  struct ir_op *source = op->mem_copy.source;

  DEBUG_ASSERT(dest && source, "dest and source should be non null");

  struct ir_glb *memcopy =
      add_well_known_global(func->unit, IR_WELL_KNOWN_GLB_MEMCPY);

  struct ir_var_ty ptr_int = var_ty_for_pointer_size(func->unit);

  struct ir_op *length_cnst =
      insert_before_ir_op(func, op, IR_OP_TY_CNST, ptr_int);
  length_cnst->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT,
                                          .int_value = op->mem_copy.length};

  struct ir_op *memcopy_addr =
      insert_after_ir_op(func, length_cnst, IR_OP_TY_ADDR, ptr_int);

  memcopy_addr->flags |= IR_OP_FLAG_CONTAINED;
  memcopy_addr->addr = (struct ir_op_addr){
      .ty = IR_OP_ADDR_TY_GLB,
      .glb = memcopy,
  };

  size_t num_args = 3;
  struct ir_op **args =
      arena_alloc(func->arena, sizeof(struct ir_op *) * num_args);

  args[0] = dest;
  args[1] = source;
  args[2] = length_cnst;

  func->flags |= IR_FUNC_FLAG_MAKES_CALL;
  op->ty = IR_OP_TY_CALL;
  op->var_ty = *memcopy->var_ty.func.ret_ty;
  op->call = (struct ir_op_call){
      .target = memcopy_addr,
      .num_args = num_args,
      .args = args,
      .func_ty = memcopy->var_ty,
  };

  lower_call(func, op);
}

static void lower_br_switch(struct ir_func *func, struct ir_op *op) {
  // lowers a `br.switch` into a series of if-else statements

  struct ir_basicblock *bb = op->stmt->basicblock;
  struct ir_basicblock_switch *bb_switch = &bb->switch_case;

  struct ir_basicblock *prev_bb = op->stmt->basicblock;

  detach_ir_op(func, op);

  struct ir_var_ty var_ty = op->br_switch.value->var_ty;

  size_t num_cases = bb_switch->num_cases;
  struct ir_split_case *split_cases = bb_switch->cases;
  for (size_t i = 0; i < num_cases; i++) {
    struct ir_split_case *split_case = &split_cases[i];

    for (size_t j = 0; j < split_case->target->num_preds; j++) {
      if (split_case->target->preds[j] == bb) {
        // remove pred
        memmove(&split_case->target->preds[j],
                &split_case->target->preds[j + 1],
                (split_case->target->num_preds - j - 1) *
                    sizeof(struct ir_basicblock *));
        split_case->target->num_preds--;
      }
    }

    struct ir_stmt *cmp_stmt = alloc_ir_stmt(func, prev_bb);
    struct ir_op *cnst = alloc_ir_op(func, cmp_stmt);
    cnst->ty = IR_OP_TY_CNST;
    cnst->var_ty = var_ty;
    cnst->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT,
                                     .int_value = split_case->value};

    struct ir_op *cmp_op = alloc_ir_op(func, cmp_stmt);
    cmp_op->ty = IR_OP_TY_BINARY_OP;
    cmp_op->var_ty = IR_VAR_TY_I32;
    cmp_op->binary_op = (struct ir_op_binary_op){
        .ty = IR_OP_BINARY_OP_TY_EQ, .lhs = op->br_switch.value, .rhs = cnst};

    struct ir_op *br_op = alloc_ir_op(func, cmp_stmt);
    br_op->ty = IR_OP_TY_BR_COND;
    br_op->var_ty = IR_VAR_TY_NONE;
    br_op->br_cond = (struct ir_op_br_cond){.cond = cmp_op};

    if (i + 1 < num_cases) {
      struct ir_basicblock *next_cond =
          insert_after_ir_basicblock(func, prev_bb);

      propogate_switch_phis(func, bb, prev_bb, split_case->target);
      make_basicblock_split(func, prev_bb, split_case->target, next_cond);

      prev_bb = next_cond;
    } else {
      struct ir_basicblock *default_target = bb_switch->default_target;

      for (size_t j = 0; j < default_target->num_preds; j++) {
        if (bb_switch->default_target->preds[j] == bb) {
          // remove pred
          memmove(&default_target->preds[j], &default_target->preds[j + 1],
                  (default_target->num_preds - j - 1) *
                      sizeof(struct ir_basicblock *));
          default_target->num_preds--;
        }
      }

      propogate_switch_phis(func, bb, prev_bb, split_case->target);
      propogate_switch_phis(func, bb, prev_bb, bb_switch->default_target);
      make_basicblock_split(func, prev_bb, split_case->target,
                            bb_switch->default_target);
    }
  }
}

// TODO: signs and stuff are wrong
static void lower_store_bitfield(struct ir_func *func, struct ir_op *op) {
  struct ir_op *value = op->store_bitfield.value;
  struct ir_op *addr = build_addr(func, op);

  struct ir_bitfield bitfield = op->store_bitfield.bitfield;

  struct ir_op *load =
      insert_before_ir_op(func, op, IR_OP_TY_LOAD, value->var_ty);
  load->load = (struct ir_op_load){.ty = IR_OP_LOAD_TY_ADDR, .addr = addr};

  op->ty = IR_OP_TY_BITFIELD_INSERT;
  op->var_ty = value->var_ty;
  op->bitfield_insert = (struct ir_op_bitfield_insert){
      .value = value, .target = load, .bitfield = bitfield};

  struct ir_op *store =
      insert_after_ir_op(func, op, IR_OP_TY_STORE, IR_VAR_TY_NONE);
  store->store = (struct ir_op_store){
      .ty = IR_OP_STORE_TY_ADDR, .addr = addr, .value = op};
}

static void lower_load_bitfield(struct ir_func *func, struct ir_op *op) {
  struct ir_op *addr = build_addr(func, op);

  struct ir_bitfield bitfield = op->load_bitfield.bitfield;

  struct ir_op *load = insert_before_ir_op(func, op, IR_OP_TY_LOAD, op->var_ty);
  load->load = (struct ir_op_load){.ty = IR_OP_LOAD_TY_ADDR, .addr = addr};

  op->ty = IR_OP_TY_BITFIELD_EXTRACT;
  op->var_ty = op->var_ty;
  op->bitfield_extract =
      (struct ir_op_bitfield_extract){.value = load, .bitfield = bitfield};
}

static void lower_store_to_addr(struct ir_func *func, struct ir_op *op) {
  struct ir_op *addr = build_addr(func, op);

  op->store.ty = IR_OP_STORE_TY_ADDR;
  op->store.addr = addr;
}

static void lower_load_to_addr(struct ir_func *func, struct ir_op *op) {
  struct ir_op *addr = build_addr(func, op);

  op->load.ty = IR_OP_LOAD_TY_ADDR;
  op->load.addr = addr;
}

static void lower_pointers(struct ir_unit *unit) {
  // Final step: turn all TY_POINTER into TY_I64 as the information is no
  // longer needed
  struct ir_glb *glb = unit->first_global;
  while (glb) {
    if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
      glb = glb->succ;
      continue;
    }

    switch (glb->ty) {
    case IR_GLB_TY_DATA:
      break;
    case IR_GLB_TY_FUNC: {
      struct ir_func *func = glb->func;
      struct ir_basicblock *basicblock = func->first;
      while (basicblock) {
        struct ir_stmt *stmt = basicblock->first;

        while (stmt) {
          struct ir_op *op = stmt->first;

          while (op) {
            if (op->var_ty.ty == IR_VAR_TY_TY_POINTER) {
              op->var_ty = var_ty_for_pointer_size(func->unit);
            }

            op = op->succ;
          }

          stmt = stmt->succ;
        }

        basicblock = basicblock->succ;
      }
      break;
    }
    }

    glb = glb->succ;
  }
}

static struct ir_var_ty get_var_ty_for_size(enum ir_reg_ty reg_ty,
                                            size_t size) {
  if (reg_ty == IR_REG_TY_INTEGRAL) {
    switch (size) {
    case 1:
      return IR_VAR_TY_I8;
    case 2:
      return IR_VAR_TY_I16;
    case 4:
      return IR_VAR_TY_I32;
    case 8:
      return IR_VAR_TY_I64;
    default:
      unreachable();
    }
  } else {
    DEBUG_ASSERT(reg_ty == IR_REG_TY_FP, "expected integral or fp reg");

    switch (size) {
    case 2:
      return IR_VAR_TY_F16;
    case 4:
      return IR_VAR_TY_F32;
    case 8:
      return IR_VAR_TY_F64;
    default:
      unreachable();
    }
  }
}
static void lower_params(struct ir_func *func) {
  struct ir_call_info call_info = func->call_info;

  if (call_info.num_params) {
    struct ir_op *param_op = func->first->first->first;
    struct ir_stmt *stmt = insert_after_ir_stmt(func, func->first->first);

    for (size_t i = 0; i < call_info.num_params; i++) {
      DEBUG_ASSERT(param_op->flags & IR_OP_FLAG_PARAM, "expected param op");

      struct ir_param_info param_info = call_info.params[i];

      switch (param_info.ty) {
      case IR_PARAM_INFO_TY_REGISTER:
      case IR_PARAM_INFO_TY_STACK:
        if (var_ty_is_aggregate(param_info.var_ty)) {
          DEBUG_ASSERT(param_op->ty == IR_OP_TY_ADDR &&
                           param_op->addr.ty == IR_OP_ADDR_TY_LCL,
                       "expected addr");

          struct ir_lcl *lcl = param_op->addr.lcl;

          size_t num_reg = param_info.num_regs;
          struct ir_op *addr =
              append_ir_op(func, stmt, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
          addr->addr = (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = lcl};

          for (size_t j = 0; j < num_reg; j++) {
            struct ir_param_reg reg = param_info.regs[j];

            struct ir_var_ty store_ty =
                get_var_ty_for_size(reg.reg.ty, reg.size);

            struct ir_op *store =
                insert_after_ir_op(func, addr, IR_OP_TY_STORE, IR_VAR_TY_NONE);

            struct ir_op *addr_offset = insert_before_ir_op(
                func, store, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
            addr_offset->addr_offset = (struct ir_op_addr_offset){
                .base = addr, .offset = j * reg.size};

            struct ir_op *mov;
            if (j == 0) {
              mov = replace_ir_op(func, param_op, IR_OP_TY_MOV, store_ty);
            } else {
              mov = insert_after_ir_op(func, param_op, IR_OP_TY_MOV, store_ty);
            }
            mov->mov = (struct ir_op_mov){.value = NULL};
            mov->flags |= IR_OP_FLAG_PARAM;

            store->store = (struct ir_op_store){
                .ty = IR_OP_STORE_TY_ADDR, .addr = addr_offset, .value = mov};

            param_op = mov;
          }
        } else if (param_info.ty == IR_PARAM_INFO_TY_REGISTER) {
          DEBUG_ASSERT(param_info.num_regs == 1,
                       "expected 1 reg for non aggregate");
        }
        break;
      case IR_PARAM_INFO_TY_POINTER: {
        DEBUG_ASSERT(param_op->ty == IR_OP_TY_ADDR &&
                         param_op->addr.ty == IR_OP_ADDR_TY_LCL,
                     "expected addr");

        DEBUG_ASSERT(param_info.num_regs == 1,
                     "expected 1 reg for non aggregate");

        struct ir_op *param =
            replace_ir_op(func, param_op, IR_OP_TY_MOV, IR_VAR_TY_POINTER);
        param->mov = (struct ir_op_mov){.value = NULL};
        param->flags |= IR_OP_FLAG_PARAM;

        struct ir_func_iter iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);
        struct ir_lcl *lcl = param_op->addr.lcl;

        // FIXME: we mark the param as being ETERNAL
        // this is because we are using it potentially across BBs without
        // generating phis the fix is to use phis properly (potentially changing
        // IR build so it doesn't do the silly "struct param becomes local"
        // thing)

        struct ir_op *op;
        while (ir_func_iter_next(&iter, &op)) {
          if (op->ty == IR_OP_TY_ADDR && op->addr.ty == IR_OP_ADDR_TY_LCL &&
              op->addr.lcl == lcl) {
            op = replace_ir_op(func, op, IR_OP_TY_MOV, IR_VAR_TY_POINTER);
            op->mov = (struct ir_op_mov){.value = param};

            param->flags |= IR_OP_FLAG_ETERNAL;
          } else if (op->ty == IR_OP_TY_LOAD &&
                     op->load.ty == IR_OP_LOAD_TY_LCL && op->load.lcl == lcl) {
            op->load.ty = IR_OP_LOAD_TY_ADDR;
            op->load.addr = param;

            param->flags |= IR_OP_FLAG_ETERNAL;
          } else if (op->ty == IR_OP_TY_STORE &&
                     op->store.ty == IR_OP_STORE_TY_LCL &&
                     op->store.lcl == lcl) {
            op->store.ty = IR_OP_STORE_TY_ADDR;
            op->store.addr = param;

            param->flags |= IR_OP_FLAG_ETERNAL;
          }
        }

        break;
      }
      }

      // there is exactly one op per param
      param_op = param_op->succ;
    }
  }

  struct ir_op *first_param = NULL;

  if (func->call_info.ret &&
      func->call_info.ret->ty == IR_PARAM_INFO_TY_POINTER) {
    if (func->first && func->first->first && func->first->first->first) {
      first_param = insert_before_ir_op(func, func->first->first->first,
                                        IR_OP_TY_MOV, IR_VAR_TY_POINTER);
    } else {
      first_param = alloc_ir_op(func, func->first->first);
      first_param->ty = IR_OP_TY_MOV;
      first_param->var_ty = IR_VAR_TY_POINTER;
    }

    first_param->flags |= IR_OP_FLAG_PARAM;
    first_param->mov = (struct ir_op_mov){.value = NULL};
  }

  struct ir_func_iter iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);

  struct ir_op *op;
  while (ir_func_iter_next(&iter, &op)) {
    if (op->ty != IR_OP_TY_RET || op->ret.value == NULL) {
      continue;
    }

    if (!func->call_info.ret) {
      op->ret.value = NULL;
      continue;
    }

    struct ir_param_info param_info = *func->call_info.ret;

    struct ir_var_ty_flattened info =
        var_ty_info_flat(func->unit, param_info.var_ty);

    switch (param_info.ty) {
    case IR_PARAM_INFO_TY_REGISTER: {
      if (var_ty_is_aggregate(param_info.var_ty)) {
        DEBUG_ASSERT(op->ret.value->ty == IR_OP_TY_LOAD, "expected load");

        size_t num_reg = param_info.num_regs;

        struct ir_op *addr = build_addr(func, op->ret.value);

        struct ir_op *gather = insert_before_ir_op(func, op, IR_OP_TY_GATHER,
                                                   op->ret.value->var_ty);
        gather->gather = (struct ir_op_gather){
            .num_values = num_reg,
            .values = arena_alloc(func->arena,
                                  sizeof(*gather->gather.values) * num_reg)};

        struct ir_op *last = gather;

        size_t field_idx = 0;
        size_t offset = 0;

        for (size_t j = 0; j < num_reg; j++) {
          struct ir_param_reg reg = param_info.regs[j];

          size_t offset_add;

          struct ir_var_ty load_ty;
          size_t field_offset = info.fields[field_idx + 1].offset - offset;
          if (field_idx + 1 >= info.num_fields || field_offset >= reg.size) {
            // only loading one field, so load the field ty
            size_t field_size =
                var_ty_info(func->unit, &info.fields[field_idx].var_ty).size;
            load_ty = get_var_ty_for_size(reg.reg.ty, field_size);

            offset_add = field_offset;
          } else {
            load_ty = get_var_ty_for_size(reg.reg.ty, reg.size);
            offset_add = reg.size;
          }

          struct ir_op *load =
              insert_before_ir_op(func, last, IR_OP_TY_LOAD, load_ty);

          struct ir_op *addr_offset = insert_before_ir_op(
              func, load, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
          addr_offset->addr_offset = (struct ir_op_addr_offset){
              // FIXME: here and in other places, is this the right logic for
              // offset? or could reg size be an incorrect metric based on
              // lower
              .base = addr,
              .offset = offset};

          offset += offset_add;

          while (field_idx < info.num_fields &&
                 info.fields[field_idx].offset < offset) {
            field_idx++;
          }

          load->load = (struct ir_op_load){.ty = IR_OP_LOAD_TY_ADDR,
                                           .addr = addr_offset};
          last = load;

          // FIXME: `j != field_idx` because reg can contain multiple

          gather->gather.values[j] =
              (struct ir_gather_value){.value = load, .field_idx = j};
        }

        detach_ir_op(func, op->ret.value);
        op->ret.value = gather;
      } else {
        DEBUG_ASSERT(param_info.num_regs == 1,
                     "expected 1 reg for non aggregate");

        struct ir_op *mov =
            insert_before_ir_op(func, op, IR_OP_TY_MOV, op->ret.value->var_ty);
        mov->mov = (struct ir_op_mov){.value = op->ret.value};

        op->ret.value = mov;
      }
      break;
    }
    case IR_PARAM_INFO_TY_POINTER: {
      DEBUG_ASSERT(op->ret.value->ty == IR_OP_TY_LOAD, "expected load");

      struct ir_op *addr = build_addr(func, op->ret.value);

      struct ir_op *mem_copy =
          insert_before_ir_op(func, op, IR_OP_TY_MEM_COPY, IR_VAR_TY_NONE);

      DEBUG_ASSERT(first_param && addr, "unexpected null");
      mem_copy->mem_copy = (struct ir_op_mem_copy){
          .dest = first_param,
          .source = addr,
          .length = var_ty_info(func->unit, &op->ret.value->var_ty).size};

      detach_ir_op(func, op->ret.value);
      op->ret.value = NULL;
      break;
    }
    case IR_PARAM_INFO_TY_STACK:
      unreachable();
    }
  }
}

void lower_call(struct ir_func *func, struct ir_op *op) {
  struct ir_func_info func_info = func->unit->target->lower_func_ty(
      func, op->call.func_ty.func, op->call.args, op->call.num_args);

  struct vector *new_args = vector_create(sizeof(struct ir_op *));

  op->call.func_ty.func = func_info.func_ty;
  op->call.func_info = func_info;

  if (func_info.call_info.ret &&
      func_info.call_info.ret->ty == IR_PARAM_INFO_TY_POINTER) {
    // FIXME: don't use succ find usage
    struct ir_op *store = op->succ;
    DEBUG_ASSERT(store->ty == IR_OP_TY_STORE, "expected store in %s", func->name);

    // move store before op so the generated address is before the call
    detach_ir_op(func, store);
    attach_ir_op(func, store, op->stmt, op->pred, op);

    struct ir_op *addr = build_addr(func, store);

    vector_push_back(new_args, &addr);
    detach_ir_op(func, store);

    op->var_ty = IR_VAR_TY_NONE;
  }

  for (size_t i = 0; i < op->call.num_args; i++) {
    struct ir_op *arg = op->call.args[i];
    DEBUG_ASSERT(i < func_info.call_info.num_params, "out of range");
    struct ir_param_info param_info = func_info.call_info.params[i];

    switch (param_info.ty) {
    case IR_PARAM_INFO_TY_STACK: {
      vector_push_back(new_args, &arg);
      break;
    }

    case IR_PARAM_INFO_TY_REGISTER:
      if (arg->ty != IR_OP_TY_LOAD && param_info.num_regs == 1) {
        // no change needed
        vector_push_back(new_args, &arg);

      } else {
        bool detach = true;
        if (arg->ty != IR_OP_TY_LOAD) {
          arg = spill_op(func, arg);
          detach = false;
        }

        struct ir_op *addr = build_addr(func, arg);
        struct ir_op *last = op;

        for (size_t j = 0; j < param_info.num_regs; j++) {
          struct ir_param_reg reg = param_info.regs[j];

          struct ir_var_ty load_ty = get_var_ty_for_size(reg.reg.ty, reg.size);

          struct ir_op *load =
              insert_before_ir_op(func, last, IR_OP_TY_LOAD, load_ty);

          struct ir_op *addr_offset = insert_before_ir_op(
              func, load, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
          addr_offset->addr_offset =
              (struct ir_op_addr_offset){.base = addr, .offset = j * reg.size};

          load->load = (struct ir_op_load){.ty = IR_OP_LOAD_TY_ADDR,
                                           .addr = addr_offset};

          vector_push_back(new_args, &load);

          last = load;
        }

        if (detach) {
          detach_ir_op(func, arg);
        }
      }
      break;
    case IR_PARAM_INFO_TY_POINTER: {
      DEBUG_ASSERT(arg->ty == IR_OP_TY_LOAD, "expected load");

      struct ir_op *addr = build_addr(func, arg);

      struct ir_lcl *copy = add_local(func, &arg->var_ty);

      struct ir_op *dest_addr =
          insert_before_ir_op(func, op, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
      dest_addr->addr =
          (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = copy};

      struct ir_op *mem_copy =
          insert_before_ir_op(func, op, IR_OP_TY_MEM_COPY, IR_VAR_TY_NONE);
      mem_copy->mem_copy = (struct ir_op_mem_copy){
          .dest = dest_addr,
          .source = addr,
          .length = var_ty_info(func->unit, &copy->var_ty).size};

      vector_push_back(new_args, &dest_addr);
      break;
    }
    }
  }

  CLONE_AND_FREE_VECTOR(func->arena, new_args, op->call.num_args,
                        op->call.args);

  if (!func_info.call_info.ret) {
    op->var_ty = IR_VAR_TY_NONE;
    return;
  }

  struct ir_param_info param_info = *func_info.call_info.ret;

  if (param_info.ty == IR_PARAM_INFO_TY_POINTER) {
    // already handled
    return;
  }

  struct ir_var_ty_flattened info =
      var_ty_info_flat(func->unit, param_info.var_ty);

  if (var_ty_is_aggregate(param_info.var_ty)) {
    // return is in reg
    // we need to generate ops to retrieve it from the registers

    op->var_ty = IR_VAR_TY_NONE;

    // HACK: should use op uses
    struct ir_op *prev_store = op->succ ? op->succ : op->stmt->succ->first;
    while (prev_store->ty != IR_OP_TY_STORE)
      prev_store = prev_store->succ;
    DEBUG_ASSERT(prev_store->ty == IR_OP_TY_STORE, "expected store after call");

    struct ir_op *addr = build_addr(func, prev_store);

    size_t field_idx = 0;
    size_t offset = 0;

    // HACK: order of stores must be order of fields for `lower_call_registers`
    struct ir_op *last_mov = op;
    struct ir_op *last = prev_store;
    for (size_t j = 0; j < param_info.num_regs; j++) {
      struct ir_param_reg reg = param_info.regs[j];

      size_t offset_add;
      struct ir_var_ty store_ty;
      size_t field_offset = info.fields[field_idx + 1].offset - offset;
      if (field_idx + 1 >= info.num_fields || field_offset >= reg.size) {
        // only storeing one field, so store the field ty
        size_t field_size =
            var_ty_info(func->unit, &info.fields[field_idx].var_ty).size;
        store_ty = get_var_ty_for_size(reg.reg.ty, field_size);

        offset_add = field_offset;
      } else {
        store_ty = get_var_ty_for_size(reg.reg.ty, reg.size);

        offset_add = reg.size;
      }

      struct ir_op *mov =
          insert_after_ir_op(func, last_mov, IR_OP_TY_MOV, store_ty);
      last_mov = mov;

      mov->mov = (struct ir_op_mov){.value = NULL};
      // we should probs have a different flag for this
      mov->flags |= IR_OP_FLAG_PARAM;

      struct ir_op *addr_offset = insert_after_ir_op(
          func, last, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
      addr_offset->addr_offset =
          (struct ir_op_addr_offset){.base = addr, .offset = offset};

      offset += offset_add;

      while (field_idx < info.num_fields &&
             info.fields[field_idx].offset < offset) {
        field_idx++;
      }

      struct ir_op *store =
          insert_after_ir_op(func, addr_offset, IR_OP_TY_STORE, IR_VAR_TY_NONE);

      store->store = (struct ir_op_store){
          .ty = IR_OP_STORE_TY_ADDR, .addr = addr_offset, .value = mov};

      last = store;
    }

    detach_ir_op(func, prev_store);
  } else {
    // single reg. don't need to do anything, call value can be used directly
  }
}

static void lower_params_registers(struct ir_func *func) {
  // assign registers to the params for regalloc

  struct ir_call_info call_info = func->call_info;

  if (call_info.num_params) {
    struct ir_op *param_op = func->first->first->first;

    size_t param_idx = 0;
    size_t reg_idx = 0;

    bool has_implicit_first =
        call_info.ret && call_info.ret->ty == IR_PARAM_INFO_TY_POINTER;

    size_t num_params =
        has_implicit_first ? call_info.num_params + 1 : call_info.num_params;

    while (param_idx < num_params) {
      struct ir_param_info *param_info;

      if (param_idx == 0 && has_implicit_first) {
        param_info = call_info.ret;
      } else {
        param_info =
            &call_info.params[has_implicit_first ? param_idx - 1 : param_idx];
      }

      switch (param_info->ty) {
      case IR_PARAM_INFO_TY_REGISTER:
      case IR_PARAM_INFO_TY_POINTER:
        DEBUG_ASSERT(param_op->flags & IR_OP_FLAG_PARAM, "expected param op");

        struct ir_op *mov =
            insert_before_ir_op(func, param_op, IR_OP_TY_MOV, param_op->var_ty);
        mov->mov = (struct ir_op_mov){.value = NULL};

        mov->reg = param_info->regs[reg_idx].reg;
        mov->flags =
            (param_op->flags & ~(IR_OP_FLAG_SPILLED | IR_OP_FLAG_ETERNAL)) |
            IR_OP_FLAG_FIXED_REG;

        DEBUG_ASSERT(mov->reg.ty != IR_REG_TY_NONE, "fixed reg had TY_NONE");

        param_op->flags &= ~IR_OP_FLAG_PARAM;

        param_op->mov = (struct ir_op_mov){.value = mov};
        param_op->reg = NO_REG;

        reg_idx++;

        if (reg_idx >= param_info->num_regs) {
          reg_idx = 0;
          param_idx++;
        }
        break;
      case IR_PARAM_INFO_TY_STACK: {
        struct ir_var_ty_info info =
            var_ty_info(func->unit, param_info->var_ty);

        struct ir_lcl *lcl;
        if (param_op->ty == IR_OP_TY_MOV) {
          lcl = add_local(func, &param_op->var_ty);

          param_op =
              replace_ir_op(func, param_op, IR_OP_TY_LOAD, param_op->var_ty);
          param_op->load =
              (struct ir_op_load){.ty = IR_OP_LOAD_TY_LCL, .lcl = lcl};
        } else {
          DEBUG_ASSERT(param_op->ty == IR_OP_TY_ADDR &&
                           param_op->addr.ty == IR_OP_ADDR_TY_LCL,
                       "expected addr.lcl");

          // FIXME: this won't dealloc allocated locals
          lcl = param_op->addr.lcl;
        }

        ssize_t offset = -param_info->stack_offset;

        lcl->flags |= IR_LCL_FLAG_PARAM;
        lcl->alloc_ty = IR_LCL_ALLOC_TY_FIXED;
        lcl->alloc = (struct ir_lcl_alloc){
            .padding = 0, .size = info.size, .offset = offset};

        param_idx++;
        break;
      }
      }

      param_op = param_op->succ;
    }
  }

  struct ir_func_iter iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);

  struct ir_op *op;
  while (ir_func_iter_next(&iter, &op)) {
    if (op->ty != IR_OP_TY_RET || op->ret.value == NULL) {
      continue;
    }

    if (!func->call_info.ret) {
      op->ret.value = NULL;
      continue;
    }

    // keep the gather, as it makes sure the moves are used and don't get
    // eliminated gather is marked as not producing a value so it won't get a
    // register allocation

    struct ir_op *ret_value = op->ret.value;
    if (ret_value->ty == IR_OP_TY_GATHER) {
      for (size_t i = 0; i < ret_value->gather.num_values; i++) {
        struct ir_gather_value *gather_val = &ret_value->gather.values[i];

        struct ir_op *value = gather_val->value;

        // FIXME: again will fail on registers which return multiple fields in
        // one reg e.g bool[2]
        value->reg = call_info.ret->regs[i].reg;
        value->flags |= IR_OP_FLAG_FIXED_REG;
      }
      ret_value->flags |= IR_OP_FLAG_FIXED_REG;

    } else if (ret_value->ty == IR_OP_TY_MOV) {
      DEBUG_ASSERT(func->call_info.ret->num_regs == 1,
                   "mov but multi reg return? expected gather");
      ret_value->reg = func->call_info.ret->regs[0].reg;
      ret_value->flags |= IR_OP_FLAG_FIXED_REG;
    } else if (ret_value->ty == IR_OP_TY_LOAD) {
      ret_value->reg = func->call_info.ret->regs[0].reg;
      ret_value->flags |= IR_OP_FLAG_FIXED_REG;
    } else {
      BUG("expected gather/mov/load for reg return");
    }
  }
}
static void lower_call_registers(struct ir_func *func, struct ir_op *op) {
  struct ir_func_info func_info = op->call.func_info;

  struct ir_call_info call_info = func_info.call_info;

  struct vector *new_args = vector_create(sizeof(struct ir_op *));

  // if this is an indirect call, put a move of the target to split the live
  // range but ensure it isn't allocated into a reg used for args
  if (!(op->call.target->flags & IR_OP_FLAG_CONTAINED)) {
    struct ir_op *mov =
        insert_before_ir_op(func, op, IR_OP_TY_MOV, IR_VAR_TY_POINTER);
    mov->mov = (struct ir_op_mov){.value = op->call.target};

    op->call.target = mov;
  }

  func->caller_stack_needed =
      MAX(func->caller_stack_needed, func_info.call_info.stack_size);

  size_t param_idx = 0;
  size_t reg_idx = 0;

  bool has_implicit_first =
      call_info.ret && call_info.ret->ty == IR_PARAM_INFO_TY_POINTER;

  struct ir_op *first = op;
  for (size_t i = 0; i < op->call.num_args; i++) {
    struct ir_param_info *param_info;

    if (param_idx == 0 && has_implicit_first) {
      param_info = call_info.ret;
    } else {
      param_info =
          &call_info.params[has_implicit_first ? param_idx - 1 : param_idx];
    }

    struct ir_op *arg = op->call.args[i];

    if (param_info->ty == IR_PARAM_INFO_TY_STACK) {
      struct ir_lcl *lcl = add_local(func, &arg->var_ty);

      struct ir_var_ty_info info = var_ty_info(func->unit, &lcl->var_ty);

      lcl->alloc_ty = IR_LCL_ALLOC_TY_FIXED;
      lcl->alloc = (struct ir_lcl_alloc){
          .padding = 0, .size = info.size, .offset = param_info->stack_offset};

      struct ir_op *dest_addr =
          insert_before_ir_op(func, first, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
      dest_addr->addr = (struct ir_op_addr){
          .ty = IR_OP_ADDR_TY_LCL,
          .lcl = lcl,
      };

      if (arg->ty == IR_OP_TY_LOAD) {
        struct ir_op *addr = build_addr(func, arg);

        struct ir_op *mem_copy =
            insert_before_ir_op(func, first, IR_OP_TY_MEM_COPY, IR_VAR_TY_NONE);
        mem_copy->mem_copy = (struct ir_op_mem_copy){
            .dest = dest_addr, .source = addr, .length = info.size};

        lower_mem_copy(func, mem_copy);
        // HACK: relies on `lower_mem_copy` transforming the op into a call (not
        // inserting it later/earlier)
        lower_call_registers(func, mem_copy);

        detach_ir_op(func, arg);

      } else {
        struct ir_op *store =
            insert_after_ir_op(func, dest_addr, IR_OP_TY_STORE, IR_VAR_TY_NONE);
        store->store = (struct ir_op_store){
            .ty = IR_OP_STORE_TY_ADDR, .addr = dest_addr, .value = arg};
      }

      param_idx++;
    } else {
      struct ir_op *mov =
          insert_before_ir_op(func, op, IR_OP_TY_MOV, arg->var_ty);
      mov->mov = (struct ir_op_mov){.value = arg};

      if (i == 0) {
        first = mov;
      }

      mov->reg = param_info->regs[reg_idx++].reg;
      mov->flags = (arg->flags & ~(IR_OP_FLAG_SPILLED | IR_OP_FLAG_ETERNAL |
                                   IR_OP_FLAG_PARAM)) |
                   IR_OP_FLAG_FIXED_REG;

      DEBUG_ASSERT(mov->reg.ty != IR_REG_TY_NONE, "fixed reg had TY_NONE");

      vector_push_back(new_args, &mov);

      if (reg_idx >= param_info->num_regs) {
        reg_idx = 0;
        param_idx++;
      }
    }
  }

  if (func_info.call_info.flags & IR_CALL_INFO_FLAG_NUM_VARIADIC) {
    struct ir_op *cnst =
        insert_before_ir_op(func, op, IR_OP_TY_CNST, IR_VAR_TY_I32);
    cnst->cnst =
        (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT,
                            .int_value = func_info.call_info.num_variadics};
    cnst->reg = func_info.call_info.num_variadics_reg;
    cnst->flags |= IR_OP_FLAG_SIDE_EFFECTS | IR_OP_FLAG_FIXED_REG;
  }

  CLONE_AND_FREE_VECTOR(func->arena, new_args, op->call.num_args,
                        op->call.args);

  if (!call_info.ret) {
    return;
  }

  struct ir_param_info ret_info = *call_info.ret;

  switch (ret_info.ty) {
  case IR_PARAM_INFO_TY_REGISTER: {
    if (ret_info.num_regs == 1) {
      struct ir_op *new_call =
          insert_before_ir_op(func, op, IR_OP_TY_CALL, op->var_ty);
      new_call->call = op->call;
      new_call->flags |= IR_OP_FLAG_FIXED_REG;
      new_call->reg = ret_info.regs[0].reg;

      op->ty = IR_OP_TY_MOV;
      op->mov = (struct ir_op_mov){.value = new_call};
    } else {
      struct ir_op *mov_op = op->succ;

      for (size_t i = 0; i < ret_info.num_regs; i++) {
        DEBUG_ASSERT(mov_op->flags & IR_OP_FLAG_PARAM, "expected param ret op");

        struct ir_op *mov =
            insert_before_ir_op(func, mov_op, IR_OP_TY_MOV, mov_op->var_ty);
        mov->mov = (struct ir_op_mov){.value = NULL};

        mov->reg = ret_info.regs[i].reg;
        mov->flags =
            (mov_op->flags & ~(IR_OP_FLAG_SPILLED | IR_OP_FLAG_ETERNAL)) |
            IR_OP_FLAG_FIXED_REG | IR_OP_FLAG_PARAM;

        mov_op->mov = (struct ir_op_mov){.value = mov};
        mov_op->flags &= ~IR_OP_FLAG_PARAM;

        mov_op = mov_op->succ;
      }
    }
    break;
  }
  case IR_PARAM_INFO_TY_STACK:
    break;
  case IR_PARAM_INFO_TY_POINTER:
    break;
  }
}

void lower_abi(struct ir_unit *unit) {
  struct ir_glb *glb = unit->first_global;
  while (glb) {
    if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
      glb = glb->succ;
      continue;
    }

    switch (glb->ty) {
    case IR_GLB_TY_DATA:
      break;
    case IR_GLB_TY_FUNC: {
      struct ir_func *func = glb->func;

      // TODO: make this lowering global (and call a target-specific function)
      // and also do it for undef symbols
      struct ir_func_info info =
          unit->target->lower_func_ty(func, func->func_ty, NULL, 0);
      func->func_ty = info.func_ty;
      func->call_info = info.call_info;

      lower_params(func);

      struct ir_func_iter iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);

      struct ir_op *op;
      while (ir_func_iter_next(&iter, &op)) {
        if (op->ty != IR_OP_TY_CALL) {
          continue;
        }

        lower_call(func, op);
      }
    }
    }

    glb = glb->succ;
  }
}

void lower(struct ir_unit *unit) {
  struct ir_glb *glb = unit->first_global;
  while (glb) {
    if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
      glb = glb->succ;
      continue;
    }

    switch (glb->ty) {
    case IR_GLB_TY_DATA:
      break;
    case IR_GLB_TY_FUNC: {
      struct ir_func *func = glb->func;
      struct ir_basicblock *basicblock = func->first;

      lower_params_registers(func);

      struct ir_func_iter iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);

      // first lower memset as it can generate calls
      struct ir_op *op;
      while (ir_func_iter_next(&iter, &op)) {
        switch (op->ty) {
        case IR_OP_TY_MEM_SET:
          lower_mem_set(func, op);
          break;
        case IR_OP_TY_MEM_COPY:
          lower_mem_copy(func, op);
          break;
        default:
          continue;
        }
      }

      iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);
      while (ir_func_iter_next(&iter, &op)) {
        if (op->ty != IR_OP_TY_CALL) {
          continue;
        }

        if (op->call.target->ty == IR_OP_TY_ADDR &&
            op->call.target->addr.ty == IR_OP_ADDR_TY_GLB &&
            !(op->call.target->flags & IR_OP_FLAG_CONTAINED)) {
          op->call.target = alloc_contained_ir_op(func, op->call.target, op);
        }

        lower_call_registers(func, op);
      }

      while (basicblock) {
        struct ir_stmt *stmt = basicblock->first;

        while (stmt) {
          op = stmt->first;

          while (op) {
            switch (op->ty) {
            case IR_OP_TY_UNKNOWN:
              BUG("unknown op!");
            case IR_OP_TY_UNDF:
            case IR_OP_TY_CUSTOM:
            case IR_OP_TY_CNST:
            case IR_OP_TY_PHI:
            case IR_OP_TY_UNARY_OP:
            case IR_OP_TY_BINARY_OP:
            case IR_OP_TY_GATHER:
            case IR_OP_TY_ADDR:
            case IR_OP_TY_ADDR_OFFSET:
            case IR_OP_TY_BR:
            case IR_OP_TY_MOV:
            case IR_OP_TY_RET:
            case IR_OP_TY_BITFIELD_EXTRACT:
            case IR_OP_TY_BITFIELD_INSERT:
            case IR_OP_TY_CALL:
            case IR_OP_TY_MEM_COPY:
            case IR_OP_TY_MEM_SET:
            case IR_OP_TY_CAST_OP:
              break;
            case IR_OP_TY_BR_COND:
              lower_br_cond(func, op);
              break;
            case IR_OP_TY_BR_SWITCH:
              lower_br_switch(func, op);
              break;
            case IR_OP_TY_STORE:
              lower_store_to_addr(func, op);
              lower_store(func, op);
              break;
            case IR_OP_TY_LOAD:
              lower_load_to_addr(func, op);
              break;
            case IR_OP_TY_STORE_BITFIELD:
              lower_store_bitfield(func, op);
              break;
            case IR_OP_TY_LOAD_BITFIELD:
              lower_load_bitfield(func, op);
              break;
            }

            op = op->succ;
          }

          stmt = stmt->succ;
        }

        basicblock = basicblock->succ;
      }

      // run an early elimination pass before we alloc locals, as it is hard to do after
      eliminate_redundant_ops(func, ELIMINATE_REDUNDANT_OPS_FLAG_DONT_ELIM_LCLS);

      // alloc locals EARLY so that targets can contain their addressing nodes
      // properly
      alloc_locals(func);
    }
    }

    glb = glb->succ;
  }

  lower_pointers(unit);

  // now target-specific lowering
  if (unit->target->lower) {
    unit->target->lower(unit);
  }

  // lower again, in case target lowering introduced any
  lower_pointers(unit);

  // now we do pruning before regalloc
  glb = unit->first_global;
  while (glb) {
    if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
      glb = glb->succ;
      continue;
    }

    switch (glb->ty) {
    case IR_GLB_TY_DATA:
      break;
    case IR_GLB_TY_FUNC: {
      struct ir_func *func = glb->func;

      prune_basicblocks(func);

      // FIXME: phis are not propogated properly
      remove_critical_edges(func);

      eliminate_redundant_ops(func, ELIMINATE_REDUNDANT_OPS_FLAG_ELIM_MOVS | ELIMINATE_REDUNDANT_OPS_FLAG_DONT_ELIM_LCLS);
    }
    }
    glb = glb->succ;
  }
}
