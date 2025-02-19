#include "lower.h"

#include "bit_twiddle.h"
#include "ir/ir.h"
#include "log.h"
#include "util.h"
#include "vector.h"

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

  if (info.size <= MEMMOVE_THRESHOLD) {
    op->ty = IR_OP_TY_MEM_COPY;
    op->mem_copy = (struct ir_op_mem_copy){
        .dest = dest_addr, .source = source_addr, .length = info.size};
    return;
  }

  struct ir_op *size =
      insert_after_ir_op(func, dest_addr, IR_OP_TY_CNST, IR_VAR_TY_NONE);
  mk_pointer_constant(func->unit, size, info.size);

  struct ir_glb *memmove =
      add_well_known_global(func->unit, IR_WELL_KNOWN_GLB_MEMMOVE);

  struct ir_var_ty ptr_int = var_ty_for_pointer_size(func->unit);
  struct ir_op *memmove_addr =
      insert_after_ir_op(func, size, IR_OP_TY_ADDR, ptr_int);
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
      insert_after_ir_op(func, addr, IR_OP_TY_CNST, IR_VAR_TY_I32);
  value_cnst->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT,
                                         .int_value = op->mem_set.value};

  struct ir_var_ty ptr_int = var_ty_for_pointer_size(func->unit);

  struct ir_op *length_cnst =
      insert_after_ir_op(func, addr, IR_OP_TY_CNST, ptr_int);
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

static void lower_store_glb(struct ir_func *func, struct ir_op *op) {
  struct ir_op *addr =
      insert_before_ir_op(func, op, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
  addr->addr =
      (struct ir_op_addr){.ty = IR_OP_ADDR_TY_GLB, .glb = op->store.glb};

  struct ir_op *value = op->store.value;

  op->store = (struct ir_op_store){
      .ty = IR_OP_STORE_TY_ADDR, .addr = addr, .value = value};
}

static void lower_load_glb(struct ir_func *func, struct ir_op *op) {
  struct ir_op *addr =
      insert_before_ir_op(func, op, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
  addr->addr =
      (struct ir_op_addr){.ty = IR_OP_ADDR_TY_GLB, .glb = op->load.glb};

  op->ty = IR_OP_TY_LOAD;
  op->load = (struct ir_op_load){.ty = IR_OP_LOAD_TY_ADDR, .addr = addr};
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
  // struct ir_var_func_ty func_ty = func->func_ty;
  struct ir_call_info call_info = func->call_info;

  if (call_info.num_params) {
    struct ir_op *param_op = func->first->first->first;
    struct ir_op *after_params = param_op->stmt->succ->first;
    for (size_t i = 0; i < call_info.num_params; i++) {
      DEBUG_ASSERT(param_op->flags & IR_OP_FLAG_PARAM, "expected param op");

      struct ir_param_info param_info = call_info.params[i];
      struct ir_var_ty_info info = var_ty_info(func->unit, param_info.var_ty);

      switch (param_info.ty) {
      case IR_PARAM_INFO_TY_REGISTER:
        if (var_ty_is_aggregate(param_info.var_ty)) {
          DEBUG_ASSERT(param_op->ty == IR_OP_TY_ADDR, "expected addr");

          struct ir_var_ty store_ty = get_var_ty_for_size(
              param_info.reg.start_reg.ty, param_info.reg.size);

          size_t num_reg =
              (info.size + (param_info.reg.size - 1)) / param_info.reg.size;

          struct ir_op *addr = insert_before_ir_op(
              func, after_params, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
          addr->addr = (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL,
                                           .lcl = param_op->addr.lcl};
          // addr->flags |= IR_OP_FLAG_CONTAINED;

          for (size_t j = num_reg; j; j--) {
            struct ir_op *store =
                insert_after_ir_op(func, addr, IR_OP_TY_STORE, IR_VAR_TY_NONE);

            struct ir_op *addr_offset = insert_before_ir_op(
                func, store, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
            addr_offset->addr_offset = (struct ir_op_addr_offset){
                .base = addr, .offset = (j - 1) * param_info.reg.size};

            struct ir_op *mov;
            if (j - 1 == 0) {
              mov = replace_ir_op(func, param_op, IR_OP_TY_MOV, store_ty);
            } else {
              mov = insert_after_ir_op(func, param_op, IR_OP_TY_MOV, store_ty);
            }
            mov->mov = (struct ir_op_mov){.value = NULL};
            mov->flags |= IR_OP_FLAG_FIXED_REG | IR_OP_FLAG_PARAM;
            mov->reg =
                (struct ir_reg){.ty = param_info.reg.start_reg.ty,
                                .idx = param_info.reg.start_reg.idx + j - 1};

            // addr_offset->flags |= IR_OP_FLAG_CONTAINED;

            store->store = (struct ir_op_store){
                .ty = IR_OP_STORE_TY_ADDR, .addr = addr_offset, .value = mov};
          }
        } else {
          param_op->flags |= IR_OP_FLAG_FIXED_REG;
          param_op->reg = param_info.reg.start_reg;
        }
        break;
      case IR_PARAM_INFO_TY_STACK:
        break;
      case IR_PARAM_INFO_TY_POINTER: {
        DEBUG_ASSERT(param_op->ty == IR_OP_TY_ADDR && param_op->addr.ty == IR_OP_ADDR_TY_LCL, "expected addr");

        struct ir_op *param = insert_before_ir_op(func, param_op, IR_OP_TY_MOV, IR_VAR_TY_POINTER);
        param->mov = (struct ir_op_mov){.value = NULL};
        param->flags |= IR_OP_FLAG_FIXED_REG | IR_OP_FLAG_PARAM;
        param->reg =
            (struct ir_reg){.ty = param_info.reg.start_reg.ty,
                            .idx = param_info.reg.start_reg.idx};
        

        struct ir_func_iter iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);
        struct ir_lcl *lcl = param_op->addr.lcl;

        struct ir_op *op;
        while (ir_func_iter_next(&iter, &op)) {
          if (op->ty == IR_OP_TY_ADDR && op->addr.ty == IR_OP_ADDR_TY_LCL && op->addr.lcl == lcl) {
            op = replace_ir_op(func, op, IR_OP_TY_MOV, IR_VAR_TY_POINTER);
            op->flags &= ~IR_OP_FLAG_PARAM;
            op->mov = (struct ir_op_mov){.value = param};
          } else if (op->ty == IR_OP_TY_LOAD && op->load.ty == IR_OP_LOAD_TY_LCL && op->load.lcl == lcl) {
            op->load.ty = IR_OP_LOAD_TY_ADDR;
            op->load.addr = param;
          } else if (op->ty == IR_OP_TY_STORE && op->store.ty == IR_OP_STORE_TY_LCL && op->store.lcl == lcl) {
            op->store.ty = IR_OP_STORE_TY_ADDR;
            op->store.addr = param;
          }
        }

        break;
      }
      }

      // there is exactly one op per param
      param_op = param_op->succ;
    }

    // now go through and add non-fixed movs so regalloc doesn't get broken by
    // long living params
    param_op = func->first->first->first;
    for (size_t i = 0; i < call_info.num_params; i++) {
      struct ir_op *mov =
          insert_before_ir_op(func, param_op, IR_OP_TY_MOV, param_op->var_ty);
      mov->mov = (struct ir_op_mov){.value = NULL};

      mov->reg = param_op->reg;
      mov->flags = param_op->flags & ~(IR_OP_FLAG_SPILLED);
      param_op->flags = IR_OP_FLAG_NONE;

      param_op->mov = (struct ir_op_mov){.value = mov};

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

    struct ir_param_info param_info = *func->call_info.ret;
    struct ir_var_ty_info info = var_ty_info(func->unit, param_info.var_ty);

    switch (param_info.ty) {
    case IR_PARAM_INFO_TY_REGISTER: {
      if (var_ty_is_aggregate(param_info.var_ty)) {
        DEBUG_ASSERT(op->ret.value->ty == IR_OP_TY_LOAD, "expected load");
        struct ir_var_ty load_ty = get_var_ty_for_size(
            param_info.reg.start_reg.ty, param_info.reg.size);

        size_t num_reg =
            (info.size + (param_info.reg.size - 1)) / param_info.reg.size;

        struct ir_op *addr = build_addr(func, op->ret.value);
        struct ir_op *last = op;

        for (size_t j = num_reg; j; j--) {
          struct ir_op *load =
              insert_before_ir_op(func, last, IR_OP_TY_LOAD, load_ty);

          struct ir_op *addr_offset = insert_before_ir_op(
              func, load, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
          addr_offset->addr_offset = (struct ir_op_addr_offset){
              .base = addr, .offset = (j - 1) * param_info.reg.size};

          // addr_offset->flags |= IR_OP_FLAG_CONTAINED;

          load->load = (struct ir_op_load){.ty = IR_OP_LOAD_TY_ADDR,
                                           .addr = addr_offset};
          load->reg =
              (struct ir_reg){.ty = param_info.reg.start_reg.ty,
                              .idx = param_info.reg.start_reg.idx + j - 1};
          load->flags |= IR_OP_FLAG_FIXED_REG | IR_OP_FLAG_SIDE_EFFECTS;
          last = load;
        }
      } else {
        struct ir_op *mov =
            insert_before_ir_op(func, op, IR_OP_TY_MOV, op->ret.value->var_ty);
        mov->mov = (struct ir_op_mov){.value = op->ret.value};
        mov->reg = param_info.reg.start_reg;
        mov->flags |= IR_OP_FLAG_FIXED_REG;
      }
      break;
    }
    case IR_PARAM_INFO_TY_POINTER:
      // nop, as we write to the pointer
      break;
    case IR_PARAM_INFO_TY_STACK:
      unreachable();
    }

    op->ret.value = NULL;
  }
}

void lower_call(struct ir_func *func, struct ir_op *op) {
  struct ir_func_info func_info = func->unit->target->lower_func_ty(
      func, op->call.func_ty.func, op->call.args, op->call.num_args);

  func->caller_stack_needed =
      MAX(func->caller_stack_needed, func_info.call_info.stack_size);

  struct vector *new_args = vector_create(sizeof(struct ir_op *));

  op->call.func_ty.func = func_info.func_ty;

  // if this is an indirect call, put a move of the target to split the live
  // range but ensure it isn't allocated into a reg used for args
  if (!(op->call.target->flags & IR_OP_FLAG_CONTAINED)) {
    struct ir_op *mov =
        insert_before_ir_op(func, op, IR_OP_TY_MOV, IR_VAR_TY_POINTER);
    mov->mov = (struct ir_op_mov){.value = op->call.target};

    op->call.target = mov;
  }

  for (size_t i = 0; i < op->call.num_args; i++) {
    struct ir_op *arg = op->call.args[i];
    DEBUG_ASSERT(i < func_info.call_info.num_params, "out of range");
    struct ir_param_info param_info = func_info.call_info.params[i];

    struct ir_var_ty_info info = var_ty_info(func->unit, param_info.var_ty);

    switch (param_info.ty) {
    case IR_PARAM_INFO_TY_REGISTER:
      if (arg->ty != IR_OP_TY_LOAD && param_info.reg.num_reg == 1) {
        struct ir_var_ty var_ty = get_var_ty_for_size(
            param_info.reg.start_reg.ty, param_info.reg.size);

        struct ir_op *mov = insert_before_ir_op(func, op, IR_OP_TY_MOV, var_ty);
        mov->mov = (struct ir_op_mov){.value = arg};
        mov->reg = param_info.reg.start_reg;
        mov->flags |= IR_OP_FLAG_FIXED_REG;

        vector_push_back(new_args, &mov);

      } else {
        if (arg->ty != IR_OP_TY_LOAD) {
          arg = spill_op(func, arg);
        }

        struct ir_var_ty load_ty = get_var_ty_for_size(
            param_info.reg.start_reg.ty, param_info.reg.size);

        struct ir_op *addr = build_addr(func, arg);
        struct ir_op *last = op;

        for (size_t j = param_info.reg.num_reg; j; j--) {
          struct ir_op *load =
              insert_before_ir_op(func, last, IR_OP_TY_LOAD, load_ty);

          struct ir_op *addr_offset = insert_before_ir_op(
              func, load, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
          addr_offset->addr_offset = (struct ir_op_addr_offset){
              .base = addr, .offset = (j - 1) * param_info.reg.size};

          // addr_offset->flags |= IR_OP_FLAG_CONTAINED;

          load->load = (struct ir_op_load){.ty = IR_OP_LOAD_TY_ADDR,
                                           .addr = addr_offset};
          load->reg =
              (struct ir_reg){.ty = param_info.reg.start_reg.ty,
                              .idx = param_info.reg.start_reg.idx + j - 1};
          load->flags |= IR_OP_FLAG_FIXED_REG;

          vector_push_back(new_args, &load);

          last = load;
        }
      }
      break;
    case IR_PARAM_INFO_TY_STACK: {
      struct ir_lcl *lcl = add_local(func, &arg->var_ty);
      lcl->flags |= IR_LCL_FLAG_FIXED_OFFSET;
      lcl->offset = param_info.stack_offset;

      if (arg->ty == IR_OP_TY_LOAD) {
        struct ir_op *addr = build_addr(func, arg);

        size_t copy = info.size;
        size_t offset = 0;
        struct ir_op *last = arg;
        while (copy) {
          // FIXME: this overcopies, e.g 8 bytes on a 7 byte struct
          size_t size = MIN(copy, 8);
          size = ROUND_UP(size, ILOG2(size) + 1);
          struct ir_var_ty store_ty =
              get_var_ty_for_size(IR_REG_TY_INTEGRAL, size);

          struct ir_op *load_addr_offset = insert_after_ir_op(
              func, last, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
          load_addr_offset->addr_offset =
              (struct ir_op_addr_offset){.base = addr, .offset = offset};
          // load_addr_offset->flags |= IR_OP_FLAG_CONTAINED;

          struct ir_op *load = insert_after_ir_op(func, load_addr_offset,
                                                  IR_OP_TY_LOAD, store_ty);

          load->load = (struct ir_op_load){.ty = IR_OP_LOAD_TY_ADDR,
                                           .addr = load_addr_offset};

          struct ir_op *store =
              insert_after_ir_op(func, load, IR_OP_TY_STORE, IR_VAR_TY_NONE);

          store->store = (struct ir_op_store){
              .ty = IR_OP_STORE_TY_LCL, .lcl = lcl, .value = load};

          last = store;
          offset += size;
          copy -= size;
        }
      } else {
        struct ir_op *store =
            insert_before_ir_op(func, op, IR_OP_TY_STORE, IR_VAR_TY_NONE);
        store->store = (struct ir_op_store){
            .ty = IR_OP_STORE_TY_LCL, .lcl = lcl, .value = arg};
        vector_push_back(new_args, &store);
      }
      break;
    }
    case IR_PARAM_INFO_TY_POINTER: {
      DEBUG_ASSERT(arg->ty == IR_OP_TY_LOAD, "expected load");

      struct ir_op *addr = build_addr(func, arg);

      struct ir_op *mov =
          insert_before_ir_op(func, op, IR_OP_TY_MOV, IR_VAR_TY_POINTER);
      mov->mov = (struct ir_op_mov){.value = addr};
      mov->reg = param_info.reg.start_reg;
      mov->flags |= IR_OP_FLAG_FIXED_REG;
      vector_push_back(new_args, &mov);
      break;
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

  op->call.args = vector_head(new_args);
  op->call.num_args = vector_length(new_args);

  if (!func_info.call_info.ret) {
    op->var_ty = IR_VAR_TY_NONE;
    return;
  }

  struct ir_param_info param_info = *func_info.call_info.ret;
  struct ir_var_ty_info info = var_ty_info(func->unit, param_info.var_ty);

  if (var_ty_is_aggregate(param_info.var_ty)) {
    op->var_ty = IR_VAR_TY_NONE;

    struct ir_var_ty store_ty =
        get_var_ty_for_size(param_info.reg.start_reg.ty, param_info.reg.size);

    size_t num_reg =
        (info.size + (param_info.reg.size - 1)) / param_info.reg.size;

    struct ir_op *prev_store = op->succ ? op->succ : op->stmt->succ->first;
    DEBUG_ASSERT(prev_store->ty == IR_OP_TY_STORE, "expected store after call");

    struct ir_op *addr = build_addr(func, prev_store);

    struct ir_op *last = prev_store;
    for (size_t j = 0; j < num_reg; j++) {
      struct ir_op *mov = insert_after_ir_op(func, op, IR_OP_TY_MOV, store_ty);

      mov->mov = (struct ir_op_mov){.value = NULL};
      mov->flags |= IR_OP_FLAG_FIXED_REG | IR_OP_FLAG_PARAM;
      mov->reg = (struct ir_reg){.ty = param_info.reg.start_reg.ty,
                                 .idx = param_info.reg.start_reg.idx + j};

      struct ir_op *addr_offset = insert_after_ir_op(
          func, last, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
      addr_offset->addr_offset = (struct ir_op_addr_offset){
          .base = addr, .offset = j * param_info.reg.size};

      // addr_offset->flags |= IR_OP_FLAG_CONTAINED;

      struct ir_op *store =
          insert_after_ir_op(func, addr_offset, IR_OP_TY_STORE, IR_VAR_TY_NONE);

      store->store = (struct ir_op_store){
          .ty = IR_OP_STORE_TY_ADDR, .addr = addr_offset, .value = mov};

      last = store;
    }

    detach_ir_op(func, prev_store);
  } else {
    // now fix the ret and add a mov to detach it
    struct ir_op *call =
        insert_before_ir_op(func, op, IR_OP_TY_CALL, op->var_ty);
    call->call = op->call;
    call->flags = op->flags;
    call->flags |= IR_OP_FLAG_FIXED_REG;
    call->reg = func_info.call_info.ret->reg.start_reg;

    op->ty = IR_OP_TY_MOV;
    op->mov = (struct ir_op_mov){.value = call};
  }
}

void lower(struct ir_unit *unit, const struct target *target) {
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

        if (op->call.target->ty == IR_OP_TY_ADDR &&
            op->call.target->addr.ty == IR_OP_ADDR_TY_GLB &&
            !(op->call.target->flags & IR_OP_FLAG_CONTAINED)) {
          op->call.target = alloc_contained_ir_op(func, op->call.target, op);
        }

        lower_call(func, op);
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
            case IR_OP_TY_ADDR:
            case IR_OP_TY_ADDR_OFFSET:
            case IR_OP_TY_BR:
            case IR_OP_TY_MOV:
            case IR_OP_TY_RET:
            case IR_OP_TY_BITFIELD_EXTRACT:
            case IR_OP_TY_BITFIELD_INSERT:
            case IR_OP_TY_CALL:
            case IR_OP_TY_MEM_COPY:
            case IR_OP_TY_CAST_OP:
              break;
            case IR_OP_TY_BR_COND:
              lower_br_cond(func, op);
              break;
            case IR_OP_TY_MEM_SET:
              lower_mem_set(func, op);
              break;
            case IR_OP_TY_BR_SWITCH:
              lower_br_switch(func, op);
              break;
            case IR_OP_TY_STORE:
              if (op->store.ty == IR_OP_STORE_TY_GLB) {
                lower_store_glb(func, op);
              }
              lower_store(func, op);
              break;
            case IR_OP_TY_LOAD:
              if (op->load.ty == IR_OP_LOAD_TY_GLB) {
                lower_load_glb(func, op);
              }
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
    }
    }

    glb = glb->succ;
  }

  lower_pointers(unit);

  // now target-specific lowering
  if (target->lower) {
    target->lower(unit);
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

      struct ir_op_use_map uses = build_op_uses_map(func);

      for (size_t i = 0; i < uses.num_use_datas; i++) {
        struct ir_op_usage *use = &uses.use_datas[i];

        if (!use->num_uses && !op_has_side_effects(use->op)) {
          // DEBUG_ASSERT(!(use->op->flags & IR_OP_FLAG_CONTAINED),
          //              "contained op %zu must have uses", use->op->id);

          debug("detaching op %zu", use->op->id);
          detach_ir_op(func, use->op);
        }
      }

      prune_basicblocks(func);
    }
    }
    glb = glb->succ;
  }

  // TODO: more IR validation good
  // e.g no type mismatches (like in build)
  // but also making sure no ops use ops from in front of them
}
