#include "lower.h"

#include "../aarch64.h"
#include "../vector.h"

static void lower_popcnt(struct ir_func *func, struct ir_op *op) {
  // ARM `cnt` instruction works on vector
  // codegen does most of the work but we need to set it up to be allocated a fp
  // reg

  DEBUG_ASSERT(op->unary_op.value->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE,
               "expected primitive");

  bool need_zext = false;
  struct ir_var_ty fp_var_ty;

  switch (op->unary_op.value->var_ty.primitive) {
  case IR_VAR_PRIMITIVE_TY_I1:
  case IR_VAR_PRIMITIVE_TY_I8:
    // validation enforces mov op is same size
    // FIXME: this is a useless zext validation should not stop us
    // (but wont worsen codegen much, one mov)
    need_zext = true;
    fp_var_ty = IR_VAR_TY_F32;
    break;
  case IR_VAR_PRIMITIVE_TY_I16:
  case IR_VAR_PRIMITIVE_TY_F16:
    fp_var_ty = IR_VAR_TY_F16;
    break;
  case IR_VAR_PRIMITIVE_TY_I32:
  case IR_VAR_PRIMITIVE_TY_F32:
    fp_var_ty = IR_VAR_TY_F32;
    break;
  case IR_VAR_PRIMITIVE_TY_I64:
  case IR_VAR_PRIMITIVE_TY_F64:
    fp_var_ty = IR_VAR_TY_F64;
    break;
  case IR_VAR_PRIMITIVE_TY_I128:
    TODO("popcnt i128");
  }

  struct ir_op *src;
  if (need_zext) {
    struct ir_op *zext =
        ir_insert_before_op(func, op, IR_OP_TY_CAST_OP, IR_VAR_TY_I32);
    zext->cast_op = (struct ir_op_cast_op){.ty = IR_OP_CAST_OP_TY_ZEXT,
                                           .value = op->unary_op.value};

    src = zext;
  } else {
    src = op->unary_op.value;
  }

  struct ir_op *mov = ir_insert_before_op(func, op, IR_OP_TY_MOV, fp_var_ty);
  mov->mov = (struct ir_op_mov){.value = src};

  struct ir_op *cnt =
      ir_insert_after_op(func, mov, IR_OP_TY_UNARY_OP, IR_VAR_TY_F32);
  cnt->unary_op =
      (struct ir_op_unary_op){.ty = IR_OP_UNARY_OP_TY_POPCNT, .value = mov};

  op = ir_replace_op(func, op, IR_OP_TY_MOV, op->var_ty);
  op->mov = (struct ir_op_mov){.value = cnt};
}

static void lower_logical_not(struct ir_func *func, struct ir_op *op) {
  DEBUG_ASSERT(op->ty == IR_OP_TY_UNARY_OP &&
                   op->unary_op.ty == IR_OP_UNARY_OP_TY_LOGICAL_NOT,
               "called on invalid op");

  struct ir_op *zero = ir_insert_before_op(func, op, IR_OP_TY_CNST, op->var_ty);
  zero->cnst.ty = IR_OP_CNST_TY_INT;
  zero->cnst.int_value = 0;

  op->ty = IR_OP_TY_BINARY_OP;
  op->binary_op.ty = IR_OP_BINARY_OP_TY_EQ;
  op->binary_op.lhs = op->unary_op.value;
  op->binary_op.rhs = zero;
}

// variable shifts require both operands to be the same size, as they use the
// same register this is fine, because we can just "fake" the type required and
// get the correct behaviour
// we do need to insert a dummy move so it does not affect behaviour of prev
// instruction though
static void lower_shift(struct ir_func *func, struct ir_op *op) {
  struct ir_op_binary_op *binary_op = &op->binary_op;

  struct ir_op *mov =
      ir_insert_before_op(func, op, IR_OP_TY_MOV, binary_op->lhs->var_ty);
  mov->mov = (struct ir_op_mov){.value = binary_op->rhs};

  binary_op->rhs = mov;
}

// ARM has no modulo function
// so instead of `x = a % b` we do
// `c = a / b; x = a - (c * b)`
static void lower_mod(struct ir_func *func, struct ir_op *op) {
  DEBUG_ASSERT(op->ty == IR_OP_TY_BINARY_OP &&
                   (op->binary_op.ty == IR_OP_BINARY_OP_TY_UMOD ||
                    op->binary_op.ty == IR_OP_BINARY_OP_TY_SMOD),
               "lower_mod called on invalid op");

  enum ir_op_binary_op_ty div_ty;
  enum ir_op_sign sign = ir_binary_op_sign(op->binary_op.ty);
  switch (sign) {
  case IR_OP_SIGN_NA:
    BUG("trying to `lower_mod` but `binary_op_sign` return `IR_OP_SIGN_NA`");
  case IR_OP_SIGN_SIGNED:
    div_ty = IR_OP_BINARY_OP_TY_SDIV;
    break;
  case IR_OP_SIGN_UNSIGNED:
    div_ty = IR_OP_BINARY_OP_TY_UDIV;
    break;
  }

  // we could directly generate an MSUB here but we instead rely on fusing later

  // c = a / b

  struct ir_op *div =
      ir_insert_before_op(func, op, IR_OP_TY_BINARY_OP, op->var_ty);
  div->binary_op.ty = div_ty;
  div->binary_op.lhs = op->binary_op.lhs;
  div->binary_op.rhs = op->binary_op.rhs;

  // y = c * b

  struct ir_op *mul =
      ir_insert_after_op(func, div, IR_OP_TY_BINARY_OP, op->var_ty);
  mul->binary_op.ty = IR_OP_BINARY_OP_TY_MUL;
  mul->binary_op.lhs = div;
  mul->binary_op.rhs = op->binary_op.rhs;

  // x = a - y

  // Now we replace `op` with `sub` (as `sub` is the op that actually produces
  // the value) this preserves links, as other ops pointing to the div will now
  // point at the sub
  op->ty = IR_OP_TY_BINARY_OP;
  op->var_ty = op->var_ty;
  op->binary_op.ty = IR_OP_BINARY_OP_TY_SUB;
  op->binary_op.lhs = op->binary_op.lhs;
  op->binary_op.rhs = mul;
}

static void lower_comparison(struct ir_func *irb, struct ir_op *op) {
  // FIXME: this should instead look at the `br.cond` and its `cond` operand
  // because looking at `succ` is not reliable

  invariant_assert(op->ty == IR_OP_TY_BINARY_OP &&
                       ir_binary_op_is_comparison(op->binary_op.ty),
                   "non comparison op");

  // mark it as writing to flag reg so register allocator doesn't intefere with
  // it
  op->reg = REG_FLAGS;

  // FIXME: there are other places where we check `op->succ` but we actually
  // want to go to next stmt if needed
  struct ir_op *succ = op->succ ? op->succ : op->stmt->succ->first;

  if (succ && succ->ty == IR_OP_TY_BR_COND) {
    // don't need to insert `mov` because emitter understands how to emit a
    // branch depending on REG_FLAGS
    return;
  }

  // emitter understands how to emit a `mov` from a comparison into REG_FLAGS
  // insert a new op after the current one, move this op into it, then make that
  // op a `mov` this turns all the ops pointing to the branch into pointing to
  // the mov, as we want
  struct ir_op *new_br = ir_insert_before_op(irb, op, op->ty, op->var_ty);
  new_br->binary_op = op->binary_op;
  new_br->reg = REG_FLAGS;

  op->ty = IR_OP_TY_MOV;
  op->mov.value = new_br;
  op->reg = NO_REG;
}

static bool fits_in_alu_imm(unsigned long long value) {
  return value < MAX_IMM_SIZE;
}

static bool can_contain_lcl_addr(struct ir_func *func, struct ir_lcl *lcl,
                                 size_t offset) {

  switch (lcl->alloc_ty) {
  case IR_LCL_ALLOC_TY_NONE:
    return false;
  case IR_LCL_ALLOC_TY_FIXED:
    // TODO: try and contain these, need to carefully work with offset
    return false;
  case IR_LCL_ALLOC_TY_NORMAL: {
    offset += func->caller_stack_needed + lcl->alloc.offset;

    size_t align = ir_var_ty_info(func->unit, &lcl->var_ty).alignment;

    if (offset > MAX_IMM_SIZE || (offset % align)) {
      return false;
    }

    return true;
  }
  }
}

static void try_contain_binary_op(struct ir_func *func, struct ir_op *op) {
  DEBUG_ASSERT(op->ty == IR_OP_TY_BINARY_OP, "expected binary op");

  struct ir_op *lhs = op->binary_op.lhs;
  struct ir_op *rhs = op->binary_op.rhs;

  bool supports_lhs_contained, supports_rhs_contained;
  switch (op->binary_op.ty) {
  case IR_OP_BINARY_OP_TY_EQ:
  case IR_OP_BINARY_OP_TY_NEQ:
  case IR_OP_BINARY_OP_TY_UGT:
  case IR_OP_BINARY_OP_TY_SGT:
  case IR_OP_BINARY_OP_TY_UGTEQ:
  case IR_OP_BINARY_OP_TY_SGTEQ:
  case IR_OP_BINARY_OP_TY_ULT:
  case IR_OP_BINARY_OP_TY_SLT:
  case IR_OP_BINARY_OP_TY_ULTEQ:
  case IR_OP_BINARY_OP_TY_SLTEQ:
    supports_lhs_contained = true;
    supports_rhs_contained = true;
    break;
  case IR_OP_BINARY_OP_TY_FMAX:
  case IR_OP_BINARY_OP_TY_FMIN:
  case IR_OP_BINARY_OP_TY_FEQ:
  case IR_OP_BINARY_OP_TY_FNEQ:
  case IR_OP_BINARY_OP_TY_FGT:
  case IR_OP_BINARY_OP_TY_FGTEQ:
  case IR_OP_BINARY_OP_TY_FLT:
  case IR_OP_BINARY_OP_TY_FLTEQ:
    supports_lhs_contained = true;
    supports_rhs_contained = true;
    break;
  case IR_OP_BINARY_OP_TY_LSHIFT:
  case IR_OP_BINARY_OP_TY_SRSHIFT:
  case IR_OP_BINARY_OP_TY_URSHIFT:
    supports_lhs_contained = false;
    supports_rhs_contained = true;
    break;
  case IR_OP_BINARY_OP_TY_AND:
  case IR_OP_BINARY_OP_TY_OR:
  case IR_OP_BINARY_OP_TY_XOR:
    // TODO: support lgs/rhs immediates in bitwise ops (the immr/imms field is
    // complex)
    supports_lhs_contained = false;
    supports_rhs_contained = false;
    break;
  case IR_OP_BINARY_OP_TY_ADD:
    supports_lhs_contained = true;
    supports_rhs_contained = true;
    break;
  case IR_OP_BINARY_OP_TY_SUB:
    supports_lhs_contained = false;
    supports_rhs_contained = true;
    break;
  case IR_OP_BINARY_OP_TY_MUL:
  case IR_OP_BINARY_OP_TY_SDIV:
  case IR_OP_BINARY_OP_TY_UDIV:
  case IR_OP_BINARY_OP_TY_SMOD:
  case IR_OP_BINARY_OP_TY_UMOD:
    supports_lhs_contained = false;
    supports_rhs_contained = false;
    break;
  case IR_OP_BINARY_OP_TY_FADD:
  case IR_OP_BINARY_OP_TY_FSUB:
  case IR_OP_BINARY_OP_TY_FMUL:
  case IR_OP_BINARY_OP_TY_FDIV:
    supports_lhs_contained = false;
    supports_rhs_contained = false;
    break;
  }

  if (supports_lhs_contained &&
      (lhs->ty == IR_OP_TY_CNST &&
       (ir_var_ty_is_integral(&lhs->var_ty) ||
        (ir_var_ty_is_fp(&lhs->var_ty) && lhs->cnst.flt_value == 0.0l)) &&
       fits_in_alu_imm(lhs->cnst.int_value))) {
    if (ir_binary_op_is_comparison(op->binary_op.ty)) {
      // flip operands and operation

      op->binary_op.ty = ir_flip_binary_comparison(op->binary_op.ty);
      op->binary_op.lhs = op->binary_op.rhs;
      op->binary_op.rhs = ir_alloc_contained_op(func, lhs, op);
    } else {
      op->binary_op.lhs = ir_alloc_contained_op(func, lhs, op);
    }
  } else if (supports_rhs_contained && (rhs->ty == IR_OP_TY_CNST &&
                                        (ir_var_ty_is_integral(&rhs->var_ty) ||
                                         (ir_var_ty_is_fp(&rhs->var_ty) &&
                                          rhs->cnst.flt_value == 0.0l)) &&
                                        fits_in_alu_imm(rhs->cnst.int_value))) {
    op->binary_op.rhs = ir_alloc_contained_op(func, rhs, op);
  }
}

static void try_contain_addr_offset(struct ir_func *func, struct ir_op *op) {
  if (op->flags & IR_OP_FLAG_CONTAINED) {
    return;
  }

  struct ir_op *base = op->addr_offset.base;
  if (base->ty != IR_OP_TY_ADDR || base->addr.ty != IR_OP_ADDR_TY_LCL) {
    return;
  }

  struct ir_lcl *lcl = base->addr.lcl;

  if (!can_contain_lcl_addr(func, lcl, 0)) {
    return;
  }

  op->addr_offset.base = ir_alloc_contained_op(func, base, op);
}

static void try_contain_load(struct ir_func *func, struct ir_op *op) {
  switch (op->load.ty) {
  case IR_OP_LOAD_TY_LCL:
  case IR_OP_LOAD_TY_GLB:
    return;
  case IR_OP_LOAD_TY_ADDR:
    break;
  }

  struct ir_op *addr = op->load.addr;

  if (addr->ty == IR_OP_TY_ADDR && addr->addr.ty == IR_OP_ADDR_TY_LCL &&
      can_contain_lcl_addr(func, addr->addr.lcl, 0)) {
    op->load.addr = ir_alloc_contained_op(func, addr, op);
  } else if (addr->ty == IR_OP_TY_ADDR_OFFSET) {
    struct ir_op_addr_offset addr_offset = addr->addr_offset;

    // FIXME: this will lower e.g `(i32) load.addr [addr.offset %0 + #7]` which
    // is not valid because it needs to be a multiple of the size

    struct ir_op *base = addr->addr_offset.base;
    bool lcl_has_offset = base->ty == IR_OP_TY_ADDR &&
                          base->addr.ty == IR_OP_ADDR_TY_LCL &&
                          (base->addr.lcl->alloc.offset ||
                           (base->addr.lcl->alloc_ty != IR_LCL_ALLOC_TY_FIXED &&
                            func->caller_stack_needed));

    if (base->ty == IR_OP_TY_ADDR && base->addr.ty == IR_OP_ADDR_TY_LCL &&
        !can_contain_lcl_addr(func, base->addr.lcl, addr_offset.offset)) {
      return;
    }

    bool offset_contain = !addr_offset.index;
    bool index_contain =
        !addr_offset.offset && !lcl_has_offset &&
        (addr_offset.scale == 1 ||
         addr_offset.scale == ir_var_ty_info(func->unit, &op->var_ty).size);

    if (offset_contain || index_contain) {
      op->load.addr = ir_alloc_contained_op(func, addr, op);
    }
  }
}

static void try_contain_store(struct ir_func *func, struct ir_op *op) {
  switch (op->store.ty) {
  case IR_OP_STORE_TY_LCL:
  case IR_OP_STORE_TY_GLB:
    return;
  case IR_OP_STORE_TY_ADDR:
    break;
  }

  struct ir_op *addr = op->store.addr;

  if (addr->ty == IR_OP_TY_ADDR && addr->addr.ty == IR_OP_ADDR_TY_LCL &&
      can_contain_lcl_addr(func, addr->addr.lcl, 0)) {
    op->store.addr = ir_alloc_contained_op(func, addr, op);
  } else if (addr->ty == IR_OP_TY_ADDR_OFFSET) {
    struct ir_op_addr_offset addr_offset = addr->addr_offset;

    struct ir_op *base = addr->addr_offset.base;
    bool lcl_has_offset = base->ty == IR_OP_TY_ADDR &&
                          base->addr.ty == IR_OP_ADDR_TY_LCL &&
                          (base->addr.lcl->alloc.offset ||
                           (base->addr.lcl->alloc_ty != IR_LCL_ALLOC_TY_FIXED &&
                            func->caller_stack_needed));

    if (base->ty == IR_OP_TY_ADDR && base->addr.ty == IR_OP_ADDR_TY_LCL &&
        !can_contain_lcl_addr(func, base->addr.lcl, addr_offset.offset)) {
      return;
    }

    bool offset_contain = !addr_offset.index;
    bool index_contain =
        !addr_offset.offset && !lcl_has_offset &&
        (addr_offset.scale == 1 ||
         addr_offset.scale ==
             ir_var_ty_info(func->unit, &op->store.value->var_ty).size);

    if (offset_contain || index_contain) {
      op->store.addr = ir_alloc_contained_op(func, addr, op);
    }
  }
}

static void lower_fp_cnst(struct ir_func *func, struct ir_op *op) {
  // transform into creating an integer, and then mov to float reg

  struct ir_var_ty int_ty;
  unsigned long long int_value;

  DEBUG_ASSERT(ir_var_ty_is_fp(&op->var_ty), "float constant not fp type?");

  switch (op->var_ty.primitive) {
  case IR_VAR_PRIMITIVE_TY_F16: {
    DEBUG_ASSERT(op->cnst.int_value == 0, "can only load f16 for 0");
    int_ty = IR_VAR_TY_I16;
    int_value = 0;

    break;
  }
  case IR_VAR_PRIMITIVE_TY_F32: {
    int_ty = IR_VAR_TY_I32;

    union {
      float f;
      unsigned u;
    } v;
    v.f = (float)op->cnst.flt_value;
    int_value = v.u;

    break;
  }
  case IR_VAR_PRIMITIVE_TY_F64: {
    int_ty = IR_VAR_TY_I64;

    union {
      double d;
      unsigned long long ull;
    } v;
    v.d = (double)op->cnst.flt_value;
    int_value = v.ull;

    break;
  }
  default:
    unreachable();
  }

  struct ir_op *int_mov = ir_insert_before_op(func, op, IR_OP_TY_CNST, int_ty);
  int_mov->cnst =
      (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = int_value};

  op->ty = IR_OP_TY_MOV;
  op->mov = (struct ir_op_mov){.value = int_mov};
}

static bool try_get_hfa_info(const struct ir_var_ty *var_ty,
                             struct ir_var_ty *member_ty, size_t *num_members,
                             size_t *member_size) {
  if (var_ty->ty != IR_VAR_TY_TY_UNION && var_ty->ty != IR_VAR_TY_TY_STRUCT) {
    return false;
  }

  if (var_ty->ty == IR_VAR_TY_TY_UNION) {
    return false;
  }

  if (!var_ty->aggregate.num_fields) {
    return false;
  }

  *member_ty = var_ty->aggregate.fields[0];

  if (!ir_var_ty_is_fp(member_ty)) {
    return false;
  }

  if (var_ty->aggregate.num_fields > 4) {
    return false;
  }

  for (size_t i = 1; i < var_ty->aggregate.num_fields; i++) {
    if (!ir_var_ty_eq(member_ty, &var_ty->aggregate.fields[i])) {
      return false;
    }
  }

  switch (member_ty->primitive) {
  case IR_VAR_PRIMITIVE_TY_F16:
    *member_size = 2;
    break;
  case IR_VAR_PRIMITIVE_TY_F32:
    *member_size = 4;
    break;
  case IR_VAR_PRIMITIVE_TY_F64:
    *member_size = 8;
    break;
  default:
    unreachable();
  }

  *num_members = var_ty->aggregate.num_fields;
  return true;
}

struct ir_func_info aarch64_lower_func_ty(struct ir_func *func,
                                          struct ir_var_func_ty func_ty,
                                          struct ir_op **args,
                                          size_t num_args) {

  struct vector *param_infos = vector_create(sizeof(struct ir_param_info));
  struct vector *params = vector_create(sizeof(struct ir_var_ty));

  struct ir_var_ty ret_ty = *func_ty.ret_ty;
  struct ir_param_info *ret_info;
  if (func_ty.ret_ty->ty == IR_VAR_TY_TY_NONE) {
    ret_ty = IR_VAR_TY_NONE;
    ret_info = NULL;
  } else {
    struct ir_var_ty_info info =
        ir_var_ty_info(func->unit, func_ty.ret_ty->ty == IR_VAR_TY_TY_ARRAY
                                       ? &IR_VAR_TY_POINTER
                                       : func_ty.ret_ty);

    ret_info = aralloc(func->arena, sizeof(*ret_info));

    struct ir_var_ty member_ty;
    size_t num_hfa_members;
    size_t hfa_member_size;
    if (try_get_hfa_info(func_ty.ret_ty, &member_ty, &num_hfa_members,
                         &hfa_member_size)) {
      // nop
      *ret_info = (struct ir_param_info){.ty = IR_PARAM_INFO_TY_REGISTER,
                                         .var_ty = func_ty.ret_ty,
                                         .num_regs = num_hfa_members};

      for (size_t i = 0; i < num_hfa_members; i++) {
        ret_info->regs[i] = (struct ir_param_reg){
            .reg = {.ty = IR_REG_TY_FP, .idx = i}, .size = hfa_member_size};
      }
    } else if (info.size > 16) {
      ret_ty = IR_VAR_TY_NONE;

      *ret_info = (struct ir_param_info){
          .ty = IR_PARAM_INFO_TY_POINTER,
          .var_ty = func_ty.ret_ty,
          .num_regs = 1,
          // macOS/linux implicit ret is x8
          // NOTE: on windows it is x0
          .regs[0] = {.reg = {.ty = IR_REG_TY_INTEGRAL, .idx = 8}, .size = 8},
      };

      vector_push_front(params, &IR_VAR_TY_POINTER);
    } else if (ir_var_ty_is_fp(func_ty.ret_ty)) {
      *ret_info = (struct ir_param_info){
          .ty = IR_PARAM_INFO_TY_REGISTER,
          .var_ty = func_ty.ret_ty,
          .num_regs = 1,
          .regs[0] = {.reg = {.ty = IR_REG_TY_FP, .idx = 0}, .size = info.size},
      };
    } else {
      size_t num_regs = (info.size + 7) / 8;
      *ret_info = (struct ir_param_info){
          .ty = IR_PARAM_INFO_TY_REGISTER,
          .var_ty = func_ty.ret_ty,
          .num_regs = num_regs,
      };

      for (size_t i = 0; i < num_regs; i++) {
        ret_info->regs[i] = (struct ir_param_reg){
            .reg = {.ty = IR_REG_TY_INTEGRAL, .idx = i}, .size = 8};
      }
    }
  }

  // NOTE: on windows, SIMD&FP registers are not used in variadics, ever

  bool variadics_on_stack;
  switch (func->unit->target->target_id) {
  case TARGET_ID_AARCH64_MACOS:
    variadics_on_stack = true;
    break;
  default:
    variadics_on_stack = false;
    break;
  }

  size_t ngrn = 0;
  size_t nsrn = 0;
  size_t nsaa = 0;

  size_t num = MAX(func_ty.num_params, num_args);
  for (size_t i = 0; i < num; i++) {
    const struct ir_var_ty *var_ty;

    bool variadic;
    if (i < func_ty.num_params) {
      var_ty = &func_ty.params[i];
      variadic = false;
    } else {
      var_ty = &args[i]->var_ty;
      variadic = func_ty.flags & IR_VAR_FUNC_TY_FLAG_VARIADIC;
    }

    enum ir_param_info_ty ty = IR_PARAM_INFO_TY_REGISTER;

    if (var_ty->ty == IR_VAR_TY_TY_ARRAY) {
      var_ty = &IR_VAR_TY_POINTER;
    }

    struct ir_var_ty_info info = ir_var_ty_info(func->unit, var_ty);

    size_t num_hfa_members;
    size_t hfa_member_size;
    struct ir_var_ty member_ty;
    bool is_hfa = try_get_hfa_info(var_ty, &member_ty, &num_hfa_members,
                                   &hfa_member_size);

    if (info.size > 16 && !is_hfa) {
      // copy to mem
      var_ty = &IR_VAR_TY_POINTER;
      ty = IR_PARAM_INFO_TY_POINTER;
      info = ir_var_ty_info(func->unit, var_ty);
    }

    if (ir_var_ty_is_aggregate(var_ty)) {
      info.size = ROUND_UP(info.size, 8);
    }

    if (!variadic || !variadics_on_stack) {
      if (ir_var_ty_is_fp(var_ty) && nsrn < 8) {
        vector_push_back(params, var_ty);

        struct ir_param_info param_info = {
            .ty = IR_PARAM_INFO_TY_REGISTER,
            .var_ty = var_ty,
            .num_regs = 1,
            .regs[0] = {.reg = {.ty = IR_REG_TY_FP, .idx = nsrn},
                        .size = info.size},
        };

        vector_push_back(param_infos, &param_info);
        nsrn++;
        continue;
      } else if (is_hfa) {
        if (nsrn + num_hfa_members <= 8) {
          struct ir_param_info param_info = {
              .ty = IR_PARAM_INFO_TY_REGISTER,
              .var_ty = var_ty,
              .num_regs = num_hfa_members,
          };

          for (size_t j = 0; j < num_hfa_members; j++) {
            // given this is a composite, we assume `source` contains a
            // pointer to it

            param_info.regs[j] = (struct ir_param_reg){
                .reg = {.ty = IR_REG_TY_FP, .idx = nsrn + j},
                .size = hfa_member_size};

            vector_push_back(params, &member_ty);
          }

          vector_push_back(param_infos, &param_info);

          nsrn += num_hfa_members;
          continue;
        }

        nsrn = 8;
        size_t nsaa_align = MAX(8, info.alignment);
        size_t size = ROUND_UP(info.size, nsaa_align);

        struct ir_param_info param_info = {.ty = IR_PARAM_INFO_TY_STACK,
                                           .var_ty = var_ty,
                                           .stack_offset = nsaa};
        vector_push_back(param_infos, &param_info);

        nsaa += size;
        continue;

      } else if (ir_var_ty_is_integral(var_ty) && info.size <= 8 && ngrn < 8) {
        vector_push_back(params, var_ty);

        struct ir_param_info param_info = {
            .ty = ty,
            .var_ty = var_ty,
            .num_regs = 1,
            .regs[0] = {.reg = {.ty = IR_REG_TY_INTEGRAL, .idx = ngrn},
                        .size = info.size}};
        vector_push_back(param_infos, &param_info);

        ngrn++;

        continue;
      }

      if (info.alignment == 16) {
        ngrn = (ngrn + 1) & 1;
      }

      if (ir_var_ty_is_integral(var_ty) && info.size == 16 && ngrn < 7) {
        // // lo to ngrn, hi to ngrn+1

        vector_push_back(params, &IR_VAR_TY_I64);
        vector_push_back(params, &IR_VAR_TY_I64);

        struct ir_param_info param_info = {
            .ty = IR_PARAM_INFO_TY_REGISTER,
            .var_ty = var_ty,
            .num_regs = 2,
            .regs[0] = {.reg = {.ty = IR_REG_TY_INTEGRAL, .idx = ngrn},
                        .size = 8},
            .regs[1] = {.reg = {.ty = IR_REG_TY_INTEGRAL, .idx = ngrn + 1},
                        .size = 8},
        };
        vector_push_back(param_infos, &param_info);

        ngrn += 2;
        continue;
      }

      size_t dw_size = (info.size + 7) / 8;
      if (ir_var_ty_is_aggregate(var_ty) && dw_size <= (8 - ngrn)) {
        struct ir_param_info param_info = {
            .ty = ty,
            .var_ty = var_ty,
            .num_regs = dw_size,
        };

        for (size_t j = 0; j < dw_size; j++) {
          // given this is a composite, we assume `source` contains a
          // pointer to it
          param_info.regs[j] = (struct ir_param_reg){
              .reg = {.ty = IR_REG_TY_INTEGRAL, .idx = ngrn + j}, .size = 8};

          vector_push_back(params, &IR_VAR_TY_I64);
        }

        vector_push_back(param_infos, &param_info);

        ngrn += dw_size;
        continue;
      }
    }

    ngrn = 8;
    size_t nsaa_align = MAX(8, info.alignment);
    nsaa = ROUND_UP(nsaa, nsaa_align);

    if (ir_var_ty_is_aggregate(var_ty)) {
      struct ir_param_info param_info = {
          .ty = IR_PARAM_INFO_TY_STACK, .var_ty = var_ty, .stack_offset = nsaa};
      vector_push_back(param_infos, &param_info);

      nsaa += info.size;
      continue;
    }

    size_t size = MAX(8, info.size);

    struct ir_param_info param_info = {
        .ty = IR_PARAM_INFO_TY_STACK, .var_ty = var_ty, .stack_offset = nsaa};
    vector_push_back(param_infos, &param_info);

    nsaa += size;
  }

  struct ir_var_func_ty new_func_ty = {
      .flags = func_ty.flags, .ret_ty = aralloc(func->arena, sizeof(ret_ty))};

  *new_func_ty.ret_ty = ret_ty;

  struct ir_call_info call_info = {.ret = ret_info,
                                   .stack_size = nsaa,
                                   .num_gp_used = ngrn,
                                   .num_fp_used = nsrn};

  CLONE_AND_FREE_VECTOR(func->arena, params, new_func_ty.num_params,
                        new_func_ty.params);
  CLONE_AND_FREE_VECTOR(func->arena, param_infos, call_info.num_params,
                        call_info.params);

  return (struct ir_func_info){.func_ty = new_func_ty, .call_info = call_info};
}

static void lower_va_start_macos(struct ir_func *func, struct ir_op *op) {
  // TODO: replace the magic 'IR_LCL_FLAG_PARAM and negative offset' with a
  // proper "offset" type that can be pos/neg relative to sp or fp

  struct ir_lcl *stack_base = ir_add_local(func, &IR_VAR_TY_POINTER);
  stack_base->alloc_ty = IR_LCL_ALLOC_TY_FIXED;
  stack_base->alloc = (struct ir_lcl_alloc){.offset = 0};

  stack_base->flags |= IR_LCL_FLAG_PARAM;

  struct ir_op *addr = op->va_start.list_addr;

  struct ir_op *sp = ir_replace_op(func, op, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
  sp->addr = (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = stack_base};

  struct ir_op *value;
  if (func->call_info.stack_size) {
    value =
        ir_insert_after_op(func, sp, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
    value->addr_offset = (struct ir_op_addr_offset){
        .base = sp, .offset = func->call_info.stack_size};
  } else {
    value = sp;
  }

  struct ir_op *store =
      ir_insert_after_op(func, value, IR_OP_TY_STORE, IR_VAR_TY_NONE);
  store->store = (struct ir_op_store){
      .ty = IR_OP_STORE_TY_ADDR, .addr = addr, .value = sp};
}

struct ir_aarch64_save_info {
  struct ir_lcl *lcl;
  size_t gp_save_sz;
  size_t fp_save_sz;
};

static struct ir_aarch64_save_info lower_va_args_generic(struct ir_func *func) {
  struct ir_call_info info = func->call_info;

  size_t gp_save_sz = ((8 - info.num_gp_used) * 8);
  size_t fp_save_sz = ((8 - info.num_gp_used) * 16);
  size_t save_sz = gp_save_sz + fp_save_sz;

  DEBUG_ASSERT(save_sz % 8 == 0, "because we use type I64 for alignment, "
                                 "save_sz must be divisible by 8");
  save_sz = save_sz / 8;

  struct ir_var_ty *el_ty =
      aralloc_init(func->arena, sizeof(*el_ty), &IR_VAR_TY_I64);
  struct ir_var_ty save_ty = {.ty = IR_VAR_TY_TY_ARRAY,
                              .array = {
                                  // TODO: make constant ptr
                                  .underlying = el_ty,
                                  .num_elements = save_sz,
                              }};

  struct ir_lcl *lcl = ir_add_local(func, &save_ty);

  struct ir_op *movs = func->first->first->last;
  struct ir_op *base =
      ir_insert_after_op(func, movs, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
  base->addr = (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = lcl};

  struct ir_op *last = base;

  // need to save all the registers in args
  for (size_t i = info.num_gp_used, idx = 0; i < 8; i++, idx++) {
    struct ir_op *addr;

    if (i) {
      addr = ir_insert_after_op(func, last, IR_OP_TY_ADDR_OFFSET,
                                IR_VAR_TY_POINTER);
      addr->addr_offset =
          (struct ir_op_addr_offset){.base = base, .offset = idx * 8};
    } else {
      addr = base;
    }

    struct ir_op *value =
        ir_insert_after_op(func, movs, IR_OP_TY_MOV, IR_VAR_TY_I64);
    value->flags |= IR_OP_FLAG_PARAM | IR_OP_FLAG_FIXED_REG;
    value->mov = (struct ir_op_mov){.value = NULL};
    value->reg = (struct ir_reg){.ty = IR_REG_TY_INTEGRAL, .idx = i};
    movs = value;

    struct ir_op *save =
        ir_insert_after_op(func, addr, IR_OP_TY_STORE, IR_VAR_TY_NONE);
    save->store = (struct ir_op_store){
        .ty = IR_OP_STORE_TY_ADDR, .addr = addr, .value = value};

    last = save;
  }

  for (size_t i = info.num_fp_used, idx = 0; i < 8; i++, idx++) {
    // TODO: save vector

    struct ir_op *addr =
        ir_insert_after_op(func, last, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
    addr->addr_offset = (struct ir_op_addr_offset){
        .base = base, .offset = gp_save_sz + (idx * 16)};

    struct ir_op *value =
        ir_insert_after_op(func, movs, IR_OP_TY_MOV, IR_VAR_TY_F64);
    value->flags |= IR_OP_FLAG_PARAM | IR_OP_FLAG_FIXED_REG;
    value->mov = (struct ir_op_mov){.value = NULL};
    value->reg = (struct ir_reg){.ty = IR_REG_TY_FP, .idx = i};
    movs = value;

    struct ir_op *save =
        ir_insert_after_op(func, addr, IR_OP_TY_STORE, IR_VAR_TY_NONE);
    save->store = (struct ir_op_store){
        .ty = IR_OP_STORE_TY_ADDR, .addr = addr, .value = value};

    last = save;
  }

  return (struct ir_aarch64_save_info){
      .lcl = lcl, .gp_save_sz = gp_save_sz, .fp_save_sz = fp_save_sz};
}

static void lower_va_start_generic(struct ir_func *func,
                                   struct ir_aarch64_save_info save_info,
                                   struct ir_op *op) {
  // The va_start macro initializes the structure as follows:
  //
  //   * __stack
  //     - set to the address following the last (highest addressed) named
  //     incoming argument on the stack, rounded upwards to a multiple of 8
  //     bytes, or if there are no named arguments on the stack, then the value
  //     of the stack pointer when the function was entered.
  //
  //   * __gr_top
  //     - set to the address of the byte immediately following the general
  //     register argument save area, the end of the save area being aligned to
  //     a 16 byte boundary.
  //
  //   * __vr_top
  //     - set to the address of the byte immediately following the FP/SIMD
  //     register argument save area, the end of the save area being aligned to
  //     a 16 byte boundary.
  //
  //   * __gr_offs
  //     - set to 0 – ((8 – named_gr) * 8).
  //
  //   * __vr_offs
  //     - set to 0 – ((8 – named_vr) * 16).

  DEBUG_ASSERT(save_info.lcl,
               "expected save_lcl to exist (maybe "
               "IR_FUNC_FLAG_USES_VA_ARGS was not set correctly)");

  struct ir_lcl *stack_base = ir_add_local(func, &IR_VAR_TY_POINTER);
  stack_base->alloc_ty = IR_LCL_ALLOC_TY_FIXED;
  stack_base->alloc = (struct ir_lcl_alloc){.offset = 0};

  stack_base->flags |= IR_LCL_FLAG_PARAM;

  struct ir_op *addr = op->va_start.list_addr;

  struct ir_op *sp = ir_replace_op(func, op, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
  sp->addr = (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = stack_base};

  struct ir_call_info info = func->call_info;

  struct ir_op *last = sp;

  // OPT: don't need to initialize vr_top/vr_offs if we know no FP registers are
  // ever used from `va_arg`

  {
    // list->stack = fp + <stack space used by named args>

    size_t stack_offset = info.stack_size;

    struct ir_op *stack_value;
    if (stack_offset) {
      stack_value = ir_insert_after_op(func, last, IR_OP_TY_ADDR_OFFSET,
                                       IR_VAR_TY_POINTER);
      stack_value->addr_offset =
          (struct ir_op_addr_offset){.base = sp, .offset = stack_offset};
    } else {
      stack_value = sp;
    }

    struct ir_op *store_stack =
        ir_insert_after_op(func, stack_value, IR_OP_TY_STORE, IR_VAR_TY_NONE);
    store_stack->store = (struct ir_op_store){
        .ty = IR_OP_STORE_TY_ADDR, .addr = addr, .value = stack_value};

    last = store_stack;
  }

  {
    // list->gr_top = <gp_save + gp_size>

    struct ir_op *gp_save_base =
        ir_insert_after_op(func, last, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
    gp_save_base->addr =
        (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = save_info.lcl};

    struct ir_op *gp_save_end = ir_insert_after_op(
        func, gp_save_base, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
    gp_save_end->addr_offset = (struct ir_op_addr_offset){
        .base = gp_save_base, .offset = save_info.gp_save_sz};

    struct ir_op *addr_gr_top = ir_insert_after_op(
        func, gp_save_end, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
    addr_gr_top->addr_offset =
        (struct ir_op_addr_offset){.base = addr, .offset = 8};

    struct ir_op *store_gr_top =
        ir_insert_after_op(func, addr_gr_top, IR_OP_TY_STORE, IR_VAR_TY_NONE);
    store_gr_top->store = (struct ir_op_store){
        .ty = IR_OP_STORE_TY_ADDR, .addr = addr_gr_top, .value = gp_save_end};

    last = store_gr_top;
  }

  {
    // list->vr_top = <fp_save + fp_size> (which is <gp_save + gp_size +
    // fp_size>)
    struct ir_op *fp_save_base =
        ir_insert_after_op(func, last, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
    fp_save_base->addr =
        (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = save_info.lcl};

    struct ir_op *fp_save_end = ir_insert_after_op(
        func, fp_save_base, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
    fp_save_end->addr_offset = (struct ir_op_addr_offset){
        .base = fp_save_base,
        .offset = save_info.gp_save_sz + save_info.fp_save_sz};

    struct ir_op *addr_vr_top = ir_insert_after_op(
        func, fp_save_end, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
    addr_vr_top->addr_offset =
        (struct ir_op_addr_offset){.base = addr, .offset = 16};

    struct ir_op *store_vr_top =
        ir_insert_after_op(func, addr_vr_top, IR_OP_TY_STORE, IR_VAR_TY_NONE);
    store_vr_top->store = (struct ir_op_store){
        .ty = IR_OP_STORE_TY_ADDR, .addr = addr_vr_top, .value = fp_save_end};

    last = store_vr_top;
  }

  {
    // list->gr_offs = 0 – ((8 – named_gr) * 8)

    ssize_t gr_offs = 0 - ((8 - info.num_gp_used) * 8);

    struct ir_op *gr_off_value =
        ir_insert_after_op(func, last, IR_OP_TY_CNST, IR_VAR_TY_POINTER);
    ir_mk_integral_constant(func->unit, gr_off_value, IR_VAR_PRIMITIVE_TY_I32,
                            (unsigned long long)gr_offs);

    struct ir_op *gr_off_addr = ir_insert_after_op(
        func, gr_off_value, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
    gr_off_addr->addr_offset =
        (struct ir_op_addr_offset){.base = addr, .offset = 24};

    struct ir_op *store_gr_off =
        ir_insert_after_op(func, gr_off_addr, IR_OP_TY_STORE, IR_VAR_TY_NONE);
    store_gr_off->store = (struct ir_op_store){
        .ty = IR_OP_STORE_TY_ADDR, .addr = gr_off_addr, .value = gr_off_value};

    last = store_gr_off;
  }

  {
    // list->vr_offs = 0 – ((8 – named_vr) * 16)

    ssize_t vr_offs = 0 - ((8 - info.num_fp_used) * 16);

    struct ir_op *vr_off_value =
        ir_insert_after_op(func, last, IR_OP_TY_CNST, IR_VAR_TY_POINTER);
    ir_mk_integral_constant(func->unit, vr_off_value, IR_VAR_PRIMITIVE_TY_I32,
                            (unsigned long long)vr_offs);

    struct ir_op *vr_off_addr = ir_insert_after_op(
        func, vr_off_value, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
    vr_off_addr->addr_offset =
        (struct ir_op_addr_offset){.base = addr, .offset = 28};

    struct ir_op *store_vr_off =
        ir_insert_after_op(func, vr_off_addr, IR_OP_TY_STORE, IR_VAR_TY_NONE);
    store_vr_off->store = (struct ir_op_store){
        .ty = IR_OP_STORE_TY_ADDR, .addr = vr_off_addr, .value = vr_off_value};

    last = store_vr_off;
  }
}

static struct ir_aarch64_save_info lower_va_args(struct ir_func *func) {
  switch (func->unit->target->target_id) {
  case TARGET_ID_AARCH64_MACOS:
    // nop
    return (struct ir_aarch64_save_info){0};
  default:
    return lower_va_args_generic(func);
  }
}

static void lower_va_start(struct ir_func *func,
                           struct ir_aarch64_save_info save_info,
                           struct ir_op *op) {
  switch (func->unit->target->target_id) {
  case TARGET_ID_AARCH64_MACOS:
    lower_va_start_macos(func, op);
    break;
  default:
    lower_va_start_generic(func, save_info, op);
    break;
  }
}

static void lower_va_arg(struct ir_func *func, struct ir_op *op) {
  switch (func->unit->target->target_id) {
  case TARGET_ID_AARCH64_MACOS: {
    // because a `va_arg` op takes the _address_ of the va_arg list, we can
    // update it

    struct ir_var_ty var_ty = op->va_arg.arg_ty;

    struct ir_op *addr = op->va_arg.list_addr;

    struct ir_op *list =
        ir_insert_before_op(func, op, IR_OP_TY_LOAD, IR_VAR_TY_POINTER);
    list->load = (struct ir_op_load){.ty = IR_OP_LOAD_TY_ADDR, .addr = addr};

    struct ir_op *load = ir_replace_op(func, op, IR_OP_TY_LOAD, var_ty);
    load->load = (struct ir_op_load){.ty = IR_OP_LOAD_TY_ADDR, .addr = list};

    size_t size = ir_var_ty_info(func->unit, &var_ty).size;

    struct ir_op *offset =
        ir_insert_after_op(func, load, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
    offset->addr_offset =
        (struct ir_op_addr_offset){.base = list, .offset = ROUND_UP(size, 8)};

    struct ir_op *store =
        ir_insert_after_op(func, offset, IR_OP_TY_STORE, IR_VAR_TY_NONE);
    store->store = (struct ir_op_store){
        .ty = IR_OP_STORE_TY_ADDR, .addr = addr, .value = offset};

    break;
  }
  default:
    TODO("linux va arg");
    break;
  }
}

void aarch64_lower_variadic(struct ir_func *func) {
  struct ir_aarch64_save_info save_info = {0};
  if (func->flags & IR_FUNC_FLAG_USES_VA_ARGS) {
    save_info = lower_va_args(func);
  }

  struct ir_func_iter iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);

  struct ir_op *op;
  while (ir_func_iter_next(&iter, &op)) {
    switch (op->ty) {
    case IR_OP_TY_VA_START:
      lower_va_start(func, save_info, op);
      break;
    case IR_OP_TY_VA_ARG:
      lower_va_arg(func, op);
      break;
    default:
      break;
    }
  }
}

void aarch64_lower(struct ir_unit *unit) {
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

      {
        // lower i128 to 2xi64
        struct ir_func_iter iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);

        struct ir_op *op;
        while (ir_func_iter_next(&iter, &op)) {
          if (op->ty == IR_OP_TY_CAST_OP &&
              ir_var_ty_is_primitive(&op->var_ty, IR_VAR_PRIMITIVE_TY_I64) &&
              ir_var_ty_is_primitive(&op->cast_op.value->var_ty,
                                     IR_VAR_PRIMITIVE_TY_I64)) {
            struct ir_op *value = op->cast_op.value;
            op->ty = IR_OP_TY_MOV;
            op->mov = (struct ir_op_mov){.value = value};
          }

          if (op->var_ty.ty != IR_VAR_TY_TY_PRIMITIVE ||
              op->var_ty.primitive != IR_VAR_PRIMITIVE_TY_I128) {
            continue;
          }

          // temp super horrendous hack: just make i64
          op->var_ty = IR_VAR_TY_I64;

          // switch (op->ty) {
          // case IR_OP_TY_LOAD:
          //   break;
          // case IR_OP_TY_STORE:
          //   break;
          // default:
          //   break;
          //   // BUG("unrecognised op producing i128");
          // }
        }
      }

      struct ir_basicblock *basicblock = func->first;
      while (basicblock) {
        struct ir_stmt *stmt = basicblock->first;

        while (stmt) {
          struct ir_op *op = stmt->first;

          while (op) {
            switch (op->ty) {
            case IR_OP_TY_CNST: {
              if (op->cnst.ty == IR_OP_CNST_TY_FLT) {
                lower_fp_cnst(func, op);
                break;
              }

              DEBUG_ASSERT(!ir_var_ty_is_fp(&op->var_ty),
                           "int-ty cnst but fp var ty");

              break;
            }
            case IR_OP_TY_BITFIELD_INSERT:
              op->bitfield_insert.value->flags |= IR_OP_FLAG_READS_DEST;
              break;
            case IR_OP_TY_UNARY_OP:
              if (op->unary_op.ty == IR_OP_UNARY_OP_TY_LOGICAL_NOT) {
                lower_logical_not(func, op);
              } else if (op->unary_op.ty == IR_OP_UNARY_OP_TY_POPCNT) {
                lower_popcnt(func, op);
              }
              break;
            case IR_OP_TY_BINARY_OP:
              switch (op->binary_op.ty) {
              case IR_OP_BINARY_OP_TY_UMOD:
              case IR_OP_BINARY_OP_TY_SMOD:
                lower_mod(func, op);
                break;
              case IR_OP_BINARY_OP_TY_SRSHIFT:
              case IR_OP_BINARY_OP_TY_LSHIFT:
              case IR_OP_BINARY_OP_TY_URSHIFT:
                lower_shift(func, op);
                break;
              default:
                break;
              }
              break;
            default:
              break;
            }

            op = op->succ;
          }

          stmt = stmt->succ;
        }

        basicblock = basicblock->succ;
      }

      // now we lower comparisons as the above can generate them (via
      // logical_not)
      basicblock = func->first;
      while (basicblock) {
        struct ir_stmt *stmt = basicblock->first;

        while (stmt) {
          struct ir_op *op = stmt->first;

          while (op) {
            switch (op->ty) {
            case IR_OP_TY_STORE:
              try_contain_store(func, op);
              break;
            case IR_OP_TY_LOAD:
              try_contain_load(func, op);
              break;
            case IR_OP_TY_BINARY_OP:
              try_contain_binary_op(func, op);

              if (ir_binary_op_is_comparison(op->binary_op.ty)) {
                lower_comparison(func, op);
              }
              break;
            case IR_OP_TY_ADDR_OFFSET:
              if (op->addr_offset.index &&
                  (popcntl(op->addr_offset.scale) != 1 ||
                   op->addr_offset.scale > 8)) {
                // do mul beforehand and set scale to 1
                struct ir_op *cnst = ir_insert_before_op(
                    func, op, IR_OP_TY_BINARY_OP, IR_VAR_TY_POINTER);
                ir_mk_integral_constant(unit, cnst, IR_VAR_PRIMITIVE_TY_I64,
                                        op->addr_offset.scale);

                struct ir_op *mul = ir_insert_before_op(
                    func, op, IR_OP_TY_BINARY_OP, IR_VAR_TY_POINTER);
                mul->binary_op =
                    (struct ir_op_binary_op){.ty = IR_OP_BINARY_OP_TY_MUL,
                                             .lhs = op->addr_offset.index,
                                             .rhs = cnst};

                op->addr_offset.scale = 1;
                op->addr_offset.index = mul;
              } else {
                try_contain_addr_offset(func, op);
              }
              break;
            default:
              break;
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
