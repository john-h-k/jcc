#include "lower.h"

#include "../aarch64.h"
#include "../lower.h"
#include "../util.h"
#include "../vector.h"

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
static void lower_shift(UNUSED struct ir_func *func, struct ir_op *op) {
  struct ir_op_binary_op *binary_op = &op->binary_op;

  binary_op->rhs->var_ty = binary_op->lhs->var_ty;
}

// ARM has no quotient function
// so instead of `x = a % b` we do
// `c = a / b; x = a - (c * b)`
static void lower_quot(struct ir_func *func, struct ir_op *op) {
  DEBUG_ASSERT(op->ty == IR_OP_TY_BINARY_OP &&
                   (op->binary_op.ty == IR_OP_BINARY_OP_TY_UQUOT ||
                    op->binary_op.ty == IR_OP_BINARY_OP_TY_SQUOT),
               "lower_quot called on invalid op");

  enum ir_op_binary_op_ty div_ty;
  enum ir_op_sign sign = ir_binary_op_sign(op->binary_op.ty);
  switch (sign) {
  case IR_OP_SIGN_NA:
    BUG("trying to `lower_quot` but `binary_op_sign` return `IR_OP_SIGN_NA`");
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

//   // look for store after, in case this is a copy
//   // FIXME: not sure if this is perfect logic (could there be ops in
//   between?) struct ir_op *nxt_store = op->succ;

//   if (!nxt_store || nxt_store->ty != IR_OP_TY_STORE_LCL) {
//     return;
//   }

//   bool simple_copy = true;
//   enum ir_var_primitive_ty simple_copy_ty;
//   switch (info.size) {
//   case 1:
//     simple_copy_ty = IR_VAR_PRIMITIVE_TY_I8;
//     break;
//   case 2:
//     simple_copy_ty = IR_VAR_PRIMITIVE_TY_I16;
//     break;
//   case 4:
//     simple_copy_ty = IR_VAR_PRIMITIVE_TY_I32;
//     break;
//   case 8:
//     simple_copy_ty = IR_VAR_PRIMITIVE_TY_I64;
//     break;

//   default:
//     simple_copy = false;
//   }

//   if (simple_copy) {
//     op->var_ty = (struct ir_var_ty){.ty = IR_VAR_TY_TY_PRIMITIVE,
//                                     .primitive = simple_copy_ty};
//     return;
//   }

//   if (info.size < MAX_REG_SIZE) {
//     todo("non-pow2 copies < MAX_REG_SIZE");
//   }

//   struct ir_var_ty copy_ty = var_ty_for_pointer_size(func->unit);

//   struct ir_lcl *src_lcl = op->load_lcl.lcl;
//   struct ir_lcl *dest_lcl = nxt_store->store.lcl;

//   struct ir_op *base_src_addr = op;
//   struct ir_op *base_dest_addr = nxt_store;

//   base_src_addr->ty = IR_OP_TY_ADDR;
//   base_src_addr->var_ty = copy_ty;
//   base_src_addr->addr =
//       (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = src_lcl};

//   base_dest_addr->ty = IR_OP_TY_ADDR;
//   base_dest_addr->var_ty = copy_ty;
//   base_dest_addr->addr =
//       (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = dest_lcl};

//   struct ir_op *last = base_dest_addr;

//   size_t size_left = info.size;
//   size_t offset = 0;
//   while (size_left >= MAX_REG_SIZE) {
//     struct ir_op *offset_cnst =
//         insert_after_ir_op(func, last, IR_OP_TY_CNST, copy_ty);
//     make_pointer_constant(func->unit, offset_cnst, offset);

//     struct ir_op *src_addr =
//         insert_after_ir_op(func, offset_cnst, IR_OP_TY_BINARY_OP, copy_ty);
//     src_addr->binary_op = (struct ir_op_binary_op){
//         .ty = IR_OP_BINARY_OP_TY_ADD, .lhs = base_src_addr, .rhs =
//         offset_cnst};

//     struct ir_op *load =
//         insert_after_ir_op(func, src_addr, IR_OP_TY_LOAD_ADDR, copy_ty);
//     load->load_addr = (struct ir_op_load_addr){.addr = src_addr};

//     struct ir_op *dest_addr =
//         insert_after_ir_op(func, load, IR_OP_TY_BINARY_OP, copy_ty);
//     dest_addr->binary_op =
//         (struct ir_op_binary_op){.ty = IR_OP_BINARY_OP_TY_ADD,
//                                  .lhs = base_dest_addr,
//                                  .rhs = offset_cnst};

//     struct ir_op *store = insert_after_ir_op(
//         func, dest_addr, IR_OP_TY_STORE_ADDR, IR_VAR_TY_NONE);
//     store->store_addr =
//         (struct ir_op_store_addr){.addr = dest_addr, .value = load};

//     last = store;
//     size_left -= MAX_REG_SIZE;
//     offset += MAX_REG_SIZE;
//   }

//   // now we have to do the last trailing load
//   // because size is >= MAX_REG_SIZE,
//   // we can just do a whole-reg copy starting from end-MAX_REG_SIZE
//   if (size_left) {
//     size_t trailing_offset = MAX_REG_SIZE - size_left;

//     struct ir_op *offset_cnst =
//         insert_after_ir_op(func, last, IR_OP_TY_CNST, copy_ty);
//     make_pointer_constant(func->unit, offset_cnst, trailing_offset);

//     struct ir_op *src_addr =
//         insert_after_ir_op(func, offset_cnst, IR_OP_TY_BINARY_OP, copy_ty);
//     src_addr->binary_op = (struct ir_op_binary_op){
//         .ty = IR_OP_BINARY_OP_TY_ADD, .lhs = base_src_addr, .rhs =
//         offset_cnst};

//     struct ir_op *load =
//         insert_after_ir_op(func, src_addr, IR_OP_TY_LOAD_ADDR, copy_ty);
//     load->load_addr = (struct ir_op_load_addr){.addr = src_addr};

//     struct ir_op *dest_addr =
//         insert_after_ir_op(func, load, IR_OP_TY_BINARY_OP, copy_ty);
//     dest_addr->binary_op =
//         (struct ir_op_binary_op){.ty = IR_OP_BINARY_OP_TY_ADD,
//                                  .lhs = base_dest_addr,
//                                  .rhs = offset_cnst};

//     struct ir_op *store = insert_after_ir_op(
//         func, dest_addr, IR_OP_TY_STORE_ADDR, IR_VAR_TY_NONE);
//     store->store_addr =
//         (struct ir_op_store_addr){.addr = dest_addr, .value = load};
//   }
// }

static bool fits_in_alu_imm(unsigned long long value) {
  return value < MAX_IMM_SIZE;
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
  case IR_OP_BINARY_OP_TY_SQUOT:
  case IR_OP_BINARY_OP_TY_UQUOT:
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
        (ir_var_ty_is_fp(&lhs->var_ty) && lhs->cnst.flt_value == 0.0)) &&
       fits_in_alu_imm(lhs->cnst.int_value))) {
    if (ir_binary_op_is_comparison(op->binary_op.ty)) {
      // flip operands and operation

      op->binary_op.ty = ir_flip_binary_comparison(op->binary_op.ty);
      op->binary_op.lhs = op->binary_op.rhs;
      op->binary_op.rhs = ir_alloc_contained_op(func, lhs, op);
    } else {
      op->binary_op.lhs = ir_alloc_contained_op(func, lhs, op);
    }
  } else if (supports_rhs_contained &&
             (rhs->ty == IR_OP_TY_CNST &&
              (ir_var_ty_is_integral(&rhs->var_ty) ||
               (ir_var_ty_is_fp(&rhs->var_ty) && rhs->cnst.flt_value == 0.0)) &&
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

  if (addr->ty == IR_OP_TY_ADDR && addr->addr.ty == IR_OP_ADDR_TY_LCL) {
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

  if (addr->ty == IR_OP_TY_ADDR && addr->addr.ty == IR_OP_ADDR_TY_LCL) {
    op->store.addr = ir_alloc_contained_op(func, addr, op);
  } else if (addr->ty == IR_OP_TY_ADDR_OFFSET) {
    struct ir_op_addr_offset addr_offset = addr->addr_offset;

    struct ir_op *base = addr->addr_offset.base;
    bool lcl_has_offset = base->ty == IR_OP_TY_ADDR &&
                          base->addr.ty == IR_OP_ADDR_TY_LCL &&
                          (base->addr.lcl->alloc.offset ||
                           (base->addr.lcl->alloc_ty != IR_LCL_ALLOC_TY_FIXED &&
                            func->caller_stack_needed));

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

static bool try_get_hfa_info(struct ir_func *func,
                             const struct ir_var_ty *var_ty,
                             struct ir_var_ty *member_ty, size_t *num_members,
                             size_t *member_size) {
  if (var_ty->ty != IR_VAR_TY_TY_UNION && var_ty->ty != IR_VAR_TY_TY_STRUCT) {
    return false;
  }

  if (var_ty->ty == IR_VAR_TY_TY_UNION) {
    TODO("union hfa handling");
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
    if (!ir_var_ty_eq(func->unit, member_ty, &var_ty->aggregate.fields[i])) {
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

  size_t ngrn = 0;
  size_t nsrn = 0;
  size_t nsaa = 0;

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

    ret_info = arena_alloc(func->arena, sizeof(*ret_info));

    struct ir_var_ty member_ty;
    size_t num_hfa_members, hfa_member_size;
    if (try_get_hfa_info(func, func_ty.ret_ty, &member_ty, &num_hfa_members,
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
          .regs[0] = {.reg = {.ty = IR_REG_TY_INTEGRAL, .idx = 0}, .size = 8},
      };

      ngrn++;
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

    if (info.size > 16) {
      // copy to mem
      var_ty = &IR_VAR_TY_POINTER;
      ty = IR_PARAM_INFO_TY_POINTER;
      info = ir_var_ty_info(func->unit, var_ty);
    }

    if (ir_var_ty_is_aggregate(var_ty)) {
      info.size = ROUND_UP(info.size, 8);
    }

    size_t num_hfa_members;
    size_t hfa_member_size;
    struct ir_var_ty member_ty;

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
      } else if (try_get_hfa_info(func, var_ty, &member_ty, &num_hfa_members,
                                  &hfa_member_size)) {
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
      .flags = func_ty.flags,
      .ret_ty = arena_alloc(func->arena, sizeof(ret_ty))};

  *new_func_ty.ret_ty = ret_ty;

  struct ir_call_info call_info = {.ret = ret_info, .stack_size = nsaa};

  CLONE_AND_FREE_VECTOR(func->arena, params, new_func_ty.num_params,
                        new_func_ty.params);
  CLONE_AND_FREE_VECTOR(func->arena, param_infos, call_info.num_params,
                        call_info.params);

  return (struct ir_func_info){.func_ty = new_func_ty, .call_info = call_info};
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

              DEBUG_ASSERT(!ir_var_ty_is_fp(&op->var_ty), "int-ty cnst but fp var ty");

              break;
            }
            case IR_OP_TY_BITFIELD_INSERT:
              op->bitfield_insert.value->flags |= IR_OP_FLAG_READS_DEST;
              break;
            case IR_OP_TY_UNARY_OP:
              if (op->binary_op.ty == IR_OP_UNARY_OP_TY_LOGICAL_NOT) {
                lower_logical_not(func, op);
              }
              break;
            case IR_OP_TY_BINARY_OP:
              switch (op->binary_op.ty) {
              case IR_OP_BINARY_OP_TY_UQUOT:
              case IR_OP_BINARY_OP_TY_SQUOT:
                lower_quot(func, op);
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
