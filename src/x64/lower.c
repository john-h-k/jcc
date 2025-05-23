#include "lower.h"

#include "../lower.h"
#include "../util.h"
#include "../vector.h"

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

struct ir_func_info x64_lower_func_ty(struct ir_func *func,
                                      struct ir_var_func_ty func_ty,
                                      struct ir_op **args, size_t num_args) {

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

    ret_info = aralloc(func->arena, sizeof(*ret_info));

    struct ir_var_ty member_ty;
    size_t num_hfa_members;
    size_t hfa_member_size;
    if (try_get_hfa_info(func_ty.ret_ty, &member_ty, &num_hfa_members,
                         &hfa_member_size)) {
      // nop
      *ret_info = (struct ir_param_info){
          .ty = IR_PARAM_INFO_TY_REGISTER,
          .var_ty = func_ty.ret_ty,
          .num_regs = num_hfa_members,
      };

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
          .regs[0] = {.reg = {.ty = IR_REG_TY_INTEGRAL, .idx = IR_REG_IDX_DI},
                      .size = 8},
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
          .num_regs = num_regs,
          .var_ty = func_ty.ret_ty,
      };

      ret_info->regs[0] = (struct ir_param_reg){
          .reg = {.ty = IR_REG_TY_INTEGRAL, .idx = IR_REG_IDX_AX}, .size = 8};
      ret_info->regs[1] = (struct ir_param_reg){
          .reg = {.ty = IR_REG_TY_INTEGRAL, .idx = IR_REG_IDX_CX}, .size = 8};
    }
  }

  // NOTE: on windows, SIMD&FP registers are not used in variadics, ever

  size_t num = MAX(func_ty.num_params, num_args);

  // number of xmm _registers_ in EAX
  // size_t num_ = 0;

  for (size_t i = 0; i < num; i++) {
    const struct ir_var_ty *var_ty;

    if (i < func_ty.num_params) {
      var_ty = &func_ty.params[i];
    } else {
      var_ty = &args[i]->var_ty;
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
    } else if (try_get_hfa_info(var_ty, &member_ty, &num_hfa_members,
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

      struct ir_param_info param_info = {
          .ty = IR_PARAM_INFO_TY_STACK, .var_ty = var_ty, .stack_offset = nsaa};
      vector_push_back(param_infos, &param_info);

      nsaa += size;
      continue;

    } else if (ir_var_ty_is_integral(var_ty) && info.size <= 8 && ngrn < 6) {
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
      ngrn = (ngrn + 1) & ~1;
    }

    // if (var_ty_is_integral(var_ty) && info.size == 16 && ngrn < 7) {
    //   // // lo to ngrn, hi to ngrn+1

    //   vector_push_back(params, &IR_VAR_TY_I64);
    //   vector_push_back(params, &IR_VAR_TY_I64);

    //   struct ir_param_info param_info = {
    //       .ty = IR_PARAM_INFO_TY_REGISTER,
    //       .var_ty = var_ty,
    //       .num_regs = 2,
    //       .regs[0] = {.reg = {.ty = IR_REG_TY_INTEGRAL, .idx = ngrn},
    //   vector_push_back(param_infos, &param_info);

    //   ngrn += 2;
    //   continue;
    // }

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

  struct ir_call_info call_info = {
      .ret = ret_info,
      .stack_size = nsaa,
      .num_variadics = nsrn,
      .num_gp_used = ngrn,
      .num_fp_used = nsrn,
      .flags = (func_ty.flags & IR_VAR_FUNC_TY_FLAG_VARIADIC)
                   ? IR_CALL_INFO_FLAG_NUM_VARIADIC
                   : IR_CALL_INFO_FLAG_NONE,
      .num_variadics_reg = {.ty = IR_REG_TY_INTEGRAL, .idx = IR_REG_IDX_AX}};

  CLONE_AND_FREE_VECTOR(func->arena, params, new_func_ty.num_params,
                        new_func_ty.params);
  CLONE_AND_FREE_VECTOR(func->arena, param_infos, call_info.num_params,
                        call_info.params);

  return (struct ir_func_info){.func_ty = new_func_ty, .call_info = call_info};
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

static void lower_fabs(struct ir_func *func, struct ir_op *op) {
  DEBUG_ASSERT(op->ty == IR_OP_TY_UNARY_OP &&
                   op->unary_op.ty == IR_OP_UNARY_OP_TY_FABS,
               "called on invalid op");

  uint64_t mask_cnst;
  enum ir_var_primitive_ty integral;
  switch (op->unary_op.value->var_ty.primitive) {
  case IR_VAR_PRIMITIVE_TY_F16:
    mask_cnst = 0x7FFF;
    integral = IR_VAR_PRIMITIVE_TY_I16;
    break;
  case IR_VAR_PRIMITIVE_TY_F32:
    mask_cnst = 0x7FFFFFFF;
    integral = IR_VAR_PRIMITIVE_TY_I32;
    break;
  case IR_VAR_PRIMITIVE_TY_F64:
    mask_cnst = 0x7FFFFFFFFFFFFFFF;
    integral = IR_VAR_PRIMITIVE_TY_I64;
    break;
  default:
    unreachable();
  }

  struct ir_op *mask = ir_insert_before_op(func, op, IR_OP_TY_CNST, op->var_ty);
  ir_mk_integral_constant(func->unit, mask, integral, mask_cnst);

  struct ir_op *fp_mask =
      ir_insert_after_op(func, mask, IR_OP_TY_CNST, op->unary_op.value->var_ty);
  fp_mask->ty = IR_OP_TY_MOV;
  fp_mask->mov = (struct ir_op_mov){.value = mask};

  op->ty = IR_OP_TY_BINARY_OP;
  op->flags |= IR_OP_FLAG_READS_DEST;
  op->binary_op.ty = IR_OP_BINARY_OP_TY_AND;
  op->binary_op.lhs = op->unary_op.value;
  op->binary_op.rhs = fp_mask;
}

static void lower_fneg(struct ir_func *func, struct ir_op *op) {
  DEBUG_ASSERT(op->ty == IR_OP_TY_UNARY_OP &&
                   op->unary_op.ty == IR_OP_UNARY_OP_TY_FNEG,
               "called on invalid op");

  uint64_t mask_cnst;
  enum ir_var_primitive_ty integral;
  switch (op->unary_op.value->var_ty.primitive) {
  case IR_VAR_PRIMITIVE_TY_F16:
    mask_cnst = 0x8000;
    integral = IR_VAR_PRIMITIVE_TY_I16;
    break;
  case IR_VAR_PRIMITIVE_TY_F32:
    mask_cnst = 0x80000000;
    integral = IR_VAR_PRIMITIVE_TY_I32;
    break;
  case IR_VAR_PRIMITIVE_TY_F64:
    mask_cnst = 0x8000000000000000;
    integral = IR_VAR_PRIMITIVE_TY_I64;
    break;
  default:
    unreachable();
  }

  struct ir_op *mask = ir_insert_before_op(func, op, IR_OP_TY_CNST, op->var_ty);
  ir_mk_integral_constant(func->unit, mask, integral, mask_cnst);

  struct ir_op *fp_mask =
      ir_insert_after_op(func, mask, IR_OP_TY_CNST, op->unary_op.value->var_ty);
  fp_mask->ty = IR_OP_TY_MOV;
  fp_mask->mov = (struct ir_op_mov){.value = mask};

  op->ty = IR_OP_TY_BINARY_OP;
  op->flags |= IR_OP_FLAG_READS_DEST;
  op->binary_op.ty = IR_OP_BINARY_OP_TY_XOR;
  op->binary_op.lhs = op->unary_op.value;
  op->binary_op.rhs = fp_mask;
}

static void lower_comparison(struct ir_func *irb, struct ir_op *op) {
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

static void lower_fp_cnst(struct ir_func *func, struct ir_op *op) {
  // transform into creating an integer, and then mov to float reg

  struct ir_var_ty int_ty;
  unsigned long long int_value;

  DEBUG_ASSERT(ir_var_ty_is_fp(&op->var_ty), "float constant not fp type?");

  switch (op->var_ty.primitive) {
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

// FIXME: use RAX value to only save required XMM registers + more registers for
// APX
#define REG_SAVE_SIZE (6 * 8 + 16 * 16)
#define REG_SAVE_FP_OFFSET (6 * 8)
#define REG_GP_USED (6 * 8)
#define REG_FP_USED REG_SAVE_FP_OFFSET + (8 * 16)

static struct ir_lcl *lower_va_args(struct ir_func *func) {
  DEBUG_ASSERT(REG_SAVE_SIZE % 8 == 0, "because we use type I64 for alignment, "
                                       "REG_SAVE_SIZE must be divisible by 8");
  size_t save_sz = REG_SAVE_SIZE / 8;

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

  struct ir_call_info info = func->call_info;

  // need to save all the registers in args
  for (size_t i = info.num_gp_used; i < 6; i++) {
    struct ir_op *addr;

    if (i) {
      addr = ir_insert_after_op(func, last, IR_OP_TY_ADDR_OFFSET,
                                IR_VAR_TY_POINTER);
      addr->addr_offset =
          (struct ir_op_addr_offset){.base = base, .offset = i * 8};
      last = addr;
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
        ir_insert_after_op(func, last, IR_OP_TY_STORE, IR_VAR_TY_NONE);
    save->store = (struct ir_op_store){
        .ty = IR_OP_STORE_TY_ADDR, .addr = last, .value = value};

    last = save;
  }

  // FIXME: the SysV spec has enough space and pretends there are 16 SIMD
  // registers used for stack but in reality it only uses 8?
  // we can't easily save the upper 16 because that includes the spill reg and
  // LSRA expects that to be available
  // FIXME: use `rax` to determine how many of these to save
  for (size_t i = info.num_fp_used; i < 8; i++) {
    // TODO: save vector

    struct ir_op *addr =
        ir_insert_after_op(func, last, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
    addr->addr_offset = (struct ir_op_addr_offset){
        .base = base, .offset = REG_SAVE_FP_OFFSET + (i * 16)};

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

  return lcl;
}

static void lower_va_start(struct ir_func *func, struct ir_lcl *save_lcl,
                           struct ir_op *op) {
  // The va_start macro initializes the structure as follows:
  //
  //   * gp_offset
  //     - The element holds the offset in bytes from reg_save_area to the place
  //     where the next available general purpose argument register is saved. In
  //     case all argument registers have been exhausted, it is set to the value
  //     48 (6 * 8).
  //
  //   * fp_offset
  //     - The element holds the offset in bytes from reg_save_area to the place
  //     where the next available floating point argument register is saved. In
  //     case all argument registers have been exhausted, it is set to the value
  //     304 (6 * 8 + 16 * 16)
  //
  //   * reg_save_area
  //     - The element points to the start of the register save area.
  //
  //   * overflow_arg_area
  //     - This pointer is used to fetch arguments passed on the stack. It is
  //     initialized with the address of the first argument passed on the stack,
  //     if any, and then always updated to point to the start of the next
  //     argument on the stack.

  DEBUG_ASSERT(save_lcl, "expected save_lcl to exist (maybe "
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
  {
    // list->gp_offset = <num gp registers used by named args * 8>

    size_t gp_offset = MIN(6 * 8, info.num_gp_used * 8);

    DEBUG_ASSERT(gp_offset == REG_GP_USED || info.stack_size == 0,
                 "stack used but not all gp reg?");

    struct ir_op *gp_offset_value =
        ir_insert_after_op(func, last, IR_OP_TY_CNST, IR_VAR_TY_I32);
    ir_mk_integral_constant(func->unit, gp_offset_value,
                            IR_VAR_PRIMITIVE_TY_I32, gp_offset);

    struct ir_op *store_gp_offset = ir_insert_after_op(
        func, gp_offset_value, IR_OP_TY_STORE, IR_VAR_TY_NONE);
    store_gp_offset->store = (struct ir_op_store){
        .ty = IR_OP_STORE_TY_ADDR, .addr = addr, .value = gp_offset_value};

    last = store_gp_offset;
  }

  {
    // list->fp_offset = REG_SAVE_FP_OFFSET + <num fp registers used by named
    // args * 16>

    size_t fp_offset = REG_SAVE_FP_OFFSET + MIN(16 * 16, info.num_fp_used * 16);

    DEBUG_ASSERT(fp_offset == REG_FP_USED || info.stack_size == 0,
                 "stack used but not all fp reg?");

    struct ir_op *fp_offset_value =
        ir_insert_after_op(func, last, IR_OP_TY_CNST, IR_VAR_TY_I32);
    ir_mk_integral_constant(func->unit, fp_offset_value,
                            IR_VAR_PRIMITIVE_TY_I32, fp_offset);

    struct ir_op *addr_fp_offset = ir_insert_after_op(
        func, fp_offset_value, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
    addr_fp_offset->addr_offset =
        (struct ir_op_addr_offset){.base = addr, .offset = 4};

    struct ir_op *store_fp_offset = ir_insert_after_op(
        func, addr_fp_offset, IR_OP_TY_STORE, IR_VAR_TY_NONE);
    store_fp_offset->store = (struct ir_op_store){.ty = IR_OP_STORE_TY_ADDR,
                                                  .addr = addr_fp_offset,
                                                  .value = fp_offset_value};

    last = store_fp_offset;
  }

  {
    // list->overflow_arg_area = fp + <stack space used by named args>

    size_t stack_offset = info.stack_size;

    struct ir_op *overflow_arg_value;
    if (stack_offset) {
      overflow_arg_value = ir_insert_after_op(func, last, IR_OP_TY_ADDR_OFFSET,
                                              IR_VAR_TY_POINTER);
      overflow_arg_value->addr_offset =
          (struct ir_op_addr_offset){.base = sp, .offset = stack_offset};

      last = overflow_arg_value;
    } else {
      overflow_arg_value = sp;
    }

    struct ir_op *overflow_arg_addr =
        ir_insert_after_op(func, last, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
    overflow_arg_addr->addr_offset =
        (struct ir_op_addr_offset){.base = addr, .offset = 8};

    struct ir_op *store_overflow_arg = ir_insert_after_op(
        func, overflow_arg_addr, IR_OP_TY_STORE, IR_VAR_TY_NONE);
    store_overflow_arg->store =
        (struct ir_op_store){.ty = IR_OP_STORE_TY_ADDR,
                             .addr = overflow_arg_addr,
                             .value = overflow_arg_value};

    last = store_overflow_arg;
  }

  {
    // list->reg_save_area = fp - reg_save_size

    struct ir_op *reg_save_value =
        ir_insert_after_op(func, last, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
    reg_save_value->addr =
        (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = save_lcl};

    // TODO: having addr_offset support negative values would be useful here

    struct ir_op *reg_save_addr = ir_insert_after_op(
        func, reg_save_value, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
    reg_save_addr->addr_offset =
        (struct ir_op_addr_offset){.base = addr, .offset = 16};

    struct ir_op *store_reg_save =
        ir_insert_after_op(func, reg_save_addr, IR_OP_TY_STORE, IR_VAR_TY_NONE);
    store_reg_save->store = (struct ir_op_store){.ty = IR_OP_STORE_TY_ADDR,
                                                 .addr = reg_save_addr,
                                                 .value = reg_save_value};

    last = store_reg_save;
  }
}

static void lower_va_arg(UNUSED struct ir_func *func, UNUSED struct ir_op *op) {
  TODO("x64 va_arg");
}

void x64_lower_variadic(struct ir_func *func) {
  struct ir_lcl *save_lcl = NULL;
  if (func->flags & IR_FUNC_FLAG_USES_VA_ARGS) {
    save_lcl = lower_va_args(func);
  }

  struct ir_func_iter iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);

  struct ir_op *op;
  while (ir_func_iter_next(&iter, &op)) {
    switch (op->ty) {
    case IR_OP_TY_VA_START:
      lower_va_start(func, save_lcl, op);
      break;
    case IR_OP_TY_VA_ARG:
      lower_va_arg(func, op);
      break;
    default:
      break;
    }
  }
}
void x64_lower(struct ir_unit *unit) {
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
            case IR_OP_TY_UNKNOWN:
              BUG("unknown op!");
            case IR_OP_TY_UNDF:
            case IR_OP_TY_PHI:
            case IR_OP_TY_RET:
            case IR_OP_TY_VA_START:
            case IR_OP_TY_VA_ARG:
              break;
            case IR_OP_TY_CNST: {
              if (op->cnst.ty == IR_OP_CNST_TY_FLT) {
                lower_fp_cnst(func, op);
                break;
              }

              break;
            }
            case IR_OP_TY_STORE:
              lower_store(func, op);
              break;
            case IR_OP_TY_LOAD:
            case IR_OP_TY_ADDR:
            case IR_OP_TY_BR_SWITCH:
            case IR_OP_TY_BR:
            case IR_OP_TY_BR_COND:
            case IR_OP_TY_MOV:
            case IR_OP_TY_CAST_OP:
            case IR_OP_TY_CALL:
            case IR_OP_TY_STORE_BITFIELD:
            case IR_OP_TY_LOAD_BITFIELD:
              break;
            case IR_OP_TY_BITFIELD_EXTRACT:
              lower_bitfield_extract(func, op);
              break;
            case IR_OP_TY_BITFIELD_INSERT:
              lower_bitfield_insert(func, op);
              break;
            case IR_OP_TY_UNARY_OP:
              if (op->unary_op.ty == IR_OP_UNARY_OP_TY_LOGICAL_NOT) {
                lower_logical_not(func, op);
              }
              break;
            case IR_OP_TY_BINARY_OP:
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
              // try_contain_store(func, op);
              break;
            case IR_OP_TY_LOAD:
              // try_contain_load(func, op);
              break;
            case IR_OP_TY_BINARY_OP:
              switch (op->binary_op.ty) {
              case IR_OP_BINARY_OP_TY_LSHIFT:
              case IR_OP_BINARY_OP_TY_SRSHIFT:
              case IR_OP_BINARY_OP_TY_URSHIFT:
                op->flags |= IR_OP_FLAG_READS_DEST;
                ir_alloc_fixed_reg_dest_op(
                    func, &op->binary_op.rhs, op,
                    (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                    .idx = IR_REG_IDX_CX});
                break;

              case IR_OP_BINARY_OP_TY_FADD:
              case IR_OP_BINARY_OP_TY_FSUB:
              case IR_OP_BINARY_OP_TY_FMUL:
              case IR_OP_BINARY_OP_TY_FDIV:
              case IR_OP_BINARY_OP_TY_AND:
              case IR_OP_BINARY_OP_TY_OR:
              case IR_OP_BINARY_OP_TY_XOR:
              case IR_OP_BINARY_OP_TY_ADD:
              case IR_OP_BINARY_OP_TY_SUB:
                // TODO: only do this where actually applicable
                op->flags |= IR_OP_FLAG_READS_DEST;
                break;
              case IR_OP_BINARY_OP_TY_MUL:
              case IR_OP_BINARY_OP_TY_SDIV:
              case IR_OP_BINARY_OP_TY_UDIV:
                op->flags |= IR_OP_FLAG_READS_DEST;

                op->write_info = (struct ir_op_write_info){
                    .num_reg_writes = 1,
                    .writes[0] = {.ty = IR_REG_TY_INTEGRAL,
                                  .idx = IR_REG_IDX_DX},
                };

                ir_alloc_fixed_reg_dest_op(
                    func, &op->binary_op.lhs, op,
                    (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                    .idx = IR_REG_IDX_AX});
                ir_alloc_fixed_reg_source_op(
                    func, op,
                    (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                    .idx = IR_REG_IDX_AX});

                break;
              case IR_OP_BINARY_OP_TY_SMOD:
              case IR_OP_BINARY_OP_TY_UMOD:
                op->flags |= IR_OP_FLAG_READS_DEST;

                op->write_info = (struct ir_op_write_info){
                    .num_reg_writes = 1,
                    .writes[0] = {.ty = IR_REG_TY_INTEGRAL,
                                  .idx = IR_REG_IDX_DX},
                };

                ir_alloc_fixed_reg_dest_op(
                    func, &op->binary_op.lhs, op,
                    (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                    .idx = IR_REG_IDX_AX});
                ir_alloc_fixed_reg_source_op(
                    func, op,
                    (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                    .idx = IR_REG_IDX_DX});

                break;
              default:
                if (ir_binary_op_is_comparison(op->binary_op.ty)) {
                  lower_comparison(func, op);
                }
                break;
              }

              break;
            case IR_OP_TY_UNARY_OP:
              switch (op->unary_op.ty) {
              case IR_OP_UNARY_OP_TY_NEG:
              case IR_OP_UNARY_OP_TY_LOGICAL_NOT:
              case IR_OP_UNARY_OP_TY_NOT:
                op->flags |= IR_OP_FLAG_READS_DEST;
                break;
              case IR_OP_UNARY_OP_TY_FNEG:
                lower_fneg(func, op);
                break;
              case IR_OP_UNARY_OP_TY_FABS:
                lower_fabs(func, op);
                break;
              default:
                break;
              }
              break;
            case IR_OP_TY_ADDR_OFFSET:
              if (op->addr_offset.index &&
                  popcntl(op->addr_offset.scale) != 1) {
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

                mul->flags |= IR_OP_FLAG_READS_DEST;

                mul->write_info = (struct ir_op_write_info){
                    .num_reg_writes = 1,
                    .writes[0] = {.ty = IR_REG_TY_INTEGRAL,
                                  .idx = IR_REG_IDX_DX},
                };

                ir_alloc_fixed_reg_dest_op(
                    func, &mul->binary_op.lhs, mul,
                    (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                    .idx = IR_REG_IDX_AX});
                ir_alloc_fixed_reg_source_op(
                    func, mul,
                    (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                    .idx = IR_REG_IDX_AX});

                op->addr_offset.scale = 1;
                op->addr_offset.index = mul;
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
