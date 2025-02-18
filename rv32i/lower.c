#include "lower.h"

#include "../lower.h"
#include "../vector.h"
#include "../util.h"

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

  if (!var_ty->struct_ty.num_fields) {
    return false;
  }

  *member_ty = var_ty->struct_ty.fields[0];

  if (!var_ty_is_fp(member_ty)) {
    return false;
  }

  if (var_ty->struct_ty.num_fields > 4) {
    return false;
  }

  for (size_t i = 1; i < var_ty->struct_ty.num_fields; i++) {
    if (!var_ty_eq(func, member_ty, &var_ty->struct_ty.fields[i])) {
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

  *num_members = var_ty->struct_ty.num_fields;
  return true;
}

struct ir_func_info rv32i_lower_func_ty(struct ir_func *func,
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
        var_ty_info(func->unit, func_ty.ret_ty->ty == IR_VAR_TY_TY_ARRAY
                                    ? &IR_VAR_TY_POINTER
                                    : func_ty.ret_ty);

    ret_info = arena_alloc(func->arena, sizeof(*ret_info));

    struct ir_var_ty member_ty;
    size_t num_hfa_members, hfa_member_size;
    if (try_get_hfa_info(func, func_ty.ret_ty, &member_ty, &num_hfa_members,
                         &hfa_member_size)) {
      // nop
      *ret_info = (struct ir_param_info){
          .ty = IR_PARAM_INFO_TY_REGISTER,
          .var_ty = func_ty.ret_ty,
          .reg = {.start_reg = {.ty = IR_REG_TY_FP, .idx = 0},
                  .size = hfa_member_size},
      };
    } else if (info.size > 16) {
      ret_ty = IR_VAR_TY_NONE;

      *ret_info = (struct ir_param_info){
          .ty = IR_PARAM_INFO_TY_POINTER,
          .var_ty = func_ty.ret_ty,
          .reg = {.start_reg = {.ty = IR_REG_TY_INTEGRAL, .idx = 0}, .size = 8},
      };

      vector_push_front(params, &IR_VAR_TY_POINTER);
    } else {
      *ret_info = (struct ir_param_info){
          .ty = IR_PARAM_INFO_TY_REGISTER,
          .var_ty = func_ty.ret_ty,
          .reg = {.start_reg = {.ty = IR_REG_TY_INTEGRAL, .idx = 0}, .size = 8},
      };
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
      DEBUG_ASSERT(func_ty.flags & IR_VAR_FUNC_TY_FLAG_VARIADIC,
                   "more args than params but not variadic");
      var_ty = &args[i]->var_ty;
      variadic = true;
    }

    if (var_ty->ty == IR_VAR_TY_TY_ARRAY) {
      var_ty = &IR_VAR_TY_POINTER;
    }

    struct ir_var_ty_info info = var_ty_info(func->unit, var_ty);

    if (info.size > 16) {
      // copy to mem
      var_ty = &IR_VAR_TY_POINTER;
      info = var_ty_info(func->unit, var_ty);
    }

    if (var_ty_is_aggregate(var_ty)) {
      info.size = ROUND_UP(info.size, 8);
    }

    size_t num_hfa_members;
    size_t hfa_member_size;
    struct ir_var_ty member_ty;

    if (!variadic || !variadics_on_stack) {
      if (var_ty_is_fp(var_ty) && nsrn < 8) {
        vector_push_back(params, var_ty);

        struct ir_param_info param_info = {
            .ty = IR_PARAM_INFO_TY_REGISTER,
            .var_ty = var_ty,
            .reg = {.start_reg = {.ty = IR_REG_TY_FP, .idx = nsrn},
                    .size = info.size},
        };
        vector_push_back(param_infos, &param_info);

        nsrn++;
        continue;
      } else if (try_get_hfa_info(func, var_ty, &member_ty, &num_hfa_members,
                                  &hfa_member_size)) {
        if (nsrn + num_hfa_members <= 8) {
          for (size_t j = 0; j < num_hfa_members; j++) {
            // given this is a composite, we assume `source` contains a
            // pointer to it

            vector_push_back(params, &member_ty);
          }

          struct ir_param_info param_info = {
              .ty = IR_PARAM_INFO_TY_REGISTER,
              .var_ty = var_ty,
              .reg = {.start_reg = {.ty = IR_REG_TY_FP, .idx = nsrn},
                      .size = hfa_member_size},
          };
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

      } else if (var_ty_is_integral(var_ty) && info.size <= 8 && ngrn <= 8) {
        vector_push_back(params, var_ty);

        struct ir_param_info param_info = {
            .ty = IR_PARAM_INFO_TY_REGISTER,
            .var_ty = var_ty,
            .reg = {.start_reg = {.ty = IR_REG_TY_INTEGRAL, .idx = ngrn},
                    .size = info.size}};
        vector_push_back(param_infos, &param_info);

        ngrn++;

        continue;
      }

      if (info.alignment == 16) {
        ngrn = (ngrn + 1) & 1;
      }

      if (var_ty_is_integral(var_ty) && info.size == 16 && ngrn < 7) {
        // // lo to ngrn, hi to ngrn+1

        vector_push_back(params, &IR_VAR_TY_I64);
        vector_push_back(params, &IR_VAR_TY_I64);

        struct ir_param_info param_info = {
            .ty = IR_PARAM_INFO_TY_REGISTER,
            .var_ty = var_ty,
            .reg = {.start_reg = {.ty = IR_REG_TY_INTEGRAL, .idx = ngrn},
                    .size = 8}};
        vector_push_back(param_infos, &param_info);

        ngrn += 2;
        continue;
      }

      size_t dw_size = (info.size + 7) / 8;
      if (var_ty_is_aggregate(var_ty) && dw_size <= (8 - ngrn)) {
        for (size_t j = 0; j < dw_size; j++) {
          // given this is a composite, we assume `source` contains a
          // pointer to it
          vector_push_back(params, &IR_VAR_TY_I64);
        }

        struct ir_param_info param_info = {
            .ty = IR_PARAM_INFO_TY_REGISTER,
            .var_ty = var_ty,
            .reg = {.start_reg = {.ty = IR_REG_TY_INTEGRAL, .idx = ngrn},
                    .size = 8}};
        vector_push_back(param_infos, &param_info);

        ngrn += dw_size;
        continue;
      }
    }

    ngrn = 8;
    size_t nsaa_align = MAX(8, info.alignment);
    nsaa = ROUND_UP(nsaa, nsaa_align);

    if (var_ty_is_aggregate(var_ty)) {
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
      .num_params = vector_length(params),
      .params = vector_head(params),
      .ret_ty = arena_alloc(func->arena, sizeof(ret_ty))};

  *new_func_ty.ret_ty = ret_ty;

  struct ir_call_info call_info = {.params = vector_head(param_infos),
                                   .num_params = vector_length(param_infos),
                                   .ret = ret_info,
                                   .stack_size = nsaa};

  return (struct ir_func_info){.func_ty = new_func_ty, .call_info = call_info};
}

static void lower_fp_cnst(struct ir_func *func, struct ir_op *op) {
  // transform into creating an integer, and then mov to float reg

  struct ir_var_ty int_ty;
  unsigned long long int_value;

  DEBUG_ASSERT(var_ty_is_fp(&op->var_ty), "float constant not fp type?");

  switch (op->var_ty.primitive) {
  case IR_VAR_PRIMITIVE_TY_F32: {
    int_ty = IR_VAR_TY_I32;

    union {
      float f;
      unsigned u;
    } v;
    v.f = (float)op->cnst.flt_value;
    int_value = v.u;

    struct ir_op *int_mov = insert_before_ir_op(func, op, IR_OP_TY_CNST, int_ty);
    int_mov->cnst =
        (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = int_value};

    op->ty = IR_OP_TY_MOV;
    op->mov = (struct ir_op_mov){.value = int_mov};
    return;
  }
  case IR_VAR_PRIMITIVE_TY_F64: {
    struct ir_glb *glb = add_global(func->unit, IR_GLB_TY_DATA, &op->var_ty,
                                    IR_GLB_DEF_TY_DEFINED, NULL);

    glb->var = arena_alloc(func->arena, sizeof(*glb->var));

    *glb->var = (struct ir_var){.ty = IR_VAR_TY_CONST_DATA,
                                          .var_ty = op->var_ty,
                                .value = {.ty = IR_VAR_VALUE_TY_FLT,
                                          .var_ty = op->var_ty,
                                          .flt_value = op->cnst.flt_value}};

    struct ir_op *addr = insert_before_ir_op(func, op, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
    addr->addr = (struct ir_op_addr){.ty = IR_OP_ADDR_TY_GLB, .glb = glb};

    op->ty = IR_OP_TY_LOAD;
    op->load = (struct ir_op_load){.ty = IR_OP_LOAD_TY_ADDR, .addr = addr};
    break;
  }
  default:
    unreachable();
  }
}

static void lower_br_cond(struct ir_func *func, struct ir_op *op) {
  struct ir_op *cond = op->br_cond.cond;
  if (cond->ty != IR_OP_TY_BINARY_OP || !binary_op_is_comparison(cond->binary_op.ty)) {
    // turn it into a `!= 0`
    struct ir_op *zero;

    enum ir_op_binary_op_ty ty;
    if (var_ty_is_fp(&cond->var_ty)) {
      ty = IR_OP_BINARY_OP_TY_FNEQ;
      zero = insert_before_ir_op(func, op, IR_OP_TY_BINARY_OP, cond->var_ty);
      mk_floating_zero_constant(func->unit, zero, cond->var_ty.primitive);
    } else {
      ty = IR_OP_BINARY_OP_TY_NEQ;
      zero = insert_before_ir_op(func, op, IR_OP_TY_CNST, cond->var_ty);
      mk_integral_constant(func->unit, zero, cond->var_ty.primitive, 0);
    }
    
    cond = insert_after_ir_op(func, zero, IR_OP_TY_BINARY_OP, IR_VAR_TY_I32);
    cond->binary_op = (struct ir_op_binary_op){
      .ty = ty,
      .lhs = op->br_cond.cond,
      .rhs = zero
    };

    op->br_cond.cond = cond;
  }

  if (var_ty_is_fp(&cond->binary_op.lhs->var_ty)) {
    DEBUG_ASSERT(var_ty_is_fp(&cond->binary_op.rhs->var_ty), "mixed binop");
  } else {
    DEBUG_ASSERT(var_ty_is_integral(&cond->binary_op.rhs->var_ty), "mixed binop");
    cond->flags |= IR_OP_FLAG_CONTAINED;
  }
}

void rv32i_lower(struct ir_unit *unit) {
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
            case IR_OP_TY_CUSTOM:
            case IR_OP_TY_PHI:
            case IR_OP_TY_BINARY_OP:
            case IR_OP_TY_RET:
            case IR_OP_TY_STORE:
            case IR_OP_TY_LOAD:
            case IR_OP_TY_CALL:
            case IR_OP_TY_STORE_BITFIELD:
            case IR_OP_TY_LOAD_BITFIELD:
            case IR_OP_TY_ADDR:
            case IR_OP_TY_BR:
            case IR_OP_TY_BR_SWITCH:
            case IR_OP_TY_MOV:
            case IR_OP_TY_UNARY_OP:
            case IR_OP_TY_CAST_OP:
              break;
            case IR_OP_TY_BR_COND:
              lower_br_cond(func, op);
              break;
            case IR_OP_TY_BITFIELD_EXTRACT:
              lower_bitfield_extract(func, op);
              break;
            case IR_OP_TY_BITFIELD_INSERT:
              lower_bitfield_insert(func, op);
              break;
            case IR_OP_TY_ADDR_OFFSET: {
              struct ir_op *base = op->addr_offset.base;

              if (op->addr_offset.offset) {
                struct ir_op *cnst = insert_before_ir_op(
                    func, op, IR_OP_TY_BINARY_OP, IR_VAR_TY_POINTER);
                mk_integral_constant(unit, cnst, IR_VAR_PRIMITIVE_TY_I32,
                                     op->addr_offset.offset);

                struct ir_op *offset = insert_before_ir_op(func, op, IR_OP_TY_BINARY_OP,
                                           IR_VAR_TY_POINTER);
                offset->binary_op = (struct ir_op_binary_op){
                    .ty = IR_OP_BINARY_OP_TY_ADD, .lhs = base, .rhs = cnst};

                base = offset;
              }

              if (op->addr_offset.index) {
                struct ir_op *cnst = insert_before_ir_op(
                    func, op, IR_OP_TY_BINARY_OP, IR_VAR_TY_POINTER);
                mk_integral_constant(unit, cnst, IR_VAR_PRIMITIVE_TY_I32,
                                     op->addr_offset.scale);

                struct ir_op *mul = insert_before_ir_op(
                    func, op, IR_OP_TY_BINARY_OP, IR_VAR_TY_POINTER);
                mul->binary_op =
                    (struct ir_op_binary_op){.ty = IR_OP_BINARY_OP_TY_MUL,
                                             .lhs = op->addr_offset.index,
                                             .rhs = cnst};

                struct ir_op *offset = insert_before_ir_op(
                    func, op, IR_OP_TY_BINARY_OP, IR_VAR_TY_POINTER);
                offset->binary_op = (struct ir_op_binary_op){
                    .ty = IR_OP_BINARY_OP_TY_ADD, .lhs = base, .rhs = mul};

                base = offset;
              }

              op->ty = IR_OP_TY_MOV;
              op->mov = (struct ir_op_mov) { .value = base };
            } break;
            case IR_OP_TY_CNST: {
              if (op->cnst.ty == IR_OP_CNST_TY_FLT) {
                lower_fp_cnst(func, op);
                break;
              }

              break;
            }
            default:
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
}
