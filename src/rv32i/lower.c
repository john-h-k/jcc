#include "lower.h"

#include "../lower.h"
#include "../util.h"
#include "../vector.h"

static bool try_get_hfa_info(const struct ir_var_ty *var_ty,
                             struct ir_var_ty *member_ty, size_t *num_members) {
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

  if (var_ty->aggregate.num_fields > 2) {
    return false;
  }

  for (size_t i = 1; i < var_ty->aggregate.num_fields; i++) {
    if (!ir_var_ty_is_fp(&var_ty->aggregate.fields[i])) {
      return false;
    }
  }

  *num_members = var_ty->aggregate.num_fields;
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
        ir_var_ty_info(func->unit, func_ty.ret_ty->ty == IR_VAR_TY_TY_ARRAY
                                    ? &IR_VAR_TY_POINTER
                                    : func_ty.ret_ty);

    ret_info = arena_alloc(func->arena, sizeof(*ret_info));

    struct ir_var_ty member_ty;
    size_t num_hfa_members;
    if (try_get_hfa_info(func_ty.ret_ty, &member_ty, &num_hfa_members)) {
      // nop
      *ret_info = (struct ir_param_info){.ty = IR_PARAM_INFO_TY_REGISTER,
                                         .var_ty = func_ty.ret_ty,
                                         .num_regs = num_hfa_members};

      DEBUG_ASSERT(ir_var_ty_is_aggregate(func_ty.ret_ty) &&
                       func_ty.ret_ty->aggregate.num_fields == num_hfa_members,
                   "hfa not expected");
      for (size_t i = 0; i < num_hfa_members; i++) {
        struct ir_var_ty *member = &func_ty.ret_ty->aggregate.fields[i];

        ret_info->regs[i] =
            (struct ir_param_reg){.reg = {.ty = IR_REG_TY_FP, .idx = i},
                                  .size = ir_var_ty_info(func->unit, member).size};
      }
    } else if (info.size > 8) {
      ret_ty = IR_VAR_TY_NONE;

      *ret_info = (struct ir_param_info){
          .ty = IR_PARAM_INFO_TY_POINTER,
          .var_ty = func_ty.ret_ty,
          .num_regs = 1,
          .regs[0] = {.reg = {.ty = IR_REG_TY_INTEGRAL, .idx = 0}, .size = 4},
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
      size_t num_regs = (info.size + 3) / 4;
      *ret_info = (struct ir_param_info){
          .ty = IR_PARAM_INFO_TY_REGISTER,
          .var_ty = func_ty.ret_ty,
          .num_regs = num_regs,
      };

      for (size_t i = 0; i < num_regs; i++) {
        ret_info->regs[i] = (struct ir_param_reg){
            .reg = {.ty = IR_REG_TY_INTEGRAL, .idx = i}, .size = 4};
      }
    }
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

    if (info.size > 8) {
      // copy to mem
      var_ty = &IR_VAR_TY_POINTER;
      ty = IR_PARAM_INFO_TY_POINTER;
      info = ir_var_ty_info(func->unit, var_ty);
    }

    if (ir_var_ty_is_fp(var_ty) && nsrn < 8 && !variadic) {
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
    } else if (ir_var_ty_is_integral(var_ty) && info.size <= 4 && ngrn < 8) {
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

    if (info.alignment == 8) {
      ngrn = (ngrn + 1) & ~1;
    }

    size_t dw_size = (info.size + 3) / 4;
    if ((ir_var_ty_is_aggregate(var_ty) || (variadic && ir_var_ty_is_fp(var_ty))) &&
        dw_size <= (8 - ngrn)) {
      struct ir_param_info param_info = {
          .ty = ty, .var_ty = var_ty, .num_regs = dw_size};

      for (size_t j = 0; j < dw_size; j++) {
        // given this is a composite, we assume `source` contains a
        // pointer to it
        param_info.regs[j] = (struct ir_param_reg){
            .reg = {.ty = IR_REG_TY_INTEGRAL, .idx = ngrn + j}, .size = 4};

        vector_push_back(params, &IR_VAR_TY_I32);
      }

      vector_push_back(param_infos, &param_info);

      ngrn += dw_size;
      continue;
    }

    ngrn = 8;
    size_t nsaa_align = MAX(4, info.alignment);
    nsaa = ROUND_UP(nsaa, nsaa_align);

    if (ir_var_ty_is_aggregate(var_ty)) {
      struct ir_param_info param_info = {
          .ty = IR_PARAM_INFO_TY_STACK, .var_ty = var_ty, .stack_offset = nsaa};
      vector_push_back(param_infos, &param_info);

      nsaa += info.size;
      continue;
    }

    // 4 or 8?
    size_t size = MAX(4, info.size);

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

    struct ir_op *int_mov =
        ir_insert_before_op(func, op, IR_OP_TY_CNST, int_ty);
    int_mov->cnst =
        (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = int_value};

    op->ty = IR_OP_TY_MOV;
    op->mov = (struct ir_op_mov){.value = int_mov};
    return;
  }
  case IR_VAR_PRIMITIVE_TY_F64: {
    struct ir_glb *glb = ir_add_global(func->unit, IR_GLB_TY_DATA, &op->var_ty,
                                    IR_GLB_DEF_TY_DEFINED, NULL);

    glb->var = arena_alloc(func->arena, sizeof(*glb->var));

    *glb->var = (struct ir_var){.ty = IR_VAR_TY_CONST_DATA,
                                .var_ty = op->var_ty,
                                .value = {.ty = IR_VAR_VALUE_TY_FLT,
                                          .var_ty = op->var_ty,
                                          .flt_value = op->cnst.flt_value}};

    struct ir_op *addr =
        ir_insert_before_op(func, op, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
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
  if (cond->ty != IR_OP_TY_BINARY_OP ||
      !ir_binary_op_is_comparison(cond->binary_op.ty)) {
    // turn it into a `!= 0`
    struct ir_op *zero;

    enum ir_op_binary_op_ty ty;
    if (ir_var_ty_is_fp(&cond->var_ty)) {
      ty = IR_OP_BINARY_OP_TY_FNEQ;
      zero = ir_insert_before_op(func, op, IR_OP_TY_BINARY_OP, cond->var_ty);
      ir_mk_floating_zero_constant(func->unit, zero, cond->var_ty.primitive);
    } else {
      ty = IR_OP_BINARY_OP_TY_NEQ;
      zero = ir_insert_before_op(func, op, IR_OP_TY_CNST, cond->var_ty);
      ir_mk_integral_constant(func->unit, zero, cond->var_ty.primitive, 0);
    }

    cond = ir_insert_after_op(func, zero, IR_OP_TY_BINARY_OP, IR_VAR_TY_I32);
    cond->binary_op = (struct ir_op_binary_op){
        .ty = ty, .lhs = op->br_cond.cond, .rhs = zero};

    op->br_cond.cond = cond;
  }

  if (ir_var_ty_is_fp(&cond->binary_op.lhs->var_ty)) {
    DEBUG_ASSERT(ir_var_ty_is_fp(&cond->binary_op.rhs->var_ty), "mixed binop");
  } else {
    DEBUG_ASSERT(ir_var_ty_is_integral(&cond->binary_op.rhs->var_ty),
                 "mixed binop");
    cond->flags |= IR_OP_FLAG_CONTAINED;
  }
}

#define MAX_IMM_SIZE (2047)

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

    if (!addr_offset.index) {
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

    if (!addr_offset.index) {
      op->store.addr = ir_alloc_contained_op(func, addr, op);
    }
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
            case IR_OP_TY_BR_COND:
              lower_br_cond(func, op);
              break;
            case IR_OP_TY_BITFIELD_EXTRACT:
              lower_bitfield_extract(func, op);
              break;
            case IR_OP_TY_BITFIELD_INSERT:
              lower_bitfield_insert(func, op);
              break;
            case IR_OP_TY_STORE:
              try_contain_store(func, op);
              break;
            case IR_OP_TY_LOAD:
              try_contain_load(func, op);
              break;
            case IR_OP_TY_ADDR_OFFSET: {
              if (op->addr_offset.index) {
                // do mul beforehand and set scale to 1
                struct ir_op *cnst = ir_insert_before_op(
                    func, op, IR_OP_TY_BINARY_OP, IR_VAR_TY_POINTER);
                ir_mk_integral_constant(unit, cnst, IR_VAR_PRIMITIVE_TY_I32,
                                     op->addr_offset.scale);

                struct ir_op *mul = ir_insert_before_op(
                    func, op, IR_OP_TY_BINARY_OP, IR_VAR_TY_POINTER);
                mul->binary_op =
                    (struct ir_op_binary_op){.ty = IR_OP_BINARY_OP_TY_MUL,
                                             .lhs = op->addr_offset.index,
                                             .rhs = cnst};

                op->addr_offset.scale = 1;
                op->addr_offset.index = mul;

                struct ir_op *lhs = op->addr_offset.base;

                struct ir_op *add = ir_replace_op(
                    func, op, IR_OP_TY_BINARY_OP, IR_VAR_TY_POINTER);
                add->binary_op = (struct ir_op_binary_op){
                    .ty = IR_OP_BINARY_OP_TY_ADD, .lhs = lhs, .rhs = mul};
              } else {
                try_contain_addr_offset(func, op);
              }
              break;
            }
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
