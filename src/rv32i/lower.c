#include "lower.h"

#include "../lower.h"
#include "../util.h"
#include "../vector.h"

struct hfa_info {
  bool fp;
  size_t size;
  struct ir_var_ty var_ty;
};

static bool try_get_hfa_info(struct ir_func *func,
                             const struct ir_var_ty *var_ty, size_t *num_int,
                             size_t *num_fp, struct hfa_info *hfa_info) {
  if (var_ty->ty != IR_VAR_TY_TY_UNION && var_ty->ty != IR_VAR_TY_TY_STRUCT) {
    return false;
  }

  *num_int = 0;
  *num_fp = 0;

  struct ir_var_ty_flattened info = ir_var_ty_info_flat(func->unit, var_ty);

  if (!info.num_fields || info.num_fields > 2) {
    return false;
  }

  if (ir_var_ty_is_fp(&info.fields[0].var_ty)) {
    (*num_fp)++;
    hfa_info[0] = (struct hfa_info){
        .fp = true,
        .size = ir_var_ty_info(func->unit, &info.fields[0].var_ty).size,
        .var_ty = info.fields[0].var_ty};
  } else if (ir_var_ty_is_integral(&info.fields[0].var_ty)) {
    (*num_int)++;
    hfa_info[0] = (struct hfa_info){
        .fp = false,
        .size = ir_var_ty_info(func->unit, &info.fields[0].var_ty).size,
        .var_ty = info.fields[0].var_ty};
  } else {
    return false;
  }

  if (info.num_fields > 1) {
    if (ir_var_ty_is_fp(&info.fields[1].var_ty)) {
      (*num_fp)++;
      hfa_info[1] = (struct hfa_info){
          .fp = true,
          .size = ir_var_ty_info(func->unit, &info.fields[1].var_ty).size,
          .var_ty = info.fields[1].var_ty};
    } else if (ir_var_ty_is_integral(&info.fields[1].var_ty)) {
      (*num_int)++;
      hfa_info[1] = (struct hfa_info){
          .fp = false,
          .size = ir_var_ty_info(func->unit, &info.fields[1].var_ty).size,
          .var_ty = info.fields[1].var_ty};
    } else {
      return false;
    }
  }

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

    struct hfa_info hfa_info[2];
    size_t num_int, num_fp;
    if (try_get_hfa_info(func, func_ty.ret_ty, &num_int, &num_fp,
                         &hfa_info[0])) {
      // nop
      *ret_info = (struct ir_param_info){.ty = IR_PARAM_INFO_TY_REGISTER,
                                         .var_ty = func_ty.ret_ty,
                                         .num_regs = num_int + num_fp};

      size_t ret_nsrn = 0;
      size_t ret_ngrn = 0;
      for (size_t i = 0; i < num_int + num_fp; i++) {
        struct hfa_info member_info = hfa_info[i];

        size_t idx;
        enum ir_reg_ty reg_ty;
        if (member_info.fp) {
          reg_ty = IR_REG_TY_FP;
          idx = ret_nsrn++;
        } else {
          reg_ty = IR_REG_TY_INTEGRAL;
          idx = ret_ngrn++;
        }

        ret_info->regs[i] = (struct ir_param_reg){
            .reg = {.ty = reg_ty, .idx = idx}, .size = member_info.size};
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

    struct hfa_info hfa_info[2];
    size_t num_int, num_fp;
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

    } else if (try_get_hfa_info(func, var_ty, &num_int, &num_fp,
                                &hfa_info[0])) {
      if (nsrn + num_fp < 8 && ngrn + num_int < 8) {
        struct ir_param_info param_info = {
            .ty = IR_PARAM_INFO_TY_REGISTER,
            .var_ty = var_ty,
            .num_regs = num_int + num_fp,
        };

        for (size_t j = 0; j < num_int + num_fp; j++) {
          struct hfa_info member_info = hfa_info[j];

          size_t idx;
          enum ir_reg_ty reg_ty;
          if (member_info.fp) {
            reg_ty = IR_REG_TY_FP;
            idx = nsrn++;
          } else {
            reg_ty = IR_REG_TY_INTEGRAL;
            idx = ngrn++;
          }

          // given this is a composite, we assume `source` contains a
          // pointer to it
          param_info.regs[j] = (struct ir_param_reg){
              .reg = {.ty = reg_ty, .idx = idx}, .size = member_info.size};

          vector_push_back(params, &member_info.var_ty);
        }

        vector_push_back(param_infos, &param_info);
        continue;
      }
    }

    if (info.size > 8) {
      // copy to mem
      var_ty = &IR_VAR_TY_POINTER;
      ty = IR_PARAM_INFO_TY_POINTER;
      info = ir_var_ty_info(func->unit, var_ty);
    }

    if (ir_var_ty_is_integral(var_ty) && info.size <= 4 && ngrn < 8) {
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
    // if ((ir_var_ty_is_aggregate(var_ty) ||
    //      (variadic && ir_var_ty_is_fp(var_ty))) &&
    //     ngrn < 8) {
    // dw_size <= (8 - ngrn)) {
    if (dw_size <= 2 && ngrn < 8) {
      size_t rem = MIN(dw_size, 8 - ngrn);
      struct ir_param_info param_info = {
          .ty = ty, .var_ty = var_ty, .num_regs = rem};

      for (size_t j = 0; j < rem; j++) {
        // given this is a composite, we assume `source` contains a
        // pointer to it
        param_info.regs[j] = (struct ir_param_reg){
            .reg = {.ty = IR_REG_TY_INTEGRAL, .idx = ngrn}, .size = 4};

        vector_push_back(params, &IR_VAR_TY_I32);

        dw_size--;
        ngrn++;
      }

      if (dw_size) {
        // dont push param, let lower recognise it
        // param_info = (struct ir_param_info){.ty = IR_PARAM_INFO_TY_STACK,
        //                                    .var_ty = &IR_VAR_TY_I32,
        //                                    .stack_offset = nsaa};
        // vector_push_back(param_infos, &param_info);

        param_info.split = true;
        param_info.stack_offset = nsaa;
        nsaa += 4;
      }
      vector_push_back(param_infos, &param_info);

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

  struct ir_call_info call_info = {
      .ret = ret_info,
      .stack_size = nsaa,
      .num_gp_used = ngrn,
      .num_fp_used = nsrn
  };

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

    if (offset > MAX_IMM_SIZE) {
      return false;
    }

    return true;
  }
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

  if (!can_contain_lcl_addr(func, lcl, op->addr_offset.offset)) {
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
    if (!can_contain_lcl_addr(func, addr->addr.lcl, 0)) {
      return;
    }

    op->load.addr = ir_alloc_contained_op(func, addr, op);
  } else if (addr->ty == IR_OP_TY_ADDR_OFFSET &&
             !(addr->flags & IR_OP_FLAG_CONTAINED)) {
    struct ir_op_addr_offset addr_offset = addr->addr_offset;

    if (!addr_offset.index && addr_offset.offset <= MAX_IMM_SIZE) {
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
    if (!can_contain_lcl_addr(func, addr->addr.lcl, 0)) {
      return;
    }

    op->store.addr = ir_alloc_contained_op(func, addr, op);
  } else if (addr->ty == IR_OP_TY_ADDR_OFFSET &&
             !(addr->flags & IR_OP_FLAG_CONTAINED)) {
    struct ir_op_addr_offset addr_offset = addr->addr_offset;

    if (!addr_offset.index && addr_offset.offset <= MAX_IMM_SIZE) {
      op->store.addr = ir_alloc_contained_op(func, addr, op);
    }
  }
}

static struct ir_lcl *lower_va_args(struct ir_func *func) {
  struct ir_call_info info = func->call_info;

  size_t save_sz = ((8 - info.num_gp_used) * 4);

  DEBUG_ASSERT(save_sz % 4 == 0, "because we use type I64 for alignment, "
                                 "save_sz must be divisible by 8");
  save_sz = save_sz / 4;

  struct ir_var_ty *el_ty =
      arena_alloc_init(func->arena, sizeof(*el_ty), &IR_VAR_TY_I32);
  struct ir_var_ty save_ty = {.ty = IR_VAR_TY_TY_ARRAY,
                              .array = {
                                  // TODO: make constant ptr
                                  .underlying = el_ty,
                                  .num_elements = save_sz,
                              }};

  // FIXME: this local must be _directly_ beneath SP but we can't easily cause that here
  struct ir_lcl *lcl = ir_add_local(func, &save_ty);

  struct ir_op *movs = func->first->first->last;
  struct ir_op *base = ir_insert_after_op(func, movs,
                                          IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
  base->addr = (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = lcl};

  struct ir_op *last = base;

  // need to save all the registers in args
  // only integer registers are used for variadics
  for (size_t i = info.num_gp_used, idx = 0; i < 8; i++, idx++) {
    struct ir_op *addr;

    if (i) {
      addr = ir_insert_after_op(func, last, IR_OP_TY_ADDR_OFFSET,
                                IR_VAR_TY_POINTER);
      addr->addr_offset =
          (struct ir_op_addr_offset){.base = base, .offset = idx * 4};
      last = addr;
    } else {
      addr = base;
    }

    struct ir_op *value =
        ir_insert_after_op(func, movs, IR_OP_TY_MOV, IR_VAR_TY_I32);
    value->flags |= IR_OP_FLAG_PARAM | IR_OP_FLAG_FIXED_REG;
    value->mov = (struct ir_op_mov){.value = NULL};
    value->reg = (struct ir_reg){.ty = IR_REG_TY_INTEGRAL, .idx = i};
    movs = value;

    struct ir_op *save =
        ir_insert_after_op(func, last, IR_OP_TY_STORE, IR_VAR_TY_NONE);
    save->store = (struct ir_op_store){
        .ty = IR_OP_STORE_TY_ADDR, .addr = addr, .value = value};

    last = save;
  }

  return lcl;
}

static void lower_va_start(struct ir_func *func, struct ir_lcl *save_lcl,
                           struct ir_op *op) {
  // pointer to fp - <reg save size> if not all gp used
  // else fp + <stack used by named args>

  struct ir_op *addr = op->va_start.list_addr;

  struct ir_op *value;
  if (func->call_info.num_gp_used < 8) {
    // fp - <reg save size>
    // (which is just the save_lcl because ABI requires the save to be directly after SP)

    value = ir_replace_op(func, op, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
    value->addr = (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = save_lcl};
  } else if (func->call_info.stack_size) {
    // fp + <stack used by named args>

    struct ir_op *sp = ir_replace_op(func, op, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
    sp->addr = (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = save_lcl};

    value =
        ir_insert_after_op(func, sp, IR_OP_TY_ADDR_OFFSET, IR_VAR_TY_POINTER);
    value->addr_offset = (struct ir_op_addr_offset){
        .base = sp, .offset = func->call_info.stack_size};
  } else {
    // fp
    value = ir_replace_op(func, op, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
    value->addr = (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = save_lcl};
  }

  struct ir_op *store =
      ir_insert_after_op(func, value, IR_OP_TY_STORE, IR_VAR_TY_NONE);
  store->store = (struct ir_op_store){
      .ty = IR_OP_STORE_TY_ADDR, .addr = addr, .value = value};

}

static void lower_va_arg(UNUSED struct ir_func *func, UNUSED struct ir_op *op) {
  TODO("x64 va_arg");
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

      struct ir_lcl *save_lcl = NULL;
      if (func->flags & IR_FUNC_FLAG_USES_VA_ARGS) {
        save_lcl = lower_va_args(func);
      }

      struct ir_basicblock *basicblock = func->first;
      while (basicblock) {
        struct ir_stmt *stmt = basicblock->first;

        while (stmt) {
          struct ir_op *op = stmt->first;

          while (op) {
            switch (op->ty) {
            case IR_OP_TY_VA_START:
              lower_va_start(func, save_lcl, op);
              break;
            case IR_OP_TY_VA_ARG:
              lower_va_arg(func, op);
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

                struct ir_op *add = ir_replace_op(func, op, IR_OP_TY_BINARY_OP,
                                                  IR_VAR_TY_POINTER);
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
