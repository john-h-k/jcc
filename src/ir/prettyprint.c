#include "prettyprint.h"

#include "../graphwriter.h"
#include "../util.h"
#include "ir.h"

#include <wchar.h>

static const char *unary_op_string(enum ir_op_unary_op_ty ty) {
  switch (ty) {
  case IR_OP_UNARY_OP_TY_FNEG:
    return "f-";
  case IR_OP_UNARY_OP_TY_FSQRT:
    return "fsqrt";
  case IR_OP_UNARY_OP_TY_FABS:
    return "fabs";
  case IR_OP_UNARY_OP_TY_NEG:
    return "-";
  case IR_OP_UNARY_OP_TY_LOGICAL_NOT:
    return "!";
  case IR_OP_UNARY_OP_TY_NOT:
    return "~";
  }
}

static const char *cast_op_string(enum ir_op_cast_op_ty ty) {
  switch (ty) {
  case IR_OP_CAST_OP_TY_SEXT:
    return "sext";
  case IR_OP_CAST_OP_TY_ZEXT:
    return "zext";
  case IR_OP_CAST_OP_TY_TRUNC:
    return "trunc";
  case IR_OP_CAST_OP_TY_CONV:
    return "conv";
  case IR_OP_CAST_OP_TY_UCONV:
    return "uconv";
  case IR_OP_CAST_OP_TY_SCONV:
    return "sconv";
  }
}

static const char *binary_op_string(enum ir_op_binary_op_ty ty) {
  switch (ty) {
  case IR_OP_BINARY_OP_TY_EQ:
    return "==";
  case IR_OP_BINARY_OP_TY_NEQ:
    return "!=";
  case IR_OP_BINARY_OP_TY_SGT:
    return "s>";
  case IR_OP_BINARY_OP_TY_SGTEQ:
    return "s>=";
  case IR_OP_BINARY_OP_TY_UGT:
    return "u>";
  case IR_OP_BINARY_OP_TY_UGTEQ:
    return "u>=";
  case IR_OP_BINARY_OP_TY_SLT:
    return "s<";
  case IR_OP_BINARY_OP_TY_SLTEQ:
    return "s<=";
  case IR_OP_BINARY_OP_TY_ULT:
    return "u<";
  case IR_OP_BINARY_OP_TY_ULTEQ:
    return "u<=";
  case IR_OP_BINARY_OP_TY_FEQ:
    return "f==";
  case IR_OP_BINARY_OP_TY_FNEQ:
    return "!=";
  case IR_OP_BINARY_OP_TY_FGT:
    return "f>";
  case IR_OP_BINARY_OP_TY_FGTEQ:
    return "f>=";
  case IR_OP_BINARY_OP_TY_FLT:
    return "f<";
  case IR_OP_BINARY_OP_TY_FLTEQ:
    return "f<=";
  case IR_OP_BINARY_OP_TY_LSHIFT:
    return "<<";
  case IR_OP_BINARY_OP_TY_SRSHIFT:
    return "s>>";
  case IR_OP_BINARY_OP_TY_URSHIFT:
    return "u>>";
  case IR_OP_BINARY_OP_TY_AND:
    return "&";
  case IR_OP_BINARY_OP_TY_OR:
    return "|";
  case IR_OP_BINARY_OP_TY_XOR:
    return "^";
  case IR_OP_BINARY_OP_TY_ADD:
    return "+";
  case IR_OP_BINARY_OP_TY_SUB:
    return "-";
  case IR_OP_BINARY_OP_TY_MUL:
    return "*";
  case IR_OP_BINARY_OP_TY_SDIV:
    return "s/";
  case IR_OP_BINARY_OP_TY_UDIV:
    return "u/";
  case IR_OP_BINARY_OP_TY_SMOD:
    return "s%";
  case IR_OP_BINARY_OP_TY_UMOD:
    return "u%";
  case IR_OP_BINARY_OP_TY_FADD:
    return "f+";
  case IR_OP_BINARY_OP_TY_FSUB:
    return "f-";
  case IR_OP_BINARY_OP_TY_FMUL:
    return "f*";
  case IR_OP_BINARY_OP_TY_FDIV:
    return "f/";
  case IR_OP_BINARY_OP_TY_FMAX:
    return "fmax";
  case IR_OP_BINARY_OP_TY_FMIN:
    return "fmin";
  }
}

static void debug_print_func_ty_string(FILE *file,
                                       const struct ir_var_func_ty *func_ty) {
  fprintf(file, "(");
  for (size_t i = 0; i < func_ty->num_params; i++) {
    debug_print_var_ty_string(file, &func_ty->params[i]);
    if (i + 1 < func_ty->num_params) {
      fprintf(file, ", ");
    }
  }
  fprintf(file, ")");
  fprintf(file, " -> ");
  debug_print_var_ty_string(file, func_ty->ret_ty);
}

static void debug_print_param_info(FILE *file, UNUSED struct ir_unit *iru,
                                   const struct ir_param_info *info) {
  switch (info->ty) {
  case IR_PARAM_INFO_TY_REGISTER:
    fprintf(file, "    register ");
    goto print_regs;

  case IR_PARAM_INFO_TY_POINTER:
    fprintf(file, "    pointer ");
    goto print_regs;

  print_regs: {
    fprintf(file, "(%zu registers): \n", info->num_regs);

    for (size_t j = 0; j < info->num_regs; j++) {
      struct ir_param_reg reg = info->regs[j];

      fprintf(file, "      ");
      debug_print_ir_reg(stderr, reg.reg);
      fprintf(file, " (size=%zu)", reg.size);

      fprintf(file, "\n");
    }

    break;
  }
  case IR_PARAM_INFO_TY_STACK:
    fprintf(file, "    (stack): offset=%zu", info->stack_offset);
    break;
  }
}

void debug_print_func_info(FILE *file, struct ir_unit *iru,
                           const struct ir_func_info *func_info) {
  fprintf(file, "FUNC_INFO: ");
  debug_print_func_ty_string(file, &func_info->func_ty);
  fprintf(file, "\n");

  if (func_info->call_info.num_params) {
    fprintf(file, "  PARAMS: \n");
    for (size_t i = 0; i < func_info->call_info.num_params; i++) {
      struct ir_param_info *info = &func_info->call_info.params[i];

      debug_print_param_info(file, iru, info);

      fprintf(file, "\n");
    }
  }

  if (func_info->call_info.ret) {
    fprintf(file, "  RET: \n");
    debug_print_param_info(file, iru, func_info->call_info.ret);
  }
}

void debug_print_var_ty_string(FILE *file, const struct ir_var_ty *var_ty) {
  switch (var_ty->ty) {
  case IR_VAR_TY_TY_NONE: {
    fprintf(file, "<none>");
    return;
  }
  case IR_VAR_TY_TY_VARIADIC: {
    fprintf(file, "<VARIADIC>");
    return;
  }
  case IR_VAR_TY_TY_POINTER: {
    fprintf(file, "PTR");
    return;
  }
  case IR_VAR_TY_TY_ARRAY: {
    fprintf(file, "ARRAY [ ");
    debug_print_var_ty_string(file, var_ty->array.underlying);
    fprintf(file, ", ");
    fprintf(file, "%zu", var_ty->array.num_elements);
    fprintf(file, " ]");
    return;
  }
  case IR_VAR_TY_TY_FUNC: {
    debug_print_func_ty_string(file, &var_ty->func);
    return;
  }
  case IR_VAR_TY_TY_STRUCT: {
    struct ir_var_ty_info info = ir_var_ty_info(NULL, var_ty);
    fprintf(file, "STRUCT (sz=%zu, align=%zu) [ ", info.size, info.alignment);
    for (size_t i = 0; i < var_ty->aggregate.num_fields; i++) {
      debug_print_var_ty_string(file, &var_ty->aggregate.fields[i]);

      if (i + 1 < var_ty->aggregate.num_fields) {
        fprintf(file, ", ");
      }
    }
    fprintf(file, " ]");
    break;
  }
  case IR_VAR_TY_TY_UNION: {
    struct ir_var_ty_info info = ir_var_ty_info(NULL, var_ty);
    fprintf(file, "UNION (sz=%zu, align=%zu) [ ", info.size, info.alignment);
    for (size_t i = 0; i < var_ty->aggregate.num_fields; i++) {
      debug_print_var_ty_string(file, &var_ty->aggregate.fields[i]);

      if (i + 1 < var_ty->aggregate.num_fields) {
        fprintf(file, ", ");
      }
    }
    fprintf(file, " ]");
    break;
  }
  case IR_VAR_TY_TY_PRIMITIVE: {
    const char *name;
    switch (var_ty->primitive) {
    case IR_VAR_PRIMITIVE_TY_I1:
      name = "i1";
      break;
    case IR_VAR_PRIMITIVE_TY_I8:
      name = "i8";
      break;
    case IR_VAR_PRIMITIVE_TY_I16:
      name = "i16";
      break;
    case IR_VAR_PRIMITIVE_TY_I32:
      name = "i32";
      break;
    case IR_VAR_PRIMITIVE_TY_I64:
      name = "i64";
      break;
    case IR_VAR_PRIMITIVE_TY_I128:
      name = "i128";
      break;
    case IR_VAR_PRIMITIVE_TY_F16:
      name = "f16";
      break;
    case IR_VAR_PRIMITIVE_TY_F32:
      name = "f32";
      break;
    case IR_VAR_PRIMITIVE_TY_F64:
      name = "f64";
      break;
    }

    fprintf(file, "%s", name);
    return;
  }
  }
}

static void debug_phi_string(FILE *file, struct ir_op_phi *phi) {
  for (size_t i = 0; i < phi->num_values; i++) {
    fprintf(file, "%%%zu (@%zu)", phi->values[i].value->id,
            phi->values[i].basicblock->id);

    if (i + 1 < phi->num_values) {
      fprintf(file, ", ");
    }
  }
}

static void debug_call_arg_string(FILE *file, struct ir_op_call *call) {
  for (size_t i = 0; i < call->num_args; i++) {
    fprintf(file, "%%%zu", call->args[i]->id);

    if (i + 1 < call->num_args) {
      fprintf(file, ", ");
    }
  }
}

void debug_print_ir_reg(FILE *file, struct ir_reg reg) {
  switch (reg.ty) {
  case IR_REG_TY_NONE:
    fprintf(file, "UNASSIGNED");
    break;
  case IR_REG_TY_SPILLED:
    fprintf(file, "SPILLED");
    break;
  case IR_REG_TY_FLAGS:
    fprintf(file, "FLAGS");
    break;
  case IR_REG_TY_INTEGRAL:
    fprintf(file, "R%zu", reg.idx);
    break;
  case IR_REG_TY_FP:
    fprintf(file, "F%zu", reg.idx);
    break;
  }
}

static void debug_lhs(FILE *file, struct ir_op *ir) {
  fprintf(file, "%%%zu (", ir->id);
  debug_print_var_ty_string(file, &ir->var_ty);
  fprintf(file, ") = ");
}

static void debug_print_lcl_alloc(FILE *file, struct ir_lcl_alloc *lcl_alloc) {
  fprintf(file, "#%zd (size=%zu, padding=%zu)", lcl_alloc->offset,
          lcl_alloc->size, lcl_alloc->padding);
}

enum print_op_ctx {
  PRINT_OP_CTX_TOP_LEVEL,
  PRINT_OP_CTX_USE,
};

static void debug_print_op_with_ctx(FILE *file, struct ir_op *op,
                                    const struct debug_print_ir_opts *opts,
                                    enum print_op_ctx ctx);

static void debug_print_op_use(FILE *file, struct ir_op *op,
                               const struct debug_print_ir_opts *opts) {
  // DEBUG_ASSERT(ir->stmt,
  //              "op used by other op but had no stmt (likely detached)");

  if (op->id == DETACHED_OP) {
    fprintf(file, "DETACHED ");
    debug_print_op_with_ctx(file, op, opts, PRINT_OP_CTX_USE);
  } else if (op->flags & IR_OP_FLAG_CONTAINED) {
    debug_print_op_with_ctx(file, op, opts, PRINT_OP_CTX_USE);
  } else {
    fprintf(file, "%%%zu", op->id);
  }
}

static void debug_print_glb_name(FILE *file, struct arena_allocator *arena,
                                 struct ir_glb *glb,
                                 const struct debug_print_ir_opts *opts) {
  if (glb->name) {
    const char *name = glb->name;

    if (opts->demangle_sym) {
      name = opts->demangle_sym(arena, glb->name);
    }

    if (!name) {
      fprintf(stderr, "wtf\n");
    }
    fprintf(stderr, "P=%p\n", (const void *)name);
    fflush(file);
    fflush(file);
    size_t len = strlen(name);
    for (size_t i = 0; i < len; i++) {
      fputc(name[i], file);
    }
    // fprintf(file, "PTR(%p)\n %s\n", (const void *)name, name);
    fflush(file);
  }
}

static void
debug_print_glb_name_comment(FILE *file, struct arena_allocator *arena,
                             struct ir_glb *glb,
                             const struct debug_print_ir_opts *opts) {
  if (!glb->name) {
    return;
  }

  fprintf(file, " ; \"");
  debug_print_glb_name(file, arena, glb, opts);
  fprintf(file, "\"");
}

static void debug_print_op_with_ctx(FILE *file, struct ir_op *op,
                                    const struct debug_print_ir_opts *opts,
                                    enum print_op_ctx ctx) {
  if (ctx != PRINT_OP_CTX_USE && op->comment) {
    fprintf(file, "// %s\n", op->comment);
  }

  if (ctx != PRINT_OP_CTX_USE) {
    debug_lhs(file, op);
  }

  switch (op->ty) {
  case IR_OP_TY_UNKNOWN:
    BUG("unknown op!");
  case IR_OP_TY_SELECT:
    fprintf(file, "select ");
    debug_print_op_use(file, op->select.cond, opts);
    fprintf(file, ", ");
    debug_print_op_use(file, op->select.true_op, opts);
    fprintf(file, ", ");
    debug_print_op_use(file, op->select.false_op, opts);
    break;
  case IR_OP_TY_VA_START:
    fprintf(file, "va_start ");
    debug_print_op_use(file, op->va_start.list_addr, opts);
    break;
  case IR_OP_TY_VA_ARG:
    fprintf(file, "va_arg ");
    debug_print_var_ty_string(file, &op->va_arg.arg_ty);
    fprintf(file, ", ");
    debug_print_op_use(file, op->va_arg.list_addr, opts);
    break;
  case IR_OP_TY_UNDF:
    fprintf(file, "UNDF");
    break;
  case IR_OP_TY_GATHER:
    fprintf(file, "gather { ");
    for (size_t i = 0; i < op->gather.num_values; i++) {
      struct ir_gather_value value = op->gather.values[i];

      fprintf(file, ".%zu = %%%zu", value.field_idx, value.value->id);

      if (i + 1 != op->gather.num_values) {
        fprintf(file, ", ");
      }
    }

    fprintf(file, " }");
    break;
  case IR_OP_TY_CALL: {
    fprintf(file, "call ");

    debug_print_op_use(file, op->call.target, opts);
    fprintf(file, " ( ");
    debug_call_arg_string(file, &op->call);
    fprintf(file, " )");
    break;
  }
  case IR_OP_TY_PHI:
    fprintf(file, "phi [ ");
    debug_phi_string(file, &op->phi);
    fprintf(file, " ]");
    break;
  case IR_OP_TY_MOV:
    if (op->mov.value) {
      debug_print_op_use(file, op->mov.value, opts);
      fprintf(file, " : (");
      debug_print_ir_reg(file, op->mov.value->reg);
      fprintf(file, " -> ");
      debug_print_ir_reg(file, op->reg);
      fprintf(file, ")");
    } else {
      fprintf(file, "<PARAM>");
    }
    break;
  case IR_OP_TY_CNST:
    switch (op->cnst.ty) {
    case IR_OP_CNST_TY_FLT:
      fprintf(file, "%Lf", op->cnst.flt_value);
      break;
    case IR_OP_CNST_TY_INT:
      fprintf(file, "%llu", op->cnst.int_value);
      break;
    }
    break;
  case IR_OP_TY_BINARY_OP:
    debug_print_op_use(file, op->binary_op.lhs, opts);
    fprintf(file, " %s ", binary_op_string(op->binary_op.ty));
    debug_print_op_use(file, op->binary_op.rhs, opts);
    break;
  case IR_OP_TY_UNARY_OP:
    fprintf(file, "%s ", unary_op_string(op->unary_op.ty));
    debug_print_op_use(file, op->unary_op.value, opts);
    break;
  case IR_OP_TY_CAST_OP:
    fprintf(file, "%s ", cast_op_string(op->cast_op.ty));
    debug_print_op_use(file, op->cast_op.value, opts);
    break;
  case IR_OP_TY_STORE:
    switch (op->store.ty) {
    case IR_OP_STORE_TY_LCL:
      if (op->store.lcl) {
        fprintf(file, "store.lcl LCL(%zu), %%%zu", op->store.lcl->id,
                op->store.value->id);
      } else {
        fprintf(file, "store.lcl LCL(UNASSIGNED), %%%zu", op->store.value->id);
      }
      break;
    case IR_OP_STORE_TY_GLB:
      if (op->load.glb) {
        fprintf(file, "store.glb GLB(%zu), %%%zu", op->store.glb->id,
                op->store.value->id);
        debug_print_glb_name_comment(file, op->stmt->basicblock->func->arena,
                                     op->addr.glb, opts);
      } else {
        fprintf(file, "store.glb GLB(UNASSIGNED), %%%zu", op->store.value->id);
      }
      break;
    case IR_OP_STORE_TY_ADDR:
      fprintf(file, "store.addr [");
      debug_print_op_use(file, op->store.addr, opts);
      fprintf(file, "], ");
      debug_print_op_use(file, op->store.value, opts);
      break;
    }

    break;
  case IR_OP_TY_LOAD:
    switch (op->load.ty) {
    case IR_OP_LOAD_TY_LCL:
      if (op->load.lcl) {
        fprintf(file, "load.lcl LCL(%zu)", op->load.lcl->id);
      } else {
        fprintf(file, "load.lcl LCL(UNASSIGNED)");
      }
      break;
    case IR_OP_LOAD_TY_GLB:
      if (op->load.glb) {
        fprintf(file, "load.glb GLB(%zu)", op->load.glb->id);
        debug_print_glb_name_comment(file, op->stmt->basicblock->func->arena,
                                     op->addr.glb, opts);
      } else {
        fprintf(file, "load.glb GLB(UNASSIGNED)");
      }
      break;
    case IR_OP_LOAD_TY_ADDR:
      fprintf(file, "load.addr [");
      debug_print_op_use(file, op->load.addr, opts);
      fprintf(file, "]");
      break;
    }

    break;

  case IR_OP_TY_STORE_BITFIELD: {
    struct ir_bitfield bitfield = op->store_bitfield.bitfield;

    switch (op->store_bitfield.ty) {
    case IR_OP_STORE_TY_LCL:
      if (op->store_bitfield.lcl) {
        fprintf(file, "store.bitfield.lcl (#%zu, #%zu) LCL(%zu), %%%zu",
                bitfield.offset, bitfield.width, op->store_bitfield.lcl->id,
                op->store_bitfield.value->id);
      } else {
        fprintf(file, "store.bitfield.lcl (#%zu, #%zu) LCL(UNASSIGNED), %%%zu",
                bitfield.offset, bitfield.width, op->store_bitfield.value->id);
      }
      break;
    case IR_OP_STORE_TY_GLB:
      if (op->store_bitfield.glb) {
        fprintf(file, "store.bitfield.glb (#%zu, #%zu) GLB(%zu), %%%zu",
                bitfield.offset, bitfield.width, op->store_bitfield.glb->id,
                op->store_bitfield.value->id);
        debug_print_glb_name_comment(file, op->stmt->basicblock->func->arena,
                                     op->addr.glb, opts);
      } else {
        fprintf(file, "store.bitfield.glb (#%zu, #%zu) GLB(UNASSIGNED), %%%zu",
                bitfield.offset, bitfield.width, op->store_bitfield.value->id);
      }
      break;
    case IR_OP_STORE_TY_ADDR:
      fprintf(file, "store.bitfield.addr (#%zu, #%zu) [", bitfield.offset,
              bitfield.width);
      debug_print_op_use(file, op->store_bitfield.addr, opts);
      fprintf(file, "], ");
      debug_print_op_use(file, op->store_bitfield.value, opts);
      break;
    }

    break;
  }
  case IR_OP_TY_LOAD_BITFIELD: {
    struct ir_bitfield bitfield = op->load_bitfield.bitfield;

    switch (op->load_bitfield.ty) {
    case IR_OP_LOAD_TY_LCL:
      if (op->load_bitfield.glb) {
        fprintf(file, "load.bitfield.lcl (#%zu, #%zu) LCL(%zu)",
                bitfield.offset, bitfield.width, op->load_bitfield.lcl->id);
      } else {
        fprintf(file, "load.bitfield.lcl (#%zu, #%zu) LCL(UNASSIGNED)",
                bitfield.offset, bitfield.width);
      }
      break;
    case IR_OP_LOAD_TY_GLB:
      if (op->load_bitfield.glb) {
        fprintf(file, "load.bitfield.glb (#%zu, #%zu) GLB(%zu)",
                bitfield.offset, bitfield.width, op->load_bitfield.glb->id);
        debug_print_glb_name_comment(file, op->stmt->basicblock->func->arena,
                                     op->addr.glb, opts);
      } else {
        fprintf(file, "load.bitfield.glb (#%zu, #%zu) GLB(UNASSIGNED)",
                bitfield.offset, bitfield.width);
      }
      break;
    case IR_OP_LOAD_TY_ADDR:
      fprintf(file, "load.bitfield.addr (#%zu, #%zu) [", bitfield.offset,
              bitfield.width);
      debug_print_op_use(file, op->load_bitfield.addr, opts);
      fprintf(file, "]");
      break;
    }

    break;
  }
  case IR_OP_TY_ADDR_OFFSET:
    fprintf(file, "addr.off ");
    debug_print_op_use(file, op->addr_offset.base, opts);
    if (op->addr_offset.index) {
      fprintf(file, " + ");
      debug_print_op_use(file, op->addr_offset.index, opts);

      if (op->addr_offset.scale != 1) {
        fprintf(file, " * #%zu", op->addr_offset.scale);
      }
    }

    if (op->addr_offset.offset) {
      fprintf(file, " + #%zu", op->addr_offset.offset);
    }
    break;
  case IR_OP_TY_ADDR:
    switch (op->addr.ty) {
    case IR_OP_ADDR_TY_LCL:
      if (ctx == PRINT_OP_CTX_USE) {
        fprintf(file, "LCL(%zu)", op->addr.lcl->id);
      } else {
        switch (op->addr.lcl->alloc_ty) {
        case IR_LCL_ALLOC_TY_NONE:
          fprintf(file, "addr LCL(%zu)", op->addr.lcl->id);
          break;
        case IR_LCL_ALLOC_TY_NORMAL:
          fprintf(file, "addr LCL(%zu) ; ", op->addr.lcl->id);
          debug_print_lcl_alloc(file, &op->addr.lcl->alloc);
          break;
        case IR_LCL_ALLOC_TY_FIXED:
          fprintf(file, "addr LCL(%zu) ; FIXED ", op->addr.lcl->id);
          debug_print_lcl_alloc(file, &op->addr.lcl->alloc);
          break;
        }
      }
      break;
    case IR_OP_ADDR_TY_GLB:
      if (ctx == PRINT_OP_CTX_USE) {
        debug_print_glb_name(file, op->stmt->basicblock->func->arena,
                             op->addr.glb, opts);
      } else {
        fprintf(file, "addr GLB(%zu)", op->addr.glb->id);
        debug_print_glb_name_comment(file, op->stmt->basicblock->func->arena,
                                     op->addr.glb, opts);
      }
      break;
    }
    break;
  case IR_OP_TY_BR: {
    struct ir_basicblock *bb = op->stmt->basicblock;
    struct ir_basicblock *target = bb->merge.target;
    if (target) {
      fprintf(file, "br @%zu", target->id);
    } else {
      fprintf(file, "br <!NULL>");
    }
    break;
  }
  case IR_OP_TY_BR_COND:
    fprintf(file, "br.cond ");
    debug_print_op_use(file, op->br_cond.cond, opts);
    fprintf(file, ", TRUE(@%zu), FALSE(@%zu)",
            op->stmt->basicblock->split.true_target->id,
            op->stmt->basicblock->split.false_target->id);
    break;
  case IR_OP_TY_BR_SWITCH:
    fprintf(file, "br.switch %%%zu, [\n", op->br_switch.value->id);

    size_t num_cases = op->stmt->basicblock->switch_case.num_cases;
    struct ir_split_case *cases = op->stmt->basicblock->switch_case.cases;
    for (size_t i = 0; i < num_cases; i++) {
      struct ir_split_case *split_case = &cases[i];

      fprintf(file, "    # %llu -> @%zu\n", split_case->value,
              split_case->target->id);
    }

    fprintf(file, "    DEFAULT -> @%zu\n",
            op->stmt->basicblock->switch_case.default_target->id);
    fprintf(file, "  ]");
    break;
  case IR_OP_TY_RET:
    if (op->ret.value) {
      fprintf(file, "return ");
      debug_print_op_use(file, op->ret.value, opts);
    } else {
      fprintf(file, "return");
    }
    break;
  case IR_OP_TY_BITFIELD_EXTRACT:
    fprintf(file, "bitfield.extract #(%zu, %zu), ",
            op->bitfield_extract.bitfield.offset,
            op->bitfield_extract.bitfield.width);
    debug_print_op_use(file, op->bitfield_extract.value, opts);
    break;
  case IR_OP_TY_BITFIELD_INSERT:
    fprintf(file, "bitfield.insert #(%zu, %zu), ",
            op->bitfield_insert.bitfield.offset,
            op->bitfield_insert.bitfield.width);
    debug_print_op_use(file, op->bitfield_insert.target, opts);
    fprintf(file, ", ");
    debug_print_op_use(file, op->bitfield_insert.value, opts);
    break;
  case IR_OP_TY_MEM_SET:
    fprintf(file, "mem.set ");
    debug_print_op_use(file, op->mem_set.addr, opts);
    fprintf(file, ", #%zu, #%d", op->mem_set.length, op->mem_set.value);
    break;
  case IR_OP_TY_MEM_COPY:
    fprintf(file, "mem.copy ");
    debug_print_op_use(file, op->mem_copy.dest, opts);
    fprintf(file, ", ");
    debug_print_op_use(file, op->mem_copy.source, opts);
    fprintf(file, ", #%zu", op->mem_set.length);
    break;
  }
}

void debug_print_op(FILE *file, struct ir_op *op,
                    const struct debug_print_ir_opts *opts) {
  debug_print_op_with_ctx(file, op, opts, PRINT_OP_CTX_TOP_LEVEL);
  fprintf(file, "\n");
}

struct prettyprint_file_metadata {
  FILE *file;
  int ctr_pad;
  size_t ctr;
};

static void prettyprint_begin_visit_stmt_file(
    UNUSED_ARG(struct ir_func *irb), struct ir_stmt *stmt,
    UNUSED const struct debug_print_ir_opts *opts, void *metadata) {
  struct prettyprint_file_metadata *fm = metadata;

  if (stmt->comment) {
    fprintf(fm->file, "// %s\n", stmt->comment);
  }

  fprintf(fm->file, "STMT ");
  if (stmt->flags) {
    fprintf(fm->file, "[ ");

    bool first = true;
#define PRINT_FLAG(name, str)                                                  \
  if (stmt->flags & IR_STMT_FLAG_##name) {                                     \
    fprintf(fm->file, first ? "." str : ", ." str);                            \
    first = false;                                                             \
  }

    PRINT_FLAG(PHI, "phi");
    PRINT_FLAG(PARAM, "param");
    PRINT_FLAG(PHI_MOV, "phi_mov");

#undef PRINT_FLAG

    fprintf(fm->file, " ] ");
  }
  fprintf(fm->file, "$ %03zu\n", stmt->id);
}

static void prettyprint_begin_visit_basicblock_file(
    UNUSED_ARG(struct ir_func *irb), struct ir_basicblock *basicblock,
    UNUSED const struct debug_print_ir_opts *opts, void *metadata) {
  struct prettyprint_file_metadata *fm = metadata;

  fprintf(fm->file, "\n\n");
  if (basicblock->comment) {
    fprintf(fm->file, "// %s\n\n", basicblock->comment);
  }
  fprintf(fm->file, "BB @ %03zu", basicblock->id);

  if (basicblock->num_preds) {
    fprintf(fm->file, " PREDS = [ ");
    for (size_t i = 0; i < basicblock->num_preds; i++) {
      fprintf(fm->file, "@%zu", basicblock->preds[i]->id);

      if (i + 1 != basicblock->num_preds) {
        fprintf(fm->file, ", ");
      }
    }
    fprintf(fm->file, " ]");
  }

  switch (basicblock->ty) {
  case IR_BASICBLOCK_TY_RET:
    fprintf(fm->file, " RET");
    break;
  case IR_BASICBLOCK_TY_SPLIT:
    fprintf(fm->file, " SPLIT");
    break;
  case IR_BASICBLOCK_TY_MERGE:
    fprintf(fm->file, " MERGE");
    break;
  case IR_BASICBLOCK_TY_SWITCH:
    fprintf(fm->file, " SWITCH");
    break;
  }

  fprintf(fm->file, "\n");
}

static void prettyprint_end_visit_basicblock_file(
    UNUSED_ARG(struct ir_func *irb),
    UNUSED_ARG(struct ir_basicblock *basicblock),
    UNUSED const struct debug_print_ir_opts *opts, void *metadata) {
  struct prettyprint_file_metadata *fm = metadata;

  fprintf(fm->file, "\n");
}

static void prettyprint_visit_op_file(struct ir_op *op,
                                      const struct debug_print_ir_opts *opts,
                                      void *metadata) {
  if (op->flags & IR_OP_FLAG_CONTAINED) {
    return;
  }

  int op_pad = /* guess */ 70;
  int flag_pad = 40;

  struct prettyprint_file_metadata *fm = metadata;

  bool supports_pos = ftell(fm->file) != -1;

  fprintf(fm->file, "%0*zu: ", fm->ctr_pad, fm->ctr++);

  long pos = ftell(fm->file);
  debug_print_op_with_ctx(fm->file, op, opts, PRINT_OP_CTX_TOP_LEVEL);

  if (supports_pos && ftell(fm->file) == pos) {
    // no line was written
    return;
  }

  long width = ftell(fm->file) - pos;
  long pad = op_pad - width;

  if (supports_pos && pad > 0) {
    fprintf(fm->file, "%*s", (int)pad, "");
  } else {
    fprintf(fm->file, "%*s", 50, "");
  }

  pos = ftell(fm->file);

  if (op->flags) {
    fprintf(fm->file, "[ ");

    bool first = true;
#define PRINT_FLAG(name, str)                                                  \
  if (op->flags & IR_OP_FLAG_##name) {                                         \
    fprintf(fm->file, first ? "." str : ", ." str);                            \
    first = false;                                                             \
  }

    PRINT_FLAG(MUST_SPILL, "must_spill");
    PRINT_FLAG(PARAM, "param");
    PRINT_FLAG(VARIADIC_PARAM, "variadic_param");
    PRINT_FLAG(SPILL, "spill");
    PRINT_FLAG(CONTAINED, "contained");
    PRINT_FLAG(FIXED_REG, "fixed_reg");
    PRINT_FLAG(SIDE_EFFECTS, "side_effects");
    PRINT_FLAG(SPILLED, "spilled");
    PRINT_FLAG(PHI_MOV, "phi_mov");
    PRINT_FLAG(READS_DEST, "reads_dest");
    PRINT_FLAG(PROMOTED, "promoted");
    PRINT_FLAG(ETERNAL, "eternal");

#undef PRINT_FLAG

    fprintf(fm->file, " ]");
  }

  width = ftell(fm->file) - pos;
  pad = flag_pad - width;

  if (supports_pos && pad > 0) {
    fprintf(fm->file, "%*s", (int)pad, "");
  } else {
    fprintf(fm->file, "%*s", 20, "");
  }

  if (opts->cb) {
    fprintf(fm->file, " | ");
    opts->cb(fm->file, op, opts->cb_metadata);
  }

  fprintf(fm->file, " | ");

  if (!ir_op_produces_value(op)) {
    fprintf(fm->file, "                ");
  } else {
    switch (op->reg.ty) {
    case IR_REG_TY_NONE:
      fprintf(fm->file, "    (UNASSIGNED)");
      break;
    case IR_REG_TY_SPILLED:
      if (op->lcl) {
        fprintf(fm->file, "    (SPILLED), LCL=%zu", op->lcl->id);
      } else {
        fprintf(fm->file, "    (SPILLED), LCL=(UNASSIGNED)");
      }
      break;
    case IR_REG_TY_FLAGS:
      fprintf(fm->file, "    (FLAGS)");
      break;
    case IR_REG_TY_INTEGRAL:
      fprintf(fm->file, "    register=R%zu", op->reg.idx);
      break;
    case IR_REG_TY_FP:
      fprintf(fm->file, "    register=F%zu", op->reg.idx);
      break;
    }
  }

  if (op->write_info.num_reg_writes) {
    fprintf(fm->file, "        |    writes=");

    for (size_t i = 0; i < op->write_info.num_reg_writes; i++) {
      struct ir_reg *write = &op->write_info.writes[i];

      switch (write->ty) {
      case IR_REG_TY_INTEGRAL:
        fprintf(fm->file, "R%zu", write->idx);
        break;
      case IR_REG_TY_FP:
        fprintf(fm->file, "F%zu", write->idx);
        break;
      default:
        BUG("doesn't make sense as reg write");
      }

      if (i + 1 != op->write_info.num_reg_writes) {
        fprintf(fm->file, ",");
      }
    }
  }

  fprintf(fm->file, "\n");
}

const struct prettyprint_callbacks FILE_WRITER_CALLBACKS = {
    .begin_visit_basicblock = prettyprint_begin_visit_basicblock_file,
    .end_visit_basicblock = prettyprint_end_visit_basicblock_file,

    .begin_visit_stmt = prettyprint_begin_visit_stmt_file,
    .end_visit_stmt = NULL,

    .visit_op = prettyprint_visit_op_file,
};

void debug_visit_stmt(struct ir_func *irb, struct ir_stmt *stmt,
                      const struct debug_print_ir_opts *opts,
                      const struct prettyprint_callbacks *callbacks,
                      void *metadata) {
  if (callbacks->begin_visit_stmt) {
    callbacks->begin_visit_stmt(irb, stmt, opts, metadata);
  }

  struct ir_op *op = stmt->first;
  while (op) {
    if (callbacks->visit_op) {
      callbacks->visit_op(op, opts, metadata);
    }

    op = op->succ;
  }

  if (callbacks->end_visit_stmt) {
    callbacks->end_visit_stmt(irb, stmt, opts, metadata);
  }
}

void debug_visit_basicblock(struct ir_func *irb,
                            struct ir_basicblock *basicblock,
                            const struct debug_print_ir_opts *opts,
                            const struct prettyprint_callbacks *callbacks,
                            void *metadata) {
  if (callbacks->begin_visit_basicblock) {
    callbacks->begin_visit_basicblock(irb, basicblock, opts, metadata);
  }

  struct ir_stmt *stmt = basicblock->first;
  while (stmt) {
    debug_visit_stmt(irb, stmt, opts, callbacks, metadata);

    stmt = stmt->succ;
  }

  if (callbacks->end_visit_basicblock) {
    callbacks->end_visit_basicblock(irb, basicblock, opts, metadata);
  }
}

void debug_visit_ir(struct ir_func *irb, const struct debug_print_ir_opts *opts,
                    const struct prettyprint_callbacks *callbacks,
                    void *metadata) {
  struct ir_basicblock *basicblock = irb->first;

  while (basicblock) {
    debug_visit_basicblock(irb, basicblock, opts, callbacks, metadata);

    basicblock = basicblock->succ;
  }
}

void debug_print_basicblock(FILE *file, struct ir_func *irb,
                            struct ir_basicblock *basicblock,
                            const struct debug_print_ir_opts *opts) {
  int ctr_pad = irb ? (int)num_digits(irb->op_count) : 20;
  size_t ctr = 0;

  struct prettyprint_file_metadata metadata = {
      .file = file, .ctr_pad = ctr_pad, .ctr = ctr};

  debug_visit_basicblock(irb, basicblock, opts, &FILE_WRITER_CALLBACKS,
                         &metadata);
}

void debug_print_stmt(FILE *file, struct ir_func *irb, struct ir_stmt *stmt,
                      const struct debug_print_ir_opts *opts) {

  int ctr_pad = (int)num_digits(irb->op_count);
  size_t ctr = 0;

  struct prettyprint_file_metadata metadata = {
      .file = file, .ctr_pad = ctr_pad, .ctr = ctr};

  debug_visit_stmt(irb, stmt, opts, &FILE_WRITER_CALLBACKS, &metadata);
}

void debug_print_ir_func(FILE *file, struct ir_func *irb,
                         const struct debug_print_ir_opts *opts) {
  int ctr_pad = (int)num_digits(irb->op_count);
  size_t ctr = 0;

  struct prettyprint_file_metadata metadata = {
      .file = file, .ctr_pad = ctr_pad, .ctr = ctr};

  fprintf(file, "FUNCTION: %s", irb->name);
  debug_print_func_ty_string(file, &irb->func_ty);

  if (opts->demangle_sym) {
    const char *demangled = opts->demangle_sym(irb->arena, irb->name);
    fprintf(file, "\n    (demangled: %s)", demangled);
  }

  fprintf(file, "\n\n");
  fprintf(file, "    num_locals: %zu\n", irb->lcl_count);
  fprintf(file, "    total_locals_size: %zu\n", irb->total_locals_size);
  fprintf(file, "    caller_stack_needed: %zu", irb->caller_stack_needed);

  if (irb->reg_usage.num_nonvolatile_used) {
    fprintf(file, "\n    nonvolatile_used: ");

    fprintf(file, "(");

    for (size_t i = 0; i < irb->reg_usage.num_nonvolatile_used; i++) {
      debug_print_ir_reg(file, irb->reg_usage.nonvolatile_used[i]);

      if (i + 1 != irb->reg_usage.num_nonvolatile_used) {
        fprintf(file, ", ");
      }
    }

    fprintf(file, ")");
  }

  if (irb->lcl_count) {
    fprintf(file, "\n\n");
    fprintf(file, "LOCALS: {\n");
    struct ir_lcl *lcl = irb->first_lcl;
    while (lcl) {
      debug_print_lcl(file, lcl, opts);

      lcl = lcl->succ;
    }
    fprintf(file, "}");
  }

  debug_visit_ir(irb, opts, &FILE_WRITER_CALLBACKS, &metadata);
}

static void debug_print_ir_var_value(FILE *file, struct ir_unit *iru,
                                     struct ir_var_value *var_value, bool top,
                                     size_t offset) {
  switch (var_value->ty) {
  case IR_VAR_VALUE_TY_STR: {
    struct ir_var_ty var_ty = var_value->var_ty;
    DEBUG_ASSERT(var_ty.ty == IR_VAR_TY_TY_ARRAY,
                 "expected IR_VAR_VALUE_TY_STR to be an array");

    struct ir_var_ty ch_ty = *var_ty.array.underlying;

    if (ch_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
        ch_ty.primitive == IR_VAR_PRIMITIVE_TY_I8) {
      fprint_str(file, var_value->str_value.value, var_value->str_value.len);
    } else if (ch_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
               ch_ty.primitive == IR_VAR_PRIMITIVE_TY_I32) {
      // it has now been given its real size (in chars), so pass byte length
      // to wstr
      // FIXME: we should have `wstr` take # of chars and parse/typechk should
      // hold the right number of chars, not byte length
      fprint_wstr(file, var_value->str_value.value,
                  var_value->str_value.len * 4);
    } else {
      BUG("don't know how to print str (expected I8 or I32 array)");
    }
    break;
  }
  case IR_VAR_VALUE_TY_ZERO:
    fprintf(file, "{ ZERO }");
    break;
  case IR_VAR_VALUE_TY_INT:
    fprintf(file, "%llu", var_value->int_value);
    break;
  case IR_VAR_VALUE_TY_FLT:
    fprintf(file, "%Lf", var_value->flt_value);
    break;
  case IR_VAR_VALUE_TY_ADDR:
    fprintf(file, "addr GLB(%zu)", var_value->addr.glb->id);
    if (var_value->addr.offset) {
      fprintf(file, " + %llu", var_value->addr.offset);
    }
    break;
  case IR_VAR_VALUE_TY_VALUE_LIST:
    if (top) {
      fprintf(file, "{\n");
    }

    for (size_t i = 0; i < var_value->value_list.num_values; i++) {
      if (top || i) {
        fprintf(file, "  ");
      }
      struct ir_var_value *sub_value = &var_value->value_list.values[i];

      size_t list_offset = offset + var_value->value_list.offsets[i];
      debug_print_ir_var_value(file, iru, sub_value, false, list_offset);

      if (sub_value->ty != IR_VAR_VALUE_TY_VALUE_LIST) {
        fprintf(file, "; OFFSET=%zu", list_offset);
        fprintf(file, ", ");
        debug_print_var_ty_string(file, &sub_value->var_ty);
        fprintf(file, "\n");
      }
    }
    if (top) {
      fprintf(file, "}");
    }
    break;
  }
}

void debug_print_ir_var(FILE *file, struct ir_var *var) {
  switch (var->ty) {
  case IR_VAR_TY_STRING_LITERAL:
    fprintf(file, "[STRING LITERAL] ");
    break;
  case IR_VAR_TY_CONST_DATA:
    fprintf(file, "[CONST DATA] ");
    break;
  case IR_VAR_TY_DATA:
    fprintf(file, "[DATA] ");
    break;
  }

  DEBUG_ASSERT(var->var_ty.ty != IR_VAR_TY_TY_NONE, "GLB with no type");

  debug_print_var_ty_string(file, &var->var_ty);
  fprintf(file, " = ");
  debug_print_ir_var_value(file, var->unit, &var->value, true, 0);
  fprintf(file, "\n\n");
}

struct print_ir_graph_metadata {
  FILE *file;
};

static void visit_op_for_graph(struct ir_op *op,
                               const struct debug_print_ir_opts *opts,
                               void *metadata) {
  struct print_ir_graph_metadata *gm = metadata;

  debug_print_op_with_ctx(gm->file, op, opts, PRINT_OP_CTX_TOP_LEVEL);

  // `\l` prints left-justified
  fprintf(gm->file, "\\l");
}

static struct graph_vertex *
get_basicblock_vertex(struct ir_func *irb, struct ir_basicblock *basicblock,
                      const struct debug_print_ir_opts *opts,
                      struct graphwriter *gwr) {
  if (!basicblock->metadata) {
    size_t digits = num_digits(irb->basicblock_count);
    digits = MAX(digits, 2);

    basicblock->metadata = vertex_from_integral(gwr, basicblock->id);

    struct graph_vertex_attr vertex_attr_font;
    vertex_attr_font.ty = GRAPH_VERTEX_ATTR_TY_FONT;
    vertex_attr_font.font.font = "Courier";
    vertex_attr(basicblock->metadata, &vertex_attr_font);

    struct graph_vertex_attr vertex_attr_rect;
    vertex_attr_rect.ty = GRAPH_VERTEX_ATTR_TY_SHAPE;
    vertex_attr_rect.shape = GRAPH_SHAPE_RECT;
    vertex_attr(basicblock->metadata, &vertex_attr_rect);

    FILE *file =
        begin_vertex_attr(basicblock->metadata, GRAPH_VERTEX_ATTR_TY_LABEL);

    struct prettyprint_callbacks callbacks = {
        .visit_op = visit_op_for_graph,
    };

    struct print_ir_graph_metadata metadata = {
        .file = file,
    };

    fprintf(metadata.file, "BB @ %03zu\n\n", basicblock->id);
    debug_visit_basicblock(irb, basicblock, opts, &callbacks, &metadata);

    end_vertex_attr(basicblock->metadata);
  }

  return basicblock->metadata;
}

static void debug_print_ir_func_graph(FILE *file, struct ir_func *irb,
                                      const struct debug_print_ir_opts *opts) {
  ir_clear_metadata(irb);

  struct graphwriter *gwr = graphwriter_create(irb->arena, GRAPH_TY_DIRECTED,
                                               GRAPH_STRICTNESS_STRICT, file);

  struct ir_basicblock *basicblock = irb->first;
  while (basicblock) {
    struct graph_vertex *basicblock_vertex =
        get_basicblock_vertex(irb, basicblock, opts, gwr);

    for (size_t i = 0; i < basicblock->num_preds; i++) {
      struct ir_basicblock *pred = basicblock->preds[i];
      struct graph_vertex *pred_vertex =
          get_basicblock_vertex(irb, pred, opts, gwr);

      edge(gwr, pred_vertex, basicblock_vertex);
    }

    basicblock = basicblock->succ;
  }

  graphwriter_free(&gwr);
}

void debug_print_ir_graph(FILE *file, struct ir_unit *iru) {
  struct ir_glb *glb = iru->first_global;
  while (glb) {
    if (glb->def_ty == IR_GLB_DEF_TY_DEFINED && glb->ty == IR_GLB_TY_FUNC) {
      // TODO: support opts in graphwriter
      debug_print_ir_func_graph(file, glb->func, DEBUG_PRINT_IR_OPTS_DEFAULT);
    }

    glb = glb->succ;
  }
}

void debug_print_lcl(FILE *file, struct ir_lcl *lcl,
                     UNUSED const struct debug_print_ir_opts *opts) {
  fprintf(file, "  ");

  switch (lcl->alloc_ty) {
  case IR_LCL_ALLOC_TY_NONE:
    fprintf(file, "[%zu] : ", lcl->id);
    break;
  case IR_LCL_ALLOC_TY_NORMAL:
    fprintf(file, "[%zu, ", lcl->id);
    debug_print_lcl_alloc(file, &lcl->alloc);
    fprintf(file, "] : ");
    break;
  case IR_LCL_ALLOC_TY_FIXED:
    fprintf(file, "[%zu, FIXED ", lcl->id);
    debug_print_lcl_alloc(file, &lcl->alloc);
    fprintf(file, "] : ");
    break;
  }

  debug_print_var_ty_string(file, &lcl->var_ty);

  if (lcl->flags) {
    fprintf(file, " [ ");

    bool first = true;
#define PRINT_FLAG(name, str)                                                  \
  if (lcl->flags & IR_LCL_FLAG_##name) {                                       \
    fprintf(file, first ? "." str : ", ." str);                                \
    first = false;                                                             \
  }

    PRINT_FLAG(SPILL, "spill");
    PRINT_FLAG(ZEROED, "zeroed");
    PRINT_FLAG(PARAM, "param");
    PRINT_FLAG(PROMOTED, "promoted");
    PRINT_FLAG(CALL_SAVE, "call_save");

#undef PRINT_FLAG

    fprintf(file, " ]");
  }

  fprintf(file, ",\n");
}

void debug_print_glb(FILE *file, struct ir_glb *glb,
                     const struct debug_print_ir_opts *opts) {
  fprintf(file, "GLB(%zu) [", glb->id);

  switch (glb->linkage) {
  case IR_LINKAGE_NONE:
    fprintf(file, "NO LINKAGE");
    break;
  case IR_LINKAGE_INTERNAL:
    fprintf(file, "INTERNAL LINKAGE");
    break;
  case IR_LINKAGE_EXTERNAL:
    fprintf(file, "EXTERNAL LINKAGE");
    break;
  }

  fprintf(file, "]");

  if (glb->flags) {
    fprintf(file, " [ ");

    bool first = true;
#define PRINT_FLAG(name, str)                                                  \
  if (glb->flags & IR_GLB_FLAG_##name) {                                       \
    fprintf(file, first ? "." str : ", ." str);                                \
    first = false;                                                             \
  }

    PRINT_FLAG(WEAK, "weak");

#undef PRINT_FLAG

    fprintf(file, " ] ");
  }

  fprintf(file, "\n");

  if (glb->def_ty == IR_GLB_DEF_TY_DEFINED) {
    switch (glb->ty) {
      // TODO: should either have name in `var` or remove it from `func` for
      // consistency
    case IR_GLB_TY_DATA:
      debug_print_glb_name(file, glb->unit->arena, glb, opts);
      debug_print_ir_var(file, glb->var);
      break;
    case IR_GLB_TY_FUNC:
      debug_print_ir_func(file, glb->func, opts);
      break;
    }
  } else {
    if (glb->name) {
      debug_print_glb_name(file, glb->unit->arena, glb, opts);
      debug_print_var_ty_string(file, &glb->var_ty);
    }
  }
  fprintf(file, "\n\n");
}

void debug_print_ir(FILE *file, struct ir_unit *iru,
                    const struct debug_print_ir_opts *opts) {
  struct ir_glb *glb = iru->first_global;
  while (glb) {
    debug_print_glb(file, glb, opts);

    glb = glb->succ;
  }
}

void debug_print_ir_object(FILE *file, const struct ir_object *object,
                           const struct debug_print_ir_opts *opts) {
  switch (object->ty) {
  case IR_OBJECT_TY_GLB:
    debug_print_glb(file, object->glb, opts);
    break;
  case IR_OBJECT_TY_LCL:
    fprintf(file, "In func %s: \n", object->lcl->func->name);
    debug_print_lcl(file, object->lcl, opts);
    break;
  case IR_OBJECT_TY_FUNC:
    debug_print_ir_func(file, object->func, opts);
    break;
  case IR_OBJECT_TY_VAR:
    debug_print_ir_var(file, object->var);
    break;
  case IR_OBJECT_TY_BASICBLOCK: {
    struct ir_func *func = object->basicblock->func;
    if (func) {
      fprintf(file, "In func %s: \n", func->name);
    } else {
      fprintf(file, "No func, likely detached\n");
    }
    debug_print_basicblock(file, func, object->basicblock, opts);
    break;
  }
  case IR_OBJECT_TY_STMT: {
    struct ir_basicblock *basicblock = object->stmt->basicblock;
    struct ir_func *func;
    if (basicblock) {
      func = basicblock->func;
      fprintf(file, "In func %s, basicblock @ %zu: \n", basicblock->func->name,
              basicblock->id);
    } else {
      func = NULL;
      fprintf(file, "No basicblock, likely detached\n");
    }

    debug_print_stmt(file, func, object->stmt, opts);
    break;
  }
  case IR_OBJECT_TY_OP: {
    struct ir_stmt *stmt = object->op->stmt;
    if (stmt) {
      fprintf(file, "In func %s, basicblock @ %zu, stmt $ %zu: \n",
              stmt->basicblock->func->name, stmt->basicblock->id, stmt->id);
    } else {
      fprintf(file, "No stmt, likely detached\n");
    }

    debug_print_ir_func(file, stmt->basicblock->func, opts);
    debug_print_op(file, object->op, opts);
    break;
  }
  }
}
