#include "prettyprint.h"

#include "../graphwriter.h"
#include "../log.h"
#include "../util.h"
#include "ir.h"

#include <math.h>

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
  case IR_OP_BINARY_OP_TY_SQUOT:
    return "s%";
  case IR_OP_BINARY_OP_TY_UQUOT:
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

void debug_print_var_ty_string(FILE *file, struct ir_unit *iru,
                               const struct ir_var_ty *var_ty) {
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
    debug_print_var_ty_string(file, iru, var_ty->array.underlying);
    fprintf(file, ", ");
    fprintf(file, "%zu", var_ty->array.num_elements);
    fprintf(file, " ]");
    return;
  }
  case IR_VAR_TY_TY_FUNC: {
    fprintf(file, "(");
    for (size_t i = 0; i < var_ty->func.num_params; i++) {
      debug_print_var_ty_string(file, iru, &var_ty->func.params[i]);
      if (i + 1 < var_ty->func.num_params) {
        fprintf(file, ", ");
      }
    }
    fprintf(file, ")");
    fprintf(file, " -> ");
    debug_print_var_ty_string(file, iru, var_ty->func.ret_ty);
    return;
  }
  case IR_VAR_TY_TY_STRUCT: {
    struct ir_var_ty_info info = var_ty_info(iru, var_ty);
    fprintf(file, "STRUCT (sz=%zu, align=%zu) [ ", info.size, info.alignment);
    for (size_t i = 0; i < var_ty->struct_ty.num_fields; i++) {
      debug_print_var_ty_string(file, iru, &var_ty->struct_ty.fields[i]);

      if (i + 1 < var_ty->struct_ty.num_fields) {
        fprintf(file, ", ");
      }
    }
    fprintf(file, " ]");
    break;
  }
  case IR_VAR_TY_TY_UNION: {
    struct ir_var_ty_info info = var_ty_info(iru, var_ty);
    fprintf(file, "UNION (sz=%zu, align=%zu) [ ", info.size, info.alignment);
    for (size_t i = 0; i < var_ty->union_ty.num_fields; i++) {
      debug_print_var_ty_string(file, iru, &var_ty->union_ty.fields[i]);

      if (i + 1 < var_ty->union_ty.num_fields) {
        fprintf(file, ", ");
      }
    }
    fprintf(file, " ]");
    break;
  }
  case IR_VAR_TY_TY_PRIMITIVE: {
    const char *name;
    switch (var_ty->primitive) {
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

static void debug_lhs(FILE *file, struct ir_func *irb, struct ir_op *ir) {
  fprintf(file, "%%%zu (", ir->id);
  debug_print_var_ty_string(file, irb->unit, &ir->var_ty);
  fprintf(file, ") = ");
}

enum print_op_ctx {
  PRINT_OP_CTX_TOP_LEVEL,
  PRINT_OP_CTX_USE,
};

static void debug_print_op(FILE *file, struct ir_func *irb, struct ir_op *ir,
                           enum print_op_ctx ctx);

static void debug_print_op_use(FILE *file, struct ir_func *irb,
                               struct ir_op *ir) {
  DEBUG_ASSERT(ir->stmt,
               "op used by other op but had no stmt (likely detached)");

  if (ir->flags & IR_OP_FLAG_CONTAINED) {
    debug_print_op(file, irb, ir, PRINT_OP_CTX_USE);
  } else {
    fprintf(file, "%%%zu", ir->id);
  }
}

static void debug_print_op(FILE *file, struct ir_func *irb, struct ir_op *ir,
                           enum print_op_ctx ctx) {
  DEBUG_ASSERT(ir->stmt, "op had no stmt");

  if (ctx != PRINT_OP_CTX_USE && ir->comment) {
    fprintf(file, "// %s\n", ir->comment);
  }

  if (ctx != PRINT_OP_CTX_USE) {
    debug_lhs(file, irb, ir);
  }

  switch (ir->ty) {
  case IR_OP_TY_UNKNOWN:
    BUG("unknown op!");
  case IR_OP_TY_UNDF:
    fprintf(file, "UNDF");
    break;
  case IR_OP_TY_CUSTOM:
    BUG("custom ops no longer supported");
  case IR_OP_TY_CALL: {
    fprintf(file, "call ");

    debug_print_op_use(file, irb, ir->call.target);
    fprintf(file, " ( ");
    debug_call_arg_string(file, &ir->call);
    fprintf(file, " )");
    break;
  }
  case IR_OP_TY_PHI:
    fprintf(file, "phi [ ");
    debug_phi_string(file, &ir->phi);
    fprintf(file, " ]");
    break;
  case IR_OP_TY_MOV:
    if (ir->mov.value) {
      fprintf(file, "%%%zu", ir->mov.value->id);
      fprintf(file, " : (");
      debug_print_ir_reg(file, ir->mov.value->reg);
      fprintf(file, " -> ");
      debug_print_ir_reg(file, ir->reg);
      fprintf(file, ")");
    } else {
      fprintf(file, "<PARAM>");
    }
    break;
  case IR_OP_TY_CNST:
    switch (ir->cnst.ty) {
    case IR_OP_CNST_TY_FLT:
      fprintf(file, "%Lf", ir->cnst.flt_value);
      break;
    case IR_OP_CNST_TY_INT:
      fprintf(file, "%llu", ir->cnst.int_value);
      break;
    }
    break;
  case IR_OP_TY_BINARY_OP:
    debug_print_op_use(file, irb, ir->binary_op.lhs);
    fprintf(file, " %s ", binary_op_string(ir->binary_op.ty));
    debug_print_op_use(file, irb, ir->binary_op.rhs);
    break;
  case IR_OP_TY_UNARY_OP:
    fprintf(file, "%s ", unary_op_string(ir->unary_op.ty));
    debug_print_op_use(file, irb, ir->unary_op.value);
    break;
  case IR_OP_TY_CAST_OP:
    fprintf(file, "%s ", cast_op_string(ir->cast_op.ty));
    debug_print_op_use(file, irb, ir->cast_op.value);
    break;
  case IR_OP_TY_STORE:
    switch (ir->store.ty) {
    case IR_OP_STORE_TY_LCL:
      if (ir->store.lcl) {
        fprintf(file, "store.lcl LCL(%zu), %%%zu", ir->store.lcl->id,
                ir->store.value->id);
      } else {
        fprintf(file, "store.lcl LCL(UNASSIGNED), %%%zu", ir->store.value->id);
      }
      break;
    case IR_OP_STORE_TY_GLB:
      if (ir->load.glb) {
        fprintf(file, "store.glb GLB(%zu), %%%zu", ir->store.glb->id,
                ir->store.value->id);
      } else {
        fprintf(file, "store.glb GLB(UNASSIGNED), %%%zu", ir->store.value->id);
      }
      break;
    case IR_OP_STORE_TY_ADDR:
      fprintf(file, "store.addr [");
      debug_print_op_use(file, irb, ir->store.addr);
      fprintf(file, "], ");
      debug_print_op_use(file, irb, ir->store.value);
      break;
    }

    break;
  case IR_OP_TY_LOAD:
    switch (ir->load.ty) {
    case IR_OP_LOAD_TY_LCL:
      if (ir->load.lcl) {
        fprintf(file, "load.lcl LCL(%zu)", ir->load.lcl->id);
      } else {
        fprintf(file, "load.lcl LCL(UNASSIGNED)");
      }
      break;
    case IR_OP_LOAD_TY_GLB:
      if (ir->load.glb) {
        fprintf(file, "load.glb GLB(%zu)", ir->load.glb->id);
      } else {
        fprintf(file, "load.glb GLB(UNASSIGNED)");
      }
      break;
    case IR_OP_LOAD_TY_ADDR:
      fprintf(file, "load.addr [");
      debug_print_op_use(file, irb, ir->load.addr);
      fprintf(file, "]");
      break;
    }

    break;

  case IR_OP_TY_STORE_BITFIELD: {
    struct ir_bitfield bitfield = ir->store_bitfield.bitfield;

    switch (ir->store_bitfield.ty) {
    case IR_OP_STORE_TY_LCL:
      if (ir->store_bitfield.lcl) {
        fprintf(file, "store.bitfield.lcl (#%zu, #%zu) LCL(%zu), %%%zu",
                bitfield.offset, bitfield.width, ir->store_bitfield.lcl->id,
                ir->store_bitfield.value->id);
      } else {
        fprintf(file, "store.bitfield.lcl (#%zu, #%zu) LCL(UNASSIGNED), %%%zu",
                bitfield.offset, bitfield.width, ir->store_bitfield.value->id);
      }
      break;
    case IR_OP_STORE_TY_GLB:
      if (ir->store_bitfield.glb) {
        fprintf(file, "store.bitfield.glb (#%zu, #%zu) GLB(%zu), %%%zu",
                bitfield.offset, bitfield.width, ir->store_bitfield.glb->id,
                ir->store_bitfield.value->id);
      } else {
        fprintf(file, "store.bitfield.glb (#%zu, #%zu) GLB(UNASSIGNED), %%%zu",
                bitfield.offset, bitfield.width, ir->store_bitfield.value->id);
      }
      break;
    case IR_OP_STORE_TY_ADDR:
      fprintf(file, "store.bitfield.addr (#%zu, #%zu) [", bitfield.offset,
              bitfield.width);
      debug_print_op_use(file, irb, ir->store_bitfield.addr);
      fprintf(file, "], ");
      debug_print_op_use(file, irb, ir->store_bitfield.value);
      break;
    }

    break;
  }
  case IR_OP_TY_LOAD_BITFIELD: {
    struct ir_bitfield bitfield = ir->load_bitfield.bitfield;

    switch (ir->load_bitfield.ty) {
    case IR_OP_LOAD_TY_LCL:
      if (ir->load_bitfield.glb) {
        fprintf(file, "load.bitfield.lcl (#%zu, #%zu) LCL(%zu)",
                bitfield.offset, bitfield.width, ir->load_bitfield.lcl->id);
      } else {
        fprintf(file, "load.bitfield.lcl (#%zu, #%zu) LCL(UNASSIGNED)",
                bitfield.offset, bitfield.width);
      }
      break;
    case IR_OP_LOAD_TY_GLB:
      if (ir->load_bitfield.glb) {
        fprintf(file, "load.bitfield.glb (#%zu, #%zu) GLB(%zu)",
                bitfield.offset, bitfield.width, ir->load_bitfield.glb->id);
      } else {
        fprintf(file, "load.bitfield.glb (#%zu, #%zu) GLB(UNASSIGNED)",
                bitfield.offset, bitfield.width);
      }
      break;
    case IR_OP_LOAD_TY_ADDR:
      fprintf(file, "load.bitfield.addr (#%zu, #%zu) [", bitfield.offset,
              bitfield.width);
      debug_print_op_use(file, irb, ir->load_bitfield.addr);
      fprintf(file, "]");
      break;
    }

    break;
  }
  case IR_OP_TY_ADDR_OFFSET:
    fprintf(file, "addr.off ");
    debug_print_op_use(file, irb, ir->addr_offset.base);
    fprintf(file, " + ");
    debug_print_op_use(file, irb, ir->addr_offset.index);
    if (ir->addr_offset.scale != 1) {
      fprintf(file, " * #%zu", ir->addr_offset.scale);
    }
    if (ir->addr_offset.offset) {
      fprintf(file, " + #%zu", ir->addr_offset.offset);
    }
    break;
  case IR_OP_TY_ADDR:
    switch (ir->addr.ty) {
    case IR_OP_ADDR_TY_LCL:
      if (ctx == PRINT_OP_CTX_USE) {
        fprintf(file, "LCL(%zu)", ir->addr.lcl->id);
      } else {
        fprintf(file, "addr LCL(%zu) { #%zu }", ir->addr.lcl->id,
                ir->addr.lcl->offset);
      }
      break;
    case IR_OP_ADDR_TY_GLB:
      if (ctx == PRINT_OP_CTX_USE) {
        fprintf(file, "%s", ir->addr.glb->name);
      } else {
        fprintf(file, "addr GLB(%zu)", ir->addr.glb->id);

        if (ir->addr.glb->name) {
          fprintf(file, " { \"%s\" }", ir->addr.glb->name);
        }
      }
      break;
    }
    break;
  case IR_OP_TY_BR: {
    invariant_assert(ir->stmt->basicblock->ty == IR_BASICBLOCK_TY_MERGE,
                     "found `br` but bb wasn't MERGE");
    struct ir_basicblock *bb = ir->stmt->basicblock;
    struct ir_basicblock *target = bb->ty == IR_BASICBLOCK_TY_MERGE
                                       ? bb->merge.target
                                       : bb->split.false_target;
    fprintf(file, "br @%zu", target->id);
    break;
  }
  case IR_OP_TY_BR_COND:
    invariant_assert(ir->stmt->basicblock->ty == IR_BASICBLOCK_TY_SPLIT,
                     "found `br.cond` but bb wasn't SPLIT");
    fprintf(file, "br.cond ");
    debug_print_op_use(file, irb, ir->br_cond.cond);
    fprintf(file, ", TRUE(@%zu), FALSE(@%zu)",
            ir->stmt->basicblock->split.true_target->id,
            ir->stmt->basicblock->split.false_target->id);
    break;
  case IR_OP_TY_BR_SWITCH:
    invariant_assert(ir->stmt->basicblock->ty == IR_BASICBLOCK_TY_SWITCH,
                     "found `br.switch` but bb wasn't SWITCH");
    fprintf(file, "br.switch %%%zu, [\n", ir->br_switch.value->id);

    size_t num_cases = ir->stmt->basicblock->switch_case.num_cases;
    struct ir_split_case *cases = ir->stmt->basicblock->switch_case.cases;
    for (size_t i = 0; i < num_cases; i++) {
      struct ir_split_case *split_case = &cases[i];

      fprintf(file, "    # %llu -> @%zu\n", split_case->value,
              split_case->target->id);
    }

    fprintf(file, "    DEFAULT -> @%zu\n",
            ir->stmt->basicblock->switch_case.default_target->id);
    fprintf(file, "  ]");
    break;
  case IR_OP_TY_RET:
    if (ir->ret.value) {
      fprintf(file, "return %%%zu", ir->ret.value->id);
    } else {
      fprintf(file, "return");
    }
    break;
  case IR_OP_TY_BITFIELD_EXTRACT:
    fprintf(file, "bitfield.extract #(%zu, %zu), ",
            ir->bitfield_extract.bitfield.offset,
            ir->bitfield_extract.bitfield.width);
    debug_print_op_use(file, irb, ir->bitfield_extract.value);
    break;
  case IR_OP_TY_BITFIELD_INSERT:
    fprintf(file, "bitfield.insert #(%zu, %zu), ",
            ir->bitfield_insert.bitfield.offset,
            ir->bitfield_insert.bitfield.width);
    debug_print_op_use(file, irb, ir->bitfield_insert.target);
    fprintf(file, ", ");
    debug_print_op_use(file, irb, ir->bitfield_insert.value);
    break;
  case IR_OP_TY_MEM_SET:
    fprintf(file, "mem.set ");
    debug_print_op_use(file, irb, ir->mem_set.addr);
    fprintf(file, ", #%zu, #%d", ir->mem_set.length, ir->mem_set.value);
    break;
  }
}

extern const struct prettyprint_callbacks GRAPH_WRITER_CALLBACKS;

struct prettyprint_file_metadata {
  FILE *file;
  int ctr_pad;
  size_t ctr;
  debug_print_op_callback *cb;
  void *cb_metadata;
};

static void
prettyprint_begin_visit_basicblock_file(UNUSED_ARG(struct ir_func *irb),
                                        struct ir_basicblock *basicblock,
                                        void *metadata) {
  struct prettyprint_file_metadata *fm = metadata;

  fslog(fm->file, "\n");
  if (basicblock->comment) {
    fslog(fm->file, "// %s\n", basicblock->comment);
  }
  fslogsl(fm->file, "BB @ %03zu", basicblock->id);

  if (basicblock->num_preds) {
    fslogsl(fm->file, " PREDS = [ ");
    for (size_t i = 0; i < basicblock->num_preds; i++) {
      fslogsl(fm->file, "@%zu", basicblock->preds[i]->id);

      if (i + 1 != basicblock->num_preds) {
        fslogsl(fm->file, ", ");
      }
    }
    fslogsl(fm->file, " ]");
  }

  fslogsl(fm->file, "\n");
}

static void prettyprint_end_visit_basicblock_file(
    UNUSED_ARG(struct ir_func *irb),
    UNUSED_ARG(struct ir_basicblock *basicblock), void *metadata) {
  struct prettyprint_file_metadata *fm = metadata;

  fslogsl(fm->file, "\n");
}

static void prettyprint_visit_op_file(struct ir_func *irb, struct ir_op *op,
                                      void *metadata) {
  // if (op->flags & IR_OP_FLAG_CONTAINED) {
  //   return;
  // }

  int op_pad = /* guess */ 50;

  struct prettyprint_file_metadata *fm = metadata;

  bool supports_pos = ftell(fm->file) != -1;

  fprintf(fm->file, "%0*zu: ", fm->ctr_pad, fm->ctr++);

  long pos = ftell(fm->file);
  debug_print_op(fm->file, irb, op, PRINT_OP_CTX_TOP_LEVEL);

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

  if (fm->cb) {
    fprintf(fm->file, " | ");
    fm->cb(fm->file, op, fm->cb_metadata);
  }

  if (op_produces_value(op)) {
    fprintf(fm->file, " | ");

    if (op->flags & IR_OP_FLAG_FIXED_REG) {
      fprintf(fm->file, "    (FIXED) ");
    }

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
      if (op->flags & IR_OP_FLAG_DONT_GIVE_REG) {
        fprintf(fm->file, "    (DONT)");
      } else {
        fprintf(fm->file, "    register=R%zu", op->reg.idx);
      }
      break;
    case IR_REG_TY_FP:
      if (op->flags & IR_OP_FLAG_DONT_GIVE_REG) {
        fprintf(fm->file, "    (DONT)");
      } else {
        fprintf(fm->file, "    register=F%zu", op->reg.idx);
      }
      break;
    }
  }

  fprintf(fm->file, "\n");
}

const struct prettyprint_callbacks FILE_WRITER_CALLBACKS = {
    .begin_visit_basicblock = prettyprint_begin_visit_basicblock_file,
    .end_visit_basicblock = prettyprint_end_visit_basicblock_file,

    .begin_visit_stmt = NULL,
    .end_visit_stmt = NULL,

    .visit_op = prettyprint_visit_op_file,
};

void debug_visit_stmt(struct ir_func *irb, struct ir_stmt *stmt,
                      const struct prettyprint_callbacks *callbacks,
                      void *metadata) {
  if (callbacks->begin_visit_stmt) {
    callbacks->begin_visit_stmt(irb, stmt, metadata);
  }

  struct ir_op *op = stmt->first;
  while (op) {
    if (callbacks->visit_op) {
      callbacks->visit_op(irb, op, metadata);
    }

    op = op->succ;
  }

  if (callbacks->end_visit_stmt) {
    callbacks->end_visit_stmt(irb, stmt, metadata);
  }
}

void debug_visit_basicblock(struct ir_func *irb,
                            struct ir_basicblock *basicblock,
                            const struct prettyprint_callbacks *callbacks,
                            void *metadata) {
  if (callbacks->begin_visit_basicblock) {
    callbacks->begin_visit_basicblock(irb, basicblock, metadata);
  }

  struct ir_stmt *stmt = basicblock->first;
  while (stmt) {
    debug_visit_stmt(irb, stmt, callbacks, metadata);

    stmt = stmt->succ;
  }

  if (callbacks->end_visit_basicblock) {
    callbacks->end_visit_basicblock(irb, basicblock, metadata);
  }
}

void debug_visit_ir(struct ir_func *irb,
                    const struct prettyprint_callbacks *callbacks,
                    void *metadata) {
  struct ir_basicblock *basicblock = irb->first;

  while (basicblock) {
    debug_visit_basicblock(irb, basicblock, callbacks, metadata);

    basicblock = basicblock->succ;
  }
}

void debug_print_basicblock(FILE *file, struct ir_func *irb,
                            struct ir_basicblock *basicblock,
                            debug_print_op_callback *cb, void *cb_metadata) {
  int ctr_pad = (int)num_digits(irb->op_count);
  size_t ctr = 0;

  struct prettyprint_file_metadata metadata = {.file = file,
                                               .ctr_pad = ctr_pad,
                                               .ctr = ctr,
                                               .cb = cb,
                                               .cb_metadata = cb_metadata};

  debug_visit_basicblock(irb, basicblock, &FILE_WRITER_CALLBACKS, &metadata);
}

void debug_print_stmt(FILE *file, struct ir_func *irb, struct ir_stmt *stmt,
                      debug_print_op_callback *cb, void *cb_metadata) {

  int ctr_pad = (int)num_digits(irb->op_count);
  size_t ctr = 0;

  struct prettyprint_file_metadata metadata = {.file = file,
                                               .ctr_pad = ctr_pad,
                                               .ctr = ctr,
                                               .cb = cb,
                                               .cb_metadata = cb_metadata};

  debug_visit_ir(stmt->basicblock->irb, &FILE_WRITER_CALLBACKS, &metadata);
}

void debug_print_ir_func(FILE *file, struct ir_func *irb,
                         debug_print_op_callback *cb, void *cb_metadata) {
  int ctr_pad = (int)num_digits(irb->op_count);
  size_t ctr = 0;

  struct prettyprint_file_metadata metadata = {.file = file,
                                               .ctr_pad = ctr_pad,
                                               .ctr = ctr,
                                               .cb = cb,
                                               .cb_metadata = cb_metadata};

  fprintf(file, "FUNCTION: %s\n", irb->name);
  fprintf(file, "    num_locals: %zu\n", irb->num_locals);
  fprintf(file, "    total_locals_size: %zu", irb->total_locals_size);

  if (irb->num_locals) {
    fprintf(file, "\n\n");
    fprintf(file, "LOCALS: {\n");
    struct ir_lcl *lcl = irb->first_local;
    while (lcl) {
      fprintf(file, "  ");

      fprintf(file, "[%zu, #%zu] : ", lcl->id, lcl->offset);
      debug_print_var_ty_string(file, irb->unit, &lcl->var_ty);

      if (lcl->flags & IR_LCL_FLAG_SPILL) {
        fprintf(file, "    (SPILL),\n");
      } else {
        fprintf(file, ",\n");
      }

      lcl = lcl->succ;
    }
    fprintf(file, "}");
  }

  debug_visit_ir(irb, &FILE_WRITER_CALLBACKS, &metadata);
}

static void debug_print_ir_var_value(FILE *file, struct ir_var_value *var_value,
                                     bool top) {
  switch (var_value->ty) {
  case IR_VAR_VALUE_TY_STR:
    fprint_str(file, var_value->str_value);
    break;
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

      debug_print_ir_var_value(file, sub_value, false);

      if (sub_value->ty != IR_VAR_VALUE_TY_VALUE_LIST) {
        fprintf(file, ",  OFFSET=%zu\n", var_value->value_list.offsets[i]);
      }
    }
    if (top) {
      fprintf(file, "}");
    }
    break;
  }
}

static void debug_print_ir_var(FILE *file, struct ir_unit *iru,
                               struct ir_var *var) {
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

  debug_print_var_ty_string(file, iru, &var->var_ty);
  fprintf(file, " = ");
  debug_print_ir_var_value(file, &var->value, true);
  fprintf(file, "\n\n");
}

struct print_ir_graph_metadata {
  FILE *file;
};

static void visit_op_for_graph(struct ir_func *irb, struct ir_op *op,
                               void *metadata) {
  struct print_ir_graph_metadata *gm = metadata;

  debug_print_op(gm->file, irb, op, PRINT_OP_CTX_TOP_LEVEL);

  // `\l` prints left-justified
  fprintf(gm->file, "\\l");
}

static struct graph_vertex *
get_basicblock_vertex(struct ir_func *irb, struct ir_basicblock *basicblock,
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
    debug_visit_basicblock(irb, basicblock, &callbacks, &metadata);

    end_vertex_attr(basicblock->metadata);
  }

  return basicblock->metadata;
}

void debug_print_ir_graph(FILE *file, struct ir_func *irb) {
  clear_metadata(irb);

  struct graphwriter *gwr = graphwriter_create(irb->arena, GRAPH_TY_DIRECTED,
                                               GRAPH_STRICTNESS_STRICT, file);

  struct ir_basicblock *basicblock = irb->first;
  while (basicblock) {
    struct graph_vertex *basicblock_vertex =
        get_basicblock_vertex(irb, basicblock, gwr);

    for (size_t i = 0; i < basicblock->num_preds; i++) {
      struct ir_basicblock *pred = basicblock->preds[i];
      struct graph_vertex *pred_vertex = get_basicblock_vertex(irb, pred, gwr);

      edge(gwr, pred_vertex, basicblock_vertex);
    }

    basicblock = basicblock->succ;
  }

  graphwriter_free(&gwr);
}

void debug_print_ir(FILE *file, struct ir_unit *iru,
                    debug_print_op_callback *cb, void *cb_metadata) {
  struct ir_glb *glb = iru->first_global;
  while (glb) {
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

    fprintf(file, "]\n");

    if (glb->def_ty == IR_GLB_DEF_TY_DEFINED) {
      switch (glb->ty) {
      case IR_GLB_TY_DATA:
        // TODO: should either have name in `var` or remove it from `func` for
        // consistency
        fprintf(file, "%s ", glb->name);
        debug_print_ir_var(file, iru, glb->var);
        break;
      case IR_GLB_TY_FUNC:
        debug_print_ir_func(file, glb->func, cb, cb_metadata);
        break;
      }
    } else {
      if (glb->name) {
        fprintf(file, "%s\n", glb->name);
      }
    }

    glb = glb->succ;
  }
}
