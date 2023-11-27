#include "prettyprint.h"
#include <math.h>

const char *binary_op_string(enum ir_op_binary_op_ty ty) {
  switch (ty) {
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
  default:
    return "?unknown?";
  }
}

const char *var_ty_string(const struct ir_op_var_ty *var_ty) {
  switch (var_ty->ty) {
  case IR_OP_VAR_TY_TY_NONE: {
    return "<none>";
  }
  case IR_OP_VAR_TY_TY_PRIMITIVE: {
    switch (var_ty->primitive) {
    case IR_OP_VAR_PRIMITIVE_TY_I8:
      return "i8";
    case IR_OP_VAR_PRIMITIVE_TY_I16:
      return "i16";
    case IR_OP_VAR_PRIMITIVE_TY_I32:
      return "i32";
    case IR_OP_VAR_PRIMITIVE_TY_I64:
      return "i64";
    }
  }
  }
}

const char *phi_string(struct ir_op_phi *phi) {
  // just assume we don't have more than 100,000 phi inputs
  char *buff = nonnull_malloc(phi->num_values * (6 + 2));
  char *head = buff;

  for (size_t i = 0; i < phi->num_values; i++) {
    head += sprintf(head, "%%%zu", phi->values[i]->id);

    if (i + 1 < phi->num_values) {
      head += sprintf(head, ", ");
    }
  }

  *head = '\0';
  return buff;
}

void debug_print_op(struct ir_op *ir) {
  FILE *file = stderr;

  switch (ir->ty) {
  case IR_OP_TY_PHI:
    fslogsl(file, "%%%zu (%s) = phi [ %s ]", ir->id, var_ty_string(&ir->var_ty),
            phi_string(&ir->phi));
    break;
  case IR_OP_TY_MOV:
    fslogsl(file, "%%%zu (%s) = %%%zu", ir->id, var_ty_string(&ir->var_ty),
            ir->mov.value->id);
    break;
  case IR_OP_TY_CNST:
    fslogsl(file, "%%%zu (%s) = %zu", ir->id, var_ty_string(&ir->var_ty),
            ir->cnst.value);
    break;
  case IR_OP_TY_BINARY_OP:
    fslogsl(file, "%%%zu (%s) = %%%zu %s %%%zu", ir->id,
            var_ty_string(&ir->var_ty), ir->binary_op.lhs->id,
            binary_op_string(ir->binary_op.ty), ir->binary_op.rhs->id);
    break;
  case IR_OP_TY_STORE_LCL:
    fslogsl(file, "%%%zu (%s) = storelcl LCL(%zu), %%%zu", ir->id,
            var_ty_string(&ir->var_ty), ir->store_lcl.lcl_idx,
            ir->store_lcl.value->id);
    break;
  case IR_OP_TY_LOAD_LCL:
    fslogsl(file, "%%%zu = loadlcl (%s) LCL(%zu)", ir->id,
            var_ty_string(&ir->var_ty), ir->load_lcl.lcl_idx);
    break;
  case IR_OP_TY_BR:
    invariant_assert(ir->stmt->basicblock->ty == IR_BASICBLOCK_TY_MERGE,
                     "found `br` but bb wasn't MERGE");
    fslogsl(file, "br @%zu", ir->stmt->basicblock->merge.target->id);
    break;
  case IR_OP_TY_BR_COND:
    invariant_assert(ir->stmt->basicblock->ty == IR_BASICBLOCK_TY_SPLIT,
                     "found `br.cond` but bb wasn't SPLIT");
    fslogsl(file, "br.cond %%%zu, TRUE(@%zu), FALSE(@%zu)",
            ir->br_cond.cond->id, ir->stmt->basicblock->split.true_target->id,
            ir->stmt->basicblock->split.false_target->id);
    break;
  case IR_OP_TY_RET:
    fslogsl(file, "return %%%zu", ir->ret.value->id);
    break;
  }
}

void debug_print_ir(struct ir_builder *irb, struct ir_basicblock *basicblock,
                    debug_print_op_callback *cb, void *cb_metadata) {
  debug("%zu statements", irb->stmt_count);

  int ctr_pad = (int)log10(irb->op_count) + 1;
  size_t ctr = 0;

  FILE *file = stderr;

  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;
    fslog(file, "\nBB @ %03zu", basicblock->id);

    while (stmt) {
      struct ir_op *ir = stmt->first;

      int op_pad = /* guess */ 50;

      while (ir) {
        fslogsl(file, "%0*zu: ", ctr_pad, ctr++);

        // HACK: this shouldn't rely on the fact `log` goes to `stderr`
        long pos = ftell(stderr);
        debug_print_op(ir);
        long width = ftell(stderr) - pos;
        long pad = op_pad - width;

        if (pad > 0) {
          fslogsl(file, "%*s", (int)pad, "");
        }

        if (cb) {
          fslogsl(file, " | ");
          cb(stderr, ir, cb_metadata);
        }
        fslogsl(file, "\n");

        ir = ir->succ;
      }

      stmt = stmt->succ;
    }
    basicblock = basicblock->succ;
  }
}
