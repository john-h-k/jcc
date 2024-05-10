#include "prettyprint.h"

#include "../graphwriter.h"
#include "ir.h"

#include <math.h>

const char *unary_op_string(enum ir_op_unary_op_ty ty) {
  switch (ty) {
  case IR_OP_UNARY_OP_TY_NEG:
    return "-";
  }
}

const char *cast_op_string(enum ir_op_cast_op_ty ty) {
  switch (ty) {
  case IR_OP_CAST_OP_TY_SEXT:
    return "sext";
  case IR_OP_CAST_OP_TY_ZEXT:
    return "zext";
  case IR_OP_CAST_OP_TY_TRUNCATE:
    return "trunc";
  }
}

const char *binary_op_string(enum ir_op_binary_op_ty ty) {
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
  case IR_OP_BINARY_OP_TY_LSHIFT:
    return "<<";
  case IR_OP_BINARY_OP_TY_SRSHIFT:
    return "s>>";
  case IR_OP_BINARY_OP_TY_URSHIFT:
    return "u>>";
  case IR_OP_BINARY_OP_TY_AND:
    return "&";
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
  }
}

void debug_var_ty_string(FILE *file, const struct ir_op_var_ty *var_ty) {
  switch (var_ty->ty) {
  case IR_OP_VAR_TY_TY_NONE: {
    fprintf(file, "<none>");
    return;
  }
  case IR_OP_VAR_TY_TY_FUNC: {
    // FIXME: buffer vuln
    fprintf(file, "(");
    for (size_t i = 0; i < var_ty->func.num_params; i++) {
      debug_var_ty_string(file, &var_ty->func.params[i]);
      if (i + 1 < var_ty->func.num_params) {
        fprintf(file, ", ");
      }
    }
    fprintf(file, ")");
    fprintf(file, " -> ");
    debug_var_ty_string(file, var_ty->func.ret_ty);
    return;
  }
  case IR_OP_VAR_TY_TY_PRIMITIVE: {
    const char *name;
    switch (var_ty->primitive) {
    case IR_OP_VAR_PRIMITIVE_TY_I8:
      name = "i8";
      break;
    case IR_OP_VAR_PRIMITIVE_TY_I16:
      name = "i16";
      break;
    case IR_OP_VAR_PRIMITIVE_TY_I32:
      name = "i32";
      break;
    case IR_OP_VAR_PRIMITIVE_TY_I64:
      name = "i64";
      break;
    }

    fprintf(file, "%s", name);
    return;
  }
  }
}

void debug_phi_string(FILE *file, struct ir_op_phi *phi) {
  // just assume we don't have more than 100,000 phi inputs
  // FIXME: buffer vuln
  for (size_t i = 0; i < phi->num_values; i++) {
    fprintf(file, "%%%zu", phi->values[i]->id);

    if (i + 1 < phi->num_values) {
      fprintf(file, ", ");
    }
  }
}

void debug_call_target_string(FILE *file, struct ir_op *target) {
  if (target->ty == IR_OP_TY_GLB) {
    fprintf(file, "%s", target->glb.global);
  } else {
    fprintf(file, "%%%zu", target->id);
  }
}

void debug_call_arg_string(FILE *file, struct ir_op_call *call) {
  for (size_t i = 0; i < call->num_args; i++) {
    fprintf(file, "%%%zu", call->args[i]->id);

    if (i + 1 < call->num_args) {
      fprintf(file, ", ");
    }
  }
}

void debug_lhs(FILE *file, struct ir_op *ir) {
  fprintf(file, "%%%zu (", ir->id);
  debug_var_ty_string(file, &ir->var_ty);
  fprintf(file, ") = ");
}

void debug_print_op(FILE *file, struct ir_builder *irb, struct ir_op *ir) {
  UNUSED_ARG(irb);

  switch (ir->ty) {
  case IR_OP_TY_CUSTOM:
    debug_lhs(file, ir);
    fprintf(file, "CUSTOM");
    // TODO: print customs
    break;
  case IR_OP_TY_GLB:
    debug_lhs(file, ir);
    fprintf(file, "GLOBAL ( %s )", ir->glb.global);
    // don't print anything for global - let the op consuming it print instead
    break;
  case IR_OP_TY_CALL: {
    debug_lhs(file, ir);
    fprintf(file, "call ");

    debug_call_target_string(file, ir->call.target);
    fprintf(file, " ( ");
    debug_call_arg_string(file, &ir->call);
    fprintf(file, " )");
    break;
  }
  case IR_OP_TY_PHI:
    debug_lhs(file, ir);
    fprintf(file, "phi [ ");
    debug_phi_string(file, &ir->phi);
    fprintf(file, " ]");
    break;
  case IR_OP_TY_MOV:
    debug_lhs(file, ir);
    if (ir->mov.value) {
      fprintf(file, "%%%zu", ir->mov.value->id);
      fprintf(file, " - (R%zu -> R%zu) ", ir->mov.value->reg, ir->reg);
    } else {
      fprintf(file, "<PARAM>");
    }
    break;
  case IR_OP_TY_CNST:
    debug_lhs(file, ir);
    fprintf(file, "%zu", ir->cnst.value);
    break;
  case IR_OP_TY_BINARY_OP:
    debug_lhs(file, ir);

    if (ir->binary_op.lhs && ir->binary_op.rhs) {
      fprintf(file, "%%%zu %s %%%zu", ir->binary_op.lhs->id,
              binary_op_string(ir->binary_op.ty), ir->binary_op.rhs->id);
    } else {
      fprintf(file, "<NULL binop>"); // FIXME: properly print
    }
    break;
  case IR_OP_TY_UNARY_OP:
    debug_lhs(file, ir);
    fprintf(file, "%s %%%zu", unary_op_string(ir->unary_op.ty),
            ir->unary_op.value->id);
    break;
  case IR_OP_TY_CAST_OP:
    debug_lhs(file, ir);
    fprintf(file, "%s %%%zu", cast_op_string(ir->cast_op.ty),
            ir->cast_op.value->id);
    break;
  case IR_OP_TY_STORE_LCL:
    debug_lhs(file, ir);
    fprintf(file, "storelcl LCL(%zu), %%%zu", ir->store_lcl.lcl_idx,
            ir->store_lcl.value->id);
    break;
  case IR_OP_TY_LOAD_LCL:
    debug_lhs(file, ir);
    fprintf(file, "loadlcl LCL(%zu)", ir->load_lcl.lcl_idx);
    break;
  case IR_OP_TY_BR:
    // this can happen post lowering!
    // invariant_assert(ir->stmt->basicblock->ty == IR_BASICBLOCK_TY_MERGE,
    //                  "found `br` but bb wasn't MERGE");
    fprintf(file, "br @%zu", ir->stmt->basicblock->merge.target->id);
    break;
  case IR_OP_TY_BR_COND:
    invariant_assert(ir->stmt->basicblock->ty == IR_BASICBLOCK_TY_SPLIT,
                     "found `br.cond` but bb wasn't SPLIT");
    fprintf(file, "br.cond %%%zu, TRUE(@%zu), FALSE(@%zu)",
            ir->br_cond.cond->id,
            ir->stmt->basicblock->split.true_target->id,
            ir->stmt->basicblock->split.false_target->id);
    break;
  case IR_OP_TY_RET:
    if (ir->ret.value) {
      fprintf(file, "return %%%zu", ir->ret.value->id);
    } else {
      fprintf(file, "return");
    }
    break;
  }
}

const struct prettyprint_callbacks GRAPH_WRITER_CALLBACKS;

struct prettyprint_file_metadata {
  FILE *file;
  int ctr_pad;
  size_t ctr;
  debug_print_op_callback *cb;
  void *cb_metadata;
};

void prettyprint_begin_visit_basicblock_file(struct ir_builder *irb,
                                             struct ir_basicblock *basicblock,
                                             void *metadata) {
  UNUSED_ARG(irb);

  struct prettyprint_file_metadata *fm = metadata;

  fslog(fm->file, "\nBB @ %03zu", basicblock->id);
}

void prettyprint_end_visit_basicblock_file(struct ir_builder *irb,
                                           struct ir_basicblock *basicblock,
                                           void *metadata) {
  UNUSED_ARG(irb);
  UNUSED_ARG(basicblock);

  struct prettyprint_file_metadata *fm = metadata;

  fslog(fm->file, "");
}

void prettyprint_visit_op_file(struct ir_builder *irb, struct ir_op *op,
                               void *metadata) {
  // if (op->ty == IR_OP_TY_GLB) {
  //   // TODO: stop this function needing to deal with GLB its a messy opcode
  //   return;
  // }

  int op_pad = /* guess */ 50;

  struct prettyprint_file_metadata *fm = metadata;

  fprintf(fm->file, "%0*zu: ", fm->ctr_pad, fm->ctr++);

  long pos = ftell(fm->file);
  debug_print_op(fm->file, irb, op);

  if (ftell(fm->file) == pos) {
    // no line was written
    return;
  }

  long width = ftell(fm->file) - pos;
  long pad = op_pad - width;

  if (pad > 0) {
    fprintf(fm->file, "%*s", (int)pad, "");
  }

  if (fm->cb) {
    fprintf(fm->file, " | ");
    fm->cb(fm->file, op, fm->cb_metadata);
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

void debug_visit_stmt(struct ir_builder *irb, struct ir_stmt *stmt,
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

void debug_visit_basicblock(struct ir_builder *irb,
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

void debug_visit_ir(struct ir_builder *irb,
                    const struct prettyprint_callbacks *callbacks,
                    void *metadata) {
  struct ir_basicblock *basicblock = irb->first;

  while (basicblock) {
    debug_visit_basicblock(irb, basicblock, callbacks, metadata);

    basicblock = basicblock->succ;
  }
}

void debug_print_basicblock(FILE *file, struct ir_builder *irb,
                            struct ir_basicblock *basicblock,
                            debug_print_op_callback *cb, void *cb_metadata) {
  int ctr_pad = (int)log10(irb->op_count) + 1;
  size_t ctr = 0;

  struct prettyprint_file_metadata metadata = {.file = file,
                                               .ctr_pad = ctr_pad,
                                               .ctr = ctr,
                                               .cb = cb,
                                               .cb_metadata = cb_metadata};

  debug_visit_basicblock(irb, basicblock, &FILE_WRITER_CALLBACKS, &metadata);
}

void debug_print_ir(FILE *file, struct ir_builder *irb,
                    debug_print_op_callback *cb, void *cb_metadata) {
  int ctr_pad = (int)log10(irb->op_count) + 1;
  size_t ctr = 0;

  struct prettyprint_file_metadata metadata = {.file = file,
                                               .ctr_pad = ctr_pad,
                                               .ctr = ctr,
                                               .cb = cb,
                                               .cb_metadata = cb_metadata};

  debug_visit_ir(irb, &FILE_WRITER_CALLBACKS, &metadata);
}

struct print_ir_graph_metadata {
  FILE *file;
};

void visit_op_for_graph(struct ir_builder *irb, struct ir_op *op,
                        void *metadata) {
  struct print_ir_graph_metadata *gm = metadata;

  debug_print_op(gm->file, irb, op);

  // `\l` prints left-justified
  fprintf(gm->file, "\\l");
}

struct graph_vertex *get_basicblock_vertex(struct ir_builder *irb,
                                           struct ir_basicblock *basicblock,
                                           struct graphwriter *gwr) {
  if (!basicblock->metadata) {
    size_t digits =
        irb->basicblock_count ? (size_t)log10(irb->basicblock_count) : 0;
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

void debug_print_ir_graph(FILE *file, struct ir_builder *irb) {
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
