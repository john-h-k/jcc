#ifndef IR_H
#define IR_H

#include "parse.h"
#include <stdlib.h>

enum ir_op_ty {
  IR_OP_TY_PHI,

  IR_OP_TY_CNST,

  IR_OP_TY_BINARY_OP,

  IR_OP_TY_RET
};

struct ir_op_cnst {
  size_t value;
};

struct ir_op_ret {
  struct ir_op *value;
};

enum ir_op_binary_op_ty {
  IR_OP_BINARY_OP_TY_ADD,
  IR_OP_BINARY_OP_TY_SUB,
  IR_OP_BINARY_OP_TY_MUL,
  IR_OP_BINARY_OP_TY_SDIV,
  IR_OP_BINARY_OP_TY_UDIV,
  IR_OP_BINARY_OP_TY_QUOT,
};

struct ir_op_binary_op {
  enum ir_op_binary_op_ty ty;
  struct ir_op *lhs;
  struct ir_op *rhs;
};

// IR does not have sign encoded in type (so `int` and `unsigned` are both IR_OP_VAR_TY_32)
// and instead encodes it in operations (e.g there are different IR ops for signed and unsigned division)
enum ir_op_var_ty {
  IR_OP_VAR_TY_I8,
  IR_OP_VAR_TY_I16,
  IR_OP_VAR_TY_I32,
  IR_OP_VAR_TY_I64,
};


struct ir_op {
  size_t id;
  enum ir_op_ty ty;

  enum ir_op_var_ty var_ty;
  
  struct ir_op *pred;
  struct ir_op *succ;
  union {
    struct ir_op_cnst cnst;
    struct ir_op_binary_op binary_op;
    struct ir_op_ret ret;
  };
};

struct ir_function {
  const char *name;
  struct ir_op *start;
  struct ir_op *end;
  size_t op_count;
};

struct ir_function
build_ir_for_function(/* needed for `associated_text */ struct parser *parser,
                      struct arena_allocator *arena, struct ast_funcdef *def);

void debug_print_ir(struct ir_op *ir);

#endif
