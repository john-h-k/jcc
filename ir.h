#ifndef IR_H
#define IR_H

#include "parse.h"
#include <stdlib.h>
#include "var_table.h"

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
  IR_OP_BINARY_OP_TY_SQUOT,
  IR_OP_BINARY_OP_TY_UQUOT,
};

enum ir_op_sign {
  IR_OP_SIGN_NA,
  IR_OP_SIGN_SIGNED,
  IR_OP_SIGN_UNSIGNED
};

enum ir_op_sign binary_op_sign(enum ir_op_binary_op_ty);

struct ir_op_binary_op {
  enum ir_op_binary_op_ty ty;
  struct ir_op *lhs;
  struct ir_op *rhs;
};

// IR does not have sign encoded in type (so `int` and `unsigned` are both
// IR_OP_VAR_TY_32) and instead encodes it in operations (e.g there are
// different IR ops for signed and unsigned division)
enum ir_op_var_primitive_ty {
  IR_OP_VAR_PRIMITIVE_TY_I8,
  IR_OP_VAR_PRIMITIVE_TY_I16,
  IR_OP_VAR_PRIMITIVE_TY_I32,
  IR_OP_VAR_PRIMITIVE_TY_I64,
};

enum ir_op_var_ty_ty {
  /* Primitives - integers, floats, pointers */
  IR_OP_VAR_TY_TY_PRIMITIVE,

  /* Aggregate */
  // IR_OP_VAR_TY_TY_AGGREGATE,
  // IR_OP_VAR_TY_TY_ARRAY,
  // IR_OP_VAR_TY_TY_UNION,
};

struct ir_op_var_ty {
  enum ir_op_var_ty_ty ty;
  union {
    enum ir_op_var_primitive_ty primitive;
  };
};

struct ir_op {
  size_t id;
  enum ir_op_ty ty;

  struct ir_op_var_ty var_ty;

  struct ir_op *pred;
  struct ir_op *succ;
  union {
    struct ir_op_cnst cnst;
    struct ir_op_binary_op binary_op;
    struct ir_op_ret ret;
  };

  void* metadata;
};

// set of ops with no SEQ_POINTs
struct ir_stmt {
  size_t id;

  struct ir_stmt *pred;
  struct ir_stmt *succ;

  // the links between ops (`pred` & `succ`) have no significance to the compilation
  // and are just for traversal.
  // meaningful links between operations are with in the op data, such as `ir_op->ret.value`,
  // which points to the op whos result is returned
  struct ir_op *first;
  struct ir_op *last;
};

struct ir_builder {
  const char *name;

  struct parser *parser;
  struct arena_allocator *arena;

  // `value` contains a `struct ir_op *` to the last op that wrote to this
  // variable or NULL if it is not yet written to
  struct var_table var_table;

  struct ir_stmt *first;
  struct ir_stmt *last;

  size_t stmt_count;
  size_t op_count;
};

struct ir_op *alloc_ir_op(struct ir_builder *irb, struct ir_stmt *stmt);

struct ir_builder *build_ir_for_function(/* needed for `associated_text */ struct parser *parser,
                      struct arena_allocator *arena, struct ast_funcdef *def);

void debug_print_ir(struct ir_stmt *ir);

#endif
