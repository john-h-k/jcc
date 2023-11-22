#ifndef IR_H
#define IR_H

#include "parse.h"
#include "var_table.h"
#include <stdlib.h>

enum ir_op_ty {
  IR_OP_TY_PHI,

  // only used late in the pipeline for eliminating phi nodes
  IR_OP_TY_MOV,

  IR_OP_TY_CNST,

  IR_OP_TY_BINARY_OP,

  IR_OP_TY_STORE_LCL,
  IR_OP_TY_LOAD_LCL,

  IR_OP_TY_BR,
  IR_OP_TY_BR_COND,
  IR_OP_TY_RET
};

bool op_produces_value(enum ir_op_ty ty);

struct ir_op_mov {
  struct ir_op *value;
};

struct ir_op_phi {
  struct ir_op **values;
  size_t num_values;
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

enum ir_op_sign { IR_OP_SIGN_NA, IR_OP_SIGN_SIGNED, IR_OP_SIGN_UNSIGNED };

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
  /* Does not produce a value */
  IR_OP_VAR_TY_TY_NONE,

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

const struct ir_op_var_ty IR_OP_VAR_TY_NONE;

struct ir_op_store_lcl {
  unsigned long lcl_idx;
  struct ir_op *value;
};

struct ir_op_load_lcl {
  unsigned long lcl_idx;
};

struct ir_op_br_cond {
  struct ir_op *cond;
  struct ir_basicblock *if_true;
  struct ir_basicblock *if_false;
};

struct ir_op_br {
  struct ir_basicblock *target;
};

#include <limits.h>

#define NO_REG (SIZE_T_MAX)
#define NO_LCL (SIZE_T_MAX)
#define REG_SPILLED (SIZE_T_MAX - 1)

struct ir_op {
  size_t id;
  enum ir_op_ty ty;

  struct ir_op_var_ty var_ty;

  struct ir_op *pred;
  struct ir_op *succ;
  struct ir_stmt *stmt;
  union {
    struct ir_op_cnst cnst;
    struct ir_op_binary_op binary_op;
    struct ir_op_ret ret;
    struct ir_op_store_lcl store_lcl;
    struct ir_op_load_lcl load_lcl;
    struct ir_op_br_cond br_cond;
    struct ir_op_br br;
    struct ir_op_phi phi;
    struct ir_op_mov mov;
  };

  // only meaningful post register-allocation
  unsigned long reg;
  unsigned long lcl_idx;
  void *metadata;
};

// set of ops with no SEQ_POINTs
struct ir_stmt {
  size_t id;

  struct ir_basicblock *basicblock;

  struct ir_stmt *pred;
  struct ir_stmt *succ;

  // the links between ops (`pred` & `succ`) have no significance to the
  // compilation and are just for traversal. meaningful links between operations
  // are with in the op data, such as `ir_op->ret.value`, which points to the op
  // whos result is returned
  struct ir_op *first;

  // last is the dominating op of the statement, and can be used as its "root".
  // all other ops in the statement are reachable from it
  struct ir_op *last;
};

struct ir_basicblock {
  size_t id;
  
  struct ir_basicblock *pred;
  struct ir_basicblock *succ;

  struct ir_stmt *first;
  struct ir_stmt *last;

  // how many ops are before this block in the function
  size_t function_offset;
};

struct ir_builder {
  const char *name;

  struct parser *parser;
  struct arena_allocator *arena;

  // `value` contains a `struct vector *` containing the last op(s) that wrote to this
  // variable or NULL if it is not yet written to
  // it will contain multiple ops
  struct var_table var_table;

  struct ir_basicblock *first;
  struct ir_basicblock *last;

  size_t basicblock_count;
  size_t stmt_count;
  size_t op_count;

  // number of stack local variables
  size_t num_locals;
  size_t total_locals_size;
};

typedef void(walk_op_callback)(struct ir_op **op, void *metadata);

void walk_stmt(struct ir_stmt *stmt, walk_op_callback *cb, void *cb_metadata);
void walk_op(struct ir_op *op, walk_op_callback *cb, void *cb_metadata);
void walk_op_uses(struct ir_op *op, walk_op_callback *cb, void *cb_metadata);

struct ir_op *alloc_ir_op(struct ir_builder *irb, struct ir_stmt *stmt);

// Helper method that ensures the essential fields in IR op are initialised
void initialise_ir_op(struct ir_op *op, size_t id, enum ir_op_ty ty,
                      struct ir_op_var_ty var_ty, struct ir_op *pred,
                      struct ir_op *succ, struct ir_stmt *stmt,
                      unsigned long reg, unsigned long lcl_idx);

struct ir_op *insert_before_ir_op(struct ir_builder *irb,
                                  struct ir_op *insert_before, enum ir_op_ty ty,
                                  struct ir_op_var_ty var_ty);

struct ir_op *insert_after_ir_op(struct ir_builder *irb,
                                 struct ir_op *insert_after, enum ir_op_ty ty,
                                 struct ir_op_var_ty var_ty);

size_t var_ty_size(struct ir_builder *irb, struct ir_op_var_ty *ty);

struct ir_builder *
build_ir_for_function(/* needed for `associated_text */ struct parser *parser,
                      struct arena_allocator *arena, struct ast_funcdef *def);

typedef void(debug_print_op_callback)(FILE *file, struct ir_op *op,
                                      void *metadata);

void debug_print_ir(struct ir_builder *irb, struct ir_basicblock *basicblock,
                    debug_print_op_callback *cb, void *cb_metadata);

#endif
