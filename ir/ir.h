#ifndef IR_IR_H
#define IR_IR_H

#include "../parse.h"
#include "../var_table.h"

#include <stdlib.h>

enum ir_op_ty {
  IR_OP_TY_PHI,

  // only used late in the pipeline for eliminating phi nodes
  IR_OP_TY_MOV,

  IR_OP_TY_CNST,

  IR_OP_TY_BINARY_OP,
  IR_OP_TY_UNARY_OP,
  IR_OP_TY_CAST_OP,

  IR_OP_TY_STORE_LCL,
  IR_OP_TY_LOAD_LCL,

  // represents a global variable or function
  IR_OP_TY_GLB,

  IR_OP_TY_BR,
  IR_OP_TY_BR_COND,
  IR_OP_TY_RET,
  IR_OP_TY_CALL,

  // represents a custom instruction understood by a particular backend
  IR_OP_TY_CUSTOM,
};

bool op_produces_value(enum ir_op_ty ty);
bool op_is_branch(enum ir_op_ty ty);

struct ir_op_glb {
  const char *global;
};

struct ir_op_mov {
  struct ir_op *value;
};

struct ir_op_call {
  struct ir_op *target;
  size_t num_args;
  struct ir_op **args;
};

struct ir_op_phi {
  // not elegant, but phi needs ref to var so it can build itself in
  // `find_phi_exprs`
  // FIXME: ?
  struct ast_var var;

  struct ir_op **values;
  size_t num_values;
};

struct ir_op_cnst {
  size_t value;
};

struct ir_op_ret {
  struct ir_op *value;
};

enum ir_op_cast_op_ty {
  IR_OP_CAST_OP_TY_SEXT,
  IR_OP_CAST_OP_TY_ZEXT,
  IR_OP_CAST_OP_TY_TRUNCATE,
};

struct ir_op_cast_op {
  enum ir_op_cast_op_ty ty;
  struct ir_op *value;
};

enum ir_op_unary_op_ty {
  IR_OP_UNARY_OP_TY_NEG,
};

struct ir_op_unary_op {
  enum ir_op_unary_op_ty ty;
  struct ir_op *value;
};

enum ir_op_binary_op_ty {
  IR_OP_BINARY_OP_TY_EQ,
  IR_OP_BINARY_OP_TY_NEQ,

  IR_OP_BINARY_OP_TY_UGT,
  IR_OP_BINARY_OP_TY_SGT,
  IR_OP_BINARY_OP_TY_UGTEQ,
  IR_OP_BINARY_OP_TY_SGTEQ,
  IR_OP_BINARY_OP_TY_ULT,
  IR_OP_BINARY_OP_TY_SLT,
  IR_OP_BINARY_OP_TY_ULTEQ,
  IR_OP_BINARY_OP_TY_SLTEQ,

  IR_OP_BINARY_OP_TY_LSHIFT,
  IR_OP_BINARY_OP_TY_SRSHIFT,
  IR_OP_BINARY_OP_TY_URSHIFT,
  IR_OP_BINARY_OP_TY_AND,

  IR_OP_BINARY_OP_TY_ADD,
  IR_OP_BINARY_OP_TY_SUB,
  IR_OP_BINARY_OP_TY_MUL,
  IR_OP_BINARY_OP_TY_SDIV,
  IR_OP_BINARY_OP_TY_UDIV,
  IR_OP_BINARY_OP_TY_SQUOT,
  IR_OP_BINARY_OP_TY_UQUOT,
};

bool binary_op_is_comparison(enum ir_op_binary_op_ty ty);

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
  IR_OP_VAR_TY_TY_FUNC,

  /* Aggregate */
  // IR_OP_VAR_TY_TY_AGGREGATE,
  // IR_OP_VAR_TY_TY_ARRAY,
  // IR_OP_VAR_TY_TY_UNION,
};

struct ir_op_var_func_ty {
  struct ir_op_var_ty *ret_ty;
  size_t num_params;
  struct ir_op_var_ty *params;
};

struct ir_op_var_ty {
  enum ir_op_var_ty_ty ty;
  union {
    enum ir_op_var_primitive_ty primitive;
    struct ir_op_var_func_ty func;
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
  // targets on `ir_basicblock`
};

#include <limits.h>

struct aarch64_op;
struct eep_op;

struct ir_op_custom {
  union {
    struct aarch64_op *aarch64;
    struct eep_op *eep;
  };
};

#define NO_LCL (SIZE_T_MAX)
#define NO_REG (SIZE_T_MAX)
#define REG_SPILLED (NO_REG - 1)
#define REG_FLAGS (REG_SPILLED - 1)
#define DONT_GIVE_REG (REG_FLAGS - 1)

enum ir_op_flags {
  IR_OP_FLAG_NONE = 0,
  IR_OP_FLAG_MUST_SPILL = 1,
  IR_OP_FLAG_PARAM = 2,
};

struct ir_op {
  size_t id;
  enum ir_op_ty ty;

  enum ir_op_flags flags;

  struct ir_op_var_ty var_ty;

  struct ir_op *pred;
  struct ir_op *succ;
  struct ir_stmt *stmt;
  union {
    struct ir_op_glb glb;
    struct ir_op_cnst cnst;
    struct ir_op_call call;
    struct ir_op_binary_op binary_op;
    struct ir_op_unary_op unary_op;
    struct ir_op_cast_op cast_op;
    struct ir_op_ret ret;
    struct ir_op_store_lcl store_lcl;
    struct ir_op_load_lcl load_lcl;
    struct ir_op_br_cond br_cond;
    /* br has no entry, as its target is on `ir_basicblock` and it has no
     * condition */
    struct ir_op_phi phi;
    struct ir_op_mov mov;
    struct ir_op_custom custom;
  };

  // only meaningful post register-allocation
  // `live_regs` is bitmask of all registers with values live, needed for spilling
  unsigned long live_regs;
  unsigned long reg;
  unsigned long lcl_idx;
  void *metadata;
};

// set of ops with no SEQ_POINTs
struct ir_stmt {
  size_t id;

  // a NULL bb means a pruned stmt
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

// ensures the basic block ends with an appropriate branch and does not contain
// any within it
bool valid_basicblock(struct ir_basicblock *basicblock);

void get_basicblock_successors(struct ir_basicblock *basicblock,
                               struct ir_basicblock **first,
                               struct ir_basicblock **second);

enum ir_basicblock_ty {
  // a split basicblock has 2 successors and occurs when there is a conditional
  // branch
  IR_BASICBLOCK_TY_SPLIT,

  // a merge basicblock has 1 successor (but its successor will have at least 2
  // predecessors)
  // and occurs when multiple basicblocks rejoin
  IR_BASICBLOCK_TY_MERGE,

  // a return has no explicit successors
  IR_BASICBLOCK_TY_RET,
};

struct ir_basicblock_merge {
  struct ir_basicblock *target;
};

struct ir_basicblock_split {
  struct ir_basicblock *true_target;
  struct ir_basicblock *false_target;
};

struct ir_basicblock {
  size_t id;

  // `value` contains a `struct vector *` containing the last op(s) that wrote
  // to this variable or NULL if it is not yet written to
  struct var_refs *var_refs;

  // a NULL irb means a pruned basicblock
  struct ir_builder *irb;

  // these are creation order traversal methods, and do not signify edges
  // between BBs
  struct ir_basicblock *pred;
  struct ir_basicblock *succ;

  struct ir_stmt *first;
  struct ir_stmt *last;

  struct ir_basicblock **preds;
  size_t num_preds;

  enum ir_basicblock_ty ty;
  union {
    struct ir_basicblock_merge merge;
    struct ir_basicblock_split split;
  };

  // how many ops are before this block in the function
  size_t function_offset;

  void *metadata;
};

enum ir_builder_flags {
  IR_BUILDER_FLAG_NONE,
  IR_BUILDER_FLAG_MAKES_CALL
};

struct ir_builder {
  const char *name;

  struct var_refs *global_var_refs;

  struct ir_op_var_func_ty func_ty;

  struct parser *parser;
  struct arena_allocator *arena;

  struct ir_basicblock *first;
  struct ir_basicblock *last;

  enum ir_builder_flags flags;

  size_t basicblock_count;
  size_t stmt_count;
  size_t op_count;
  size_t next_id;

  // starts at index of first nonvolatile register, which is a bit odd
  // could change or generally make nice to work with
  unsigned long nonvolatile_registers_used;

  // number of stack local variables
  size_t num_locals;
  size_t total_locals_size;

  // stack spaces from this can be reused, so we store them here
  // `num_locals` is still always correct, these fields are used by `lower.c`
  // to determine whether new locals need to be added or there are enough already
  size_t num_call_saves;
  size_t total_call_saves_size;

  // used during emitting
  size_t offset;
};

struct ir_unit {
  struct ir_builder **funcs;
  size_t num_funcs;
};

typedef void(walk_op_callback)(struct ir_op **op, void *metadata);

void walk_stmt(struct ir_stmt *stmt, walk_op_callback *cb, void *cb_metadata);
void walk_op(struct ir_op *op, walk_op_callback *cb, void *cb_metadata);
void walk_op_uses(struct ir_op *op, walk_op_callback *cb, void *cb_metadata);

bool stmt_is_empty(struct ir_stmt *stmt);
bool basicblock_is_empty(struct ir_basicblock *basicblock);

void prune_basicblocks(struct ir_builder *irb);
void prune_stmts(struct ir_builder *irb, struct ir_basicblock *basicblock);

void clear_metadata(struct ir_builder *irb);
void rebuild_ids(struct ir_builder *irb);

struct ir_op *alloc_ir_op(struct ir_builder *irb, struct ir_stmt *stmt);

struct ir_stmt *alloc_ir_stmt(struct ir_builder *irb,
                              struct ir_basicblock *basicblock);

struct ir_basicblock *alloc_ir_basicblock(struct ir_builder *irb);

void add_pred_to_basicblock(struct ir_builder *irb,
                            struct ir_basicblock *basicblock,
                            struct ir_basicblock *pred);

// NOTE: does NOT connect the blocks to the end (return) block, must be done manually
struct ir_basicblock *insert_basicblocks_after(struct ir_builder *irb,
                              struct ir_op *insert_after,
                              struct ir_basicblock *first);

// Helper method that ensures the essential fields in IR op are initialised
void initialise_ir_op(struct ir_op *op, size_t id, enum ir_op_ty ty,
                      struct ir_op_var_ty var_ty, unsigned long reg,
                      unsigned long lcl_idx);

void move_after_ir_op(struct ir_builder *irb, struct ir_op *op,
                      struct ir_op *move_after);
void move_before_ir_op(struct ir_builder *irb, struct ir_op *op,
                       struct ir_op *move_before);

// swaps ops but does NOT swap their uses - expressions pointing to `left` will
// now point to `right`
void swap_ir_ops(struct ir_builder *irb, struct ir_op *left,
                 struct ir_op *right);

struct ir_op *insert_before_ir_op(struct ir_builder *irb,
                                  struct ir_op *insert_before, enum ir_op_ty ty,
                                  struct ir_op_var_ty var_ty);

struct ir_op *insert_after_ir_op(struct ir_builder *irb,
                                 struct ir_op *insert_after, enum ir_op_ty ty,
                                 struct ir_op_var_ty var_ty);

size_t var_ty_size(struct ir_builder *irb, struct ir_op_var_ty *ty);

#endif
