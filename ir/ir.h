#ifndef IR_IR_H
#define IR_IR_H

#include "../target.h"

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

enum ir_op_ty {
  IR_OP_TY_UNKNOWN,

  IR_OP_TY_PHI,

  IR_OP_TY_UNDF,

  // only used late in the pipeline for eliminating phi nodes
  IR_OP_TY_MOV,

  IR_OP_TY_CNST,

  IR_OP_TY_BINARY_OP,
  IR_OP_TY_UNARY_OP,
  IR_OP_TY_CAST_OP,

  IR_OP_TY_STORE,
  IR_OP_TY_LOAD,
  IR_OP_TY_STORE_BITFIELD,
  IR_OP_TY_LOAD_BITFIELD,

  IR_OP_TY_BITFIELD_EXTRACT,
  IR_OP_TY_BITFIELD_INSERT,

  IR_OP_TY_MEM_SET,

  IR_OP_TY_ADDR,
  IR_OP_TY_ADDR_OFFSET,

  IR_OP_TY_BR,
  IR_OP_TY_BR_COND,
  IR_OP_TY_BR_SWITCH,
  IR_OP_TY_RET,
  IR_OP_TY_CALL,

  // represents a custom instruction understood by a particular backend
  IR_OP_TY_CUSTOM,
};

struct ir_op_mov {
  struct ir_op *value;
};

struct ir_phi_entry {
  struct ir_basicblock *basicblock;
  struct ir_op *value;
};

struct ir_op_phi {
  struct ir_phi_entry *values;
  size_t num_values;
};

enum ir_op_cnst_ty {
  IR_OP_CNST_TY_FLT,
  IR_OP_CNST_TY_INT,
};

struct ir_op_cnst {
  enum ir_op_cnst_ty ty;

  union {
    unsigned long long int_value;
    long double flt_value;
  };
};

struct ir_op_ret {
  struct ir_op *value;
};

enum ir_op_cast_op_ty {
  IR_OP_CAST_OP_TY_SEXT,
  IR_OP_CAST_OP_TY_ZEXT,
  IR_OP_CAST_OP_TY_TRUNC,

  // convert between float types
  IR_OP_CAST_OP_TY_CONV,

  // convert float <-> unsigned
  IR_OP_CAST_OP_TY_UCONV,

  // convert float <-> signed
  IR_OP_CAST_OP_TY_SCONV,
};

struct ir_op_cast_op {
  enum ir_op_cast_op_ty ty;
  struct ir_op *value;
};

enum ir_op_unary_op_ty {
  IR_OP_UNARY_OP_TY_FNEG,
  IR_OP_UNARY_OP_TY_FSQRT,
  IR_OP_UNARY_OP_TY_FABS,
  IR_OP_UNARY_OP_TY_NEG,
  IR_OP_UNARY_OP_TY_LOGICAL_NOT,
  IR_OP_UNARY_OP_TY_NOT,
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

  IR_OP_BINARY_OP_TY_FMAX,
  IR_OP_BINARY_OP_TY_FMIN,

  IR_OP_BINARY_OP_TY_FEQ,
  IR_OP_BINARY_OP_TY_FNEQ,

  IR_OP_BINARY_OP_TY_FGT,
  IR_OP_BINARY_OP_TY_FGTEQ,
  IR_OP_BINARY_OP_TY_FLT,
  IR_OP_BINARY_OP_TY_FLTEQ,

  IR_OP_BINARY_OP_TY_LSHIFT,
  IR_OP_BINARY_OP_TY_SRSHIFT,
  IR_OP_BINARY_OP_TY_URSHIFT,
  IR_OP_BINARY_OP_TY_AND,
  IR_OP_BINARY_OP_TY_OR,
  IR_OP_BINARY_OP_TY_XOR,

  IR_OP_BINARY_OP_TY_ADD,
  IR_OP_BINARY_OP_TY_SUB,
  IR_OP_BINARY_OP_TY_MUL,
  IR_OP_BINARY_OP_TY_SDIV,
  IR_OP_BINARY_OP_TY_UDIV,
  IR_OP_BINARY_OP_TY_SQUOT,
  IR_OP_BINARY_OP_TY_UQUOT,

  IR_OP_BINARY_OP_TY_FADD,
  IR_OP_BINARY_OP_TY_FSUB,
  IR_OP_BINARY_OP_TY_FMUL,
  IR_OP_BINARY_OP_TY_FDIV,
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
enum ir_var_primitive_ty {
  IR_VAR_PRIMITIVE_TY_I8,
  IR_VAR_PRIMITIVE_TY_I16,
  IR_VAR_PRIMITIVE_TY_I32,
  IR_VAR_PRIMITIVE_TY_I64,

  IR_VAR_PRIMITIVE_TY_F16,
  IR_VAR_PRIMITIVE_TY_F32,
  IR_VAR_PRIMITIVE_TY_F64,
};

enum ir_var_ty_ty {
  /* Does not produce a value */
  IR_VAR_TY_TY_NONE,

  /* Primitives - integers, floats, pointers */
  IR_VAR_TY_TY_PRIMITIVE,
  IR_VAR_TY_TY_FUNC,

  IR_VAR_TY_TY_POINTER,

  /* Aggregate */
  IR_VAR_TY_TY_ARRAY,
  IR_VAR_TY_TY_STRUCT,
  IR_VAR_TY_TY_UNION,

  /* Variadic */
  IR_VAR_TY_TY_VARIADIC,
};

enum ir_var_func_ty_flags {
  IR_VAR_FUNC_TY_FLAG_NONE = 0,
  IR_VAR_FUNC_TY_FLAG_VARIADIC = 1
};

struct ir_var_func_ty {
  struct ir_var_ty *ret_ty;
  size_t num_params;
  struct ir_var_ty *params;

  enum ir_var_func_ty_flags flags;
};

bool is_func_variadic(const struct ir_var_func_ty *ty);

struct ir_var_struct_ty {
  size_t num_fields;
  struct ir_var_ty *fields;
};

struct ir_var_union_ty {
  size_t num_fields;
  struct ir_var_ty *fields;
};

struct ir_var_array_ty {
  struct ir_var_ty *underlying;

  union {
    size_t num_elements;
  };
};

struct ir_var_ty {
  enum ir_var_ty_ty ty;

  union {
    enum ir_var_primitive_ty primitive;
    struct ir_var_func_ty func;
    struct ir_var_array_ty array;
    struct ir_var_struct_ty struct_ty;
    struct ir_var_union_ty union_ty;
  };
};

extern const struct ir_var_ty IR_VAR_TY_UNKNOWN;
extern const struct ir_var_ty IR_VAR_TY_POINTER;
extern const struct ir_var_ty IR_VAR_TY_NONE;
extern const struct ir_var_ty IR_VAR_TY_I8;
extern const struct ir_var_ty IR_VAR_TY_I16;
extern const struct ir_var_ty IR_VAR_TY_I32;
extern const struct ir_var_ty IR_VAR_TY_I64;
extern const struct ir_var_ty IR_VAR_TY_F32;
extern const struct ir_var_ty IR_VAR_TY_F64;
extern const struct ir_var_ty IR_VAR_TY_VARIADIC;

struct ir_op_call {
  struct ir_var_ty func_ty;

  struct ir_op *target;
  size_t num_args;
  struct ir_op **args;

  // must be preserved because of unspecified functions e.g `int foo();
  // foo(7, 8.3)`
  struct ir_var_ty *arg_var_tys;
};

struct ir_op_addr_offset {
  // `addr = base + (index * scale) + offset`

  struct ir_op *base;
  struct ir_op *index;
  size_t scale;

  size_t offset;
};

struct ir_bitfield {
  size_t offset;
  size_t width;
};

struct ir_op_bitfield_extract {
  struct ir_op *value;

  struct ir_bitfield bitfield;
};

struct ir_op_bitfield_insert {
  struct ir_op *target;
  struct ir_op *value;

  struct ir_bitfield bitfield;
};

enum ir_op_store_ty {
  IR_OP_STORE_TY_LCL,
  IR_OP_STORE_TY_GLB,
  IR_OP_STORE_TY_ADDR,
};

struct ir_op_store {
  enum ir_op_store_ty ty;

  struct ir_op *value;

  union {
    struct ir_lcl *lcl;
    struct ir_glb *glb;
    struct ir_op *addr;
  };
};

struct ir_op_store_bitfield {
  enum ir_op_store_ty ty;

  struct ir_op *value;
  struct ir_bitfield bitfield;

  union {
    struct ir_lcl *lcl;
    struct ir_glb *glb;
    struct ir_op *addr;
  };
};

enum ir_op_load_ty {
  IR_OP_LOAD_TY_LCL,
  IR_OP_LOAD_TY_GLB,
  IR_OP_LOAD_TY_ADDR,
};

struct ir_op_load {
  enum ir_op_load_ty ty;

  union {
    struct ir_lcl *lcl;
    struct ir_glb *glb;
    struct ir_op *addr;
  };
};

struct ir_op_load_bitfield {
  enum ir_op_load_ty ty;

  struct ir_bitfield bitfield;

  union {
    struct ir_lcl *lcl;
    struct ir_glb *glb;
    struct ir_op *addr;
  };
};

enum ir_op_addr_ty {
  IR_OP_ADDR_TY_LCL,
  IR_OP_ADDR_TY_GLB,
};

struct ir_op_addr {
  enum ir_op_addr_ty ty;

  union {
    struct ir_lcl *lcl;
    struct ir_glb *glb;
  };
};

struct ir_op_br_switch {
  struct ir_op *value;
  // targets on `ir_basicblock`
};

struct ir_op_br_cond {
  struct ir_op *cond;
  // targets on `ir_basicblock`
};

struct ir_op_mem_set {
  struct ir_op *addr;
  unsigned char value;
  size_t length;
};

struct aarch64_op;
struct eep_op;

struct ir_op_custom {
  union {
    struct aarch64_op *aarch64;
    struct eep_op *eep;
  };
};

enum ir_reg_ty {
  IR_REG_TY_NONE,
  IR_REG_TY_SPILLED,
  IR_REG_TY_FLAGS,
  IR_REG_TY_INTEGRAL,
  IR_REG_TY_FP,
};

struct ir_reg {
  enum ir_reg_ty ty;

  union {
    unsigned long idx;
  };
};

#define NO_REG                                                                 \
  (struct ir_reg) { .ty = IR_REG_TY_NONE }
#define REG_SPILLED                                                            \
  (struct ir_reg) { .ty = IR_REG_TY_SPILLED }
#define REG_FLAGS                                                              \
  (struct ir_reg) { .ty = IR_REG_TY_FLAGS }

enum ir_op_flags {
  IR_OP_FLAG_NONE = 0,

  // Force this variable to spill
  IR_OP_FLAG_MUST_SPILL = 1,

  // Indicates this is a magic mov representing a parameter
  IR_OP_FLAG_PARAM = 2,

  // indicates this value is passed as a variadic
  IR_OP_FLAG_VARIADIC_PARAM = 4,

  // indicates the op is a load_lcl/store_lcl used for a spill
  IR_OP_FLAG_SPILL = 8,

  // indicates this op does not generate any instructions and it is encoded in
  // its uses
  IR_OP_FLAG_CONTAINED = 16,

  // reg is assigned and cannot change
  IR_OP_FLAG_FIXED_REG = 32,

  // op has side effects (e.g is an assignment)
  IR_OP_FLAG_SIDE_EFFECTS = 64,

  // op has been spilled and all consumers must reload it
  IR_OP_FLAG_SPILLED = 128,

  // this op is a mov used to eliminate phis
  IR_OP_FLAG_PHI_MOV = 256,

  // this op reads from dest reg, so it cannot be overwritten
  IR_OP_FLAG_READS_DEST = 512
};

struct ir_op_write_info {
  size_t num_reg_writes;
  struct ir_reg writes[4];
};

struct ir_op {
  size_t id;
  enum ir_op_ty ty;

  enum ir_op_flags flags;

  struct ir_var_ty var_ty;

  struct ir_op *pred;
  struct ir_op *succ;
  struct ir_stmt *stmt;
  union {
    struct ir_op_cnst cnst;
    struct ir_op_call call;
    struct ir_op_binary_op binary_op;
    struct ir_op_unary_op unary_op;
    struct ir_op_cast_op cast_op;
    struct ir_op_ret ret;
    struct ir_op_mem_set mem_set;
    struct ir_op_store store;
    struct ir_op_load load;
    struct ir_op_store_bitfield store_bitfield;
    struct ir_op_load_bitfield load_bitfield;
    struct ir_op_bitfield_extract bitfield_extract;
    struct ir_op_bitfield_insert bitfield_insert;
    struct ir_op_addr addr;
    struct ir_op_addr_offset addr_offset;
    struct ir_op_br_cond br_cond;
    struct ir_op_br_switch br_switch;
    /* br has no entry, as its target is on `ir_basicblock` and it has no
     * condition */
    struct ir_op_phi phi;
    struct ir_op_mov mov;
    struct ir_op_custom custom;
  };

  struct ir_lcl *lcl;

  // only meaningful post register-allocation
  struct ir_reg reg;
  void *metadata;

  // contains any registers other than `reg` which this instruction writes to
  struct ir_op_write_info write_info;

  const char *comment;
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

enum ir_basicblock_ty {
  // a return has no explicit successors
  IR_BASICBLOCK_TY_RET,

  // a split basicblock has 2 successors and occurs when there is a conditional
  // branch
  IR_BASICBLOCK_TY_SPLIT,

  // a merge basicblock has 1 successor (but its successor will have at least 2
  // predecessors)
  // and occurs when multiple basicblocks rejoin
  IR_BASICBLOCK_TY_MERGE,

  // a list of possible values to jump to, and then false branch
  IR_BASICBLOCK_TY_SWITCH,
};

struct ir_basicblock_merge {
  struct ir_basicblock *target;
};

struct ir_basicblock_split {
  struct ir_basicblock *true_target;
  struct ir_basicblock *false_target;
};

struct ir_split_case {
  unsigned long long value;
  struct ir_basicblock *target;
};

struct ir_basicblock_switch {
  size_t num_cases;
  struct ir_split_case *cases;

  struct ir_basicblock *default_target;
};

struct ir_basicblock {
  size_t id;

  // a NULL irb means a pruned basicblock
  struct ir_func *irb;

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
    struct ir_basicblock_switch switch_case;
  };

  struct instr *first_instr;
  struct instr *last_instr;

  void *metadata;
  const char *comment;
};

enum ir_func_flags { IR_FUNC_FLAG_NONE = 0, IR_FUNC_FLAG_MAKES_CALL = 1, IR_FUNC_FLAG_NEEDS_SSP = 2 };

enum ir_var_data_ty {
  IR_VAR_TY_STRING_LITERAL,
  IR_VAR_TY_CONST_DATA,
  IR_VAR_TY_DATA
};

struct ir_var_addr {
  struct ir_glb *glb;
  unsigned long long offset;
};

struct ir_var_value_list {
  struct ir_var_value *values;
  size_t *offsets;
  size_t num_values;
};

enum ir_var_value_ty {
  IR_VAR_VALUE_TY_ZERO,
  IR_VAR_VALUE_TY_INT,
  IR_VAR_VALUE_TY_FLT,
  IR_VAR_VALUE_TY_STR,
  IR_VAR_VALUE_TY_ADDR,
  IR_VAR_VALUE_TY_VALUE_LIST,
};

struct ir_var_value {
  enum ir_var_value_ty ty;
  struct ir_var_ty var_ty;

  union {
    const char *str_value;
    unsigned long long int_value;
    long double flt_value;
    struct ir_var_addr addr;
    struct ir_var_value_list value_list;
  };
};

struct ir_var {
  enum ir_var_data_ty ty;

  struct ir_var_ty var_ty;
  struct ir_var_value value;
};

enum ir_glb_ty {
  IR_GLB_TY_DATA,
  IR_GLB_TY_FUNC,
};

enum ir_glb_def_ty {
  IR_GLB_DEF_TY_DEFINED,
  IR_GLB_DEF_TY_UNDEFINED,
  IR_GLB_DEF_TY_TENTATIVE
};

enum ir_linkage { IR_LINKAGE_NONE, IR_LINKAGE_INTERNAL, IR_LINKAGE_EXTERNAL };

struct ir_glb {
  size_t id;

  enum ir_glb_ty ty;
  enum ir_glb_def_ty def_ty;
  enum ir_linkage linkage;

  struct ir_glb *pred;
  struct ir_glb *succ;

  const char *name;

  struct ir_var_ty var_ty;

  union {
    struct ir_var *var;
    struct ir_func *func;
  };
};

enum ir_lcl_flags {
  IR_LCL_FLAG_NONE = 0,

  // spill generated by regalloc
  IR_LCL_FLAG_SPILL = 1,
};

struct ir_lcl {
  size_t id;

  struct ir_var_ty var_ty;

  struct ir_lcl *pred;
  struct ir_lcl *succ;

  enum ir_lcl_flags flags;

  // each local is effectively in SSA, where it is stored to only once
  struct ir_op *store;

  void *metadata;

  // HACK: this sucks, stores the current offset of the local but means they
  // cannot be compacted
  size_t offset;
};

struct ir_reg_usage {
  struct bitset *gp_registers_used;
  struct bitset *fp_registers_used;
};

struct ir_func {
  struct ir_unit *unit;

  const char *name;

  struct ir_var_func_ty func_ty;

  struct arena_allocator *arena;

  struct ir_basicblock *first;
  struct ir_basicblock *last;

  enum ir_func_flags flags;

  size_t basicblock_count;
  size_t stmt_count;
  size_t op_count;
  size_t next_basicblock_id;
  size_t next_stmt_id;
  size_t next_op_id;

  size_t num_locals;
  struct ir_lcl *first_local;
  struct ir_lcl *last_local;

  struct ir_reg_usage reg_usage;

  // number of stack local variables
  size_t total_locals_size;
};

struct ir_unit {
  struct arena_allocator *arena;
  struct typechk *tchk;
  const struct target *target;

  struct ir_glb *first_global;
  struct ir_glb *last_global;

  size_t num_globals;

  struct {
    struct ir_glb *memmove;
    struct ir_glb *memset;
  } well_known_glbs;
};

enum ir_well_known_glb {
  IR_WELL_KNOWN_GLB_MEMMOVE,
  IR_WELL_KNOWN_GLB_MEMSET,
};

struct ir_glb *add_well_known_global(struct ir_unit *iru, enum ir_well_known_glb glb);

typedef void(walk_op_callback)(struct ir_op **op, void *metadata);

bool op_has_side_effects(const struct ir_op *ty);
bool op_produces_value(const struct ir_op *ty);
bool op_is_branch(enum ir_op_ty ty);

void walk_stmt(struct ir_stmt *stmt, walk_op_callback *cb, void *cb_metadata);
void walk_op(struct ir_op *op, walk_op_callback *cb, void *cb_metadata);
void walk_op_uses(struct ir_op *op, walk_op_callback *cb, void *cb_metadata);

bool stmt_is_empty(struct ir_stmt *stmt);
bool basicblock_is_empty(struct ir_basicblock *basicblock);

void prune_basicblocks(struct ir_func *irb);
void prune_stmts(struct ir_func *irb, struct ir_basicblock *basicblock);

void clear_metadata(struct ir_func *irb);
void rebuild_ids(struct ir_func *irb);

struct ir_lcl *add_local(struct ir_func *irb, const struct ir_var_ty *var_ty);

struct ir_glb *add_global(struct ir_unit *iru, enum ir_glb_ty ty,
                          const struct ir_var_ty *var_ty,
                          enum ir_glb_def_ty def_ty, const char *name);

struct ir_op *alloc_ir_op(struct ir_func *irb, struct ir_stmt *stmt);

// clones an op so it can be marked contained
// else we would need to ensure all consumers can contain it
struct ir_op *alloc_contained_ir_op(struct ir_func *irb, struct ir_op *op,
                                    struct ir_op *consumer);

// fixing an op is difficult, because fixed ops with overlapping lifetimes won't be able to allocate
// so to fix an op, you provide the point it must be fixed at (`consumer`) and we insert a mov before then and fix that
// we take `**op` so that we can then reassign the consumer op to the fixed move
struct ir_op *alloc_fixed_reg_dest_ir_op(struct ir_func *irb, struct ir_op **op,
                                    struct ir_op *consumer, struct ir_reg reg);

struct ir_op *alloc_fixed_reg_source_ir_op(struct ir_func *irb,
                                    struct ir_op *producer, struct ir_reg reg);

void mk_integral_constant(struct ir_unit *iru, struct ir_op *op,
                            enum ir_var_primitive_ty ty,
                            unsigned long long value);
void mk_pointer_constant(struct ir_unit *iru, struct ir_op *op,
                           unsigned long long value);

struct ir_op *build_addr(struct ir_func *irb, struct ir_op *load);

struct ir_stmt *alloc_ir_stmt(struct ir_func *irb,
                              struct ir_basicblock *basicblock);

struct ir_basicblock *alloc_ir_basicblock(struct ir_func *irb);

void add_pred_to_basicblock(struct ir_func *irb,
                            struct ir_basicblock *basicblock,
                            struct ir_basicblock *pred);

// NOTE: does NOT connect the blocks to the end (return) block, must be done
// manually
struct ir_basicblock *insert_basicblocks_after_op(struct ir_func *irb,
                                                  struct ir_op *insert_after,
                                                  struct ir_basicblock *first);

void make_basicblock_split(struct ir_func *irb,
                           struct ir_basicblock *basicblock,
                           struct ir_basicblock *true_target,
                           struct ir_basicblock *false_target);

void make_basicblock_merge(struct ir_func *irb,
                           struct ir_basicblock *basicblock,
                           struct ir_basicblock *target);

void make_basicblock_switch(struct ir_func *irb,
                            struct ir_basicblock *basicblock, size_t num_cases,
                            struct ir_split_case *cases,
                            struct ir_basicblock *default_target);

void detach_ir_basicblock(struct ir_func *irb,
                          struct ir_basicblock *basicblock);
void detach_ir_stmt(struct ir_func *irb, struct ir_stmt *stmt);
void detach_ir_op(struct ir_func *irb, struct ir_op *op);

void initialise_ir_basicblock(struct ir_basicblock *basicblock, size_t id);

// Helper method that ensures the essential fields in IR op are initialised
void initialise_ir_op(struct ir_op *op, size_t id, enum ir_op_ty ty,
                      struct ir_var_ty var_ty, struct ir_reg,
                      struct ir_lcl *lcl);

void move_after_ir_op(struct ir_func *irb, struct ir_op *op,
                      struct ir_op *move_after);
void move_before_ir_op(struct ir_func *irb, struct ir_op *op,
                       struct ir_op *move_before);

void attach_ir_op(struct ir_func *irb, struct ir_op *op, struct ir_stmt *stmt,
                  struct ir_op *pred, struct ir_op *succ);

void move_after_ir_basicblock(struct ir_func *irb,
                              struct ir_basicblock *basicblock,
                              struct ir_basicblock *move_after);
void move_before_ir_basicblock(struct ir_func *irb,
                               struct ir_basicblock *basicblock,
                               struct ir_basicblock *move_before);

void attach_ir_basicblock(struct ir_func *irb, struct ir_basicblock *basicblock,
                          struct ir_basicblock *pred,
                          struct ir_basicblock *succ);

// swaps ops but does NOT swap their uses - expressions pointing to `left` will
// now point to `right`
void swap_ir_ops(struct ir_func *irb, struct ir_op *left, struct ir_op *right);

// swaps ops AND their uses
void swap_ir_ops_in_place(struct ir_func *irb, struct ir_op *left, struct ir_op *right);

struct ir_op *replace_ir_op(struct ir_func *irb, struct ir_op *op,
                            enum ir_op_ty ty, struct ir_var_ty var_ty);

struct ir_op *insert_before_ir_op(struct ir_func *irb,
                                  struct ir_op *insert_before, enum ir_op_ty ty,
                                  struct ir_var_ty var_ty);

struct ir_op *insert_after_ir_op(struct ir_func *irb,
                                 struct ir_op *insert_after, enum ir_op_ty ty,
                                 struct ir_var_ty var_ty);

struct ir_op *insert_phi(struct ir_func *irb, struct ir_basicblock *basicblock,
                         struct ir_var_ty var_ty);

struct ir_basicblock *
insert_before_ir_basicblock(struct ir_func *irb,
                            struct ir_basicblock *insert_before);

struct ir_basicblock *
insert_after_ir_basicblock(struct ir_func *irb,
                           struct ir_basicblock *insert_after);

struct ir_var_ty_info {
  size_t size;
  size_t alignment;

  size_t num_fields;
  size_t *offsets;
};

struct ir_var_ty_info var_ty_info(struct ir_unit *iru,
                                  const struct ir_var_ty *ty);

enum ir_var_primitive_ty var_ty_pointer_primitive_ty(struct ir_unit *iru);

struct ir_var_ty var_ty_for_pointer_size(struct ir_unit *iru);
struct ir_var_ty var_ty_make_array(struct ir_unit *iru,
                                   const struct ir_var_ty *underlying,
                                   size_t num_elements);

bool var_ty_is_primitive(const struct ir_var_ty *var_ty,
                         enum ir_var_primitive_ty primitive);
bool var_ty_is_integral(const struct ir_var_ty *var_ty);
bool var_ty_is_fp(const struct ir_var_ty *var_ty);
bool var_ty_is_aggregate(const struct ir_var_ty *var_ty);

bool var_ty_eq(struct ir_func *irb, const struct ir_var_ty *l,
               const struct ir_var_ty *r);

struct ir_op *spill_op(struct ir_func *irb, struct ir_op *op);

struct ir_op_use {
  struct ir_op *op;

  struct ir_op *uses;
  size_t num_uses;
};

struct ir_op_uses {
  struct ir_op_use *use_datas;
  size_t num_use_datas;
};

struct ir_op_uses build_op_uses_map(struct ir_func *func);

size_t unique_idx_for_ir_reg(struct ir_reg reg);
struct ir_reg ir_reg_for_unique_idx(size_t idx);

struct location {
  size_t idx;
  void *metadata;
};

struct move {
  struct location from;
  struct location to;
};

struct move_set {
  struct move *moves;
  size_t num_moves;
};

struct move_set gen_move_order(struct arena_allocator *arena,
                               struct location *from, struct location *to,
                               size_t num, size_t tmp_index);

#endif
