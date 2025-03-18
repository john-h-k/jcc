#ifndef IR_IR_H
#define IR_IR_H

#include "../util.h"

#include <stdbool.h>
#include <stdint.h>

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
  IR_OP_TY_MEM_COPY,

  IR_OP_TY_ADDR,
  IR_OP_TY_ADDR_OFFSET,

  IR_OP_TY_BR,
  IR_OP_TY_BR_COND,
  IR_OP_TY_BR_SWITCH,
  IR_OP_TY_RET,
  IR_OP_TY_CALL,

  // gathers a set of SSA values into an aggregate
  IR_OP_TY_GATHER,

  // Low-level operations that only occur after lowering
  // IR_OP_TY_STORE_REG,
  // IR_OP_TY_LOAD_REG,
  // IR_OP_TY_MOV_REG,
};

struct ir_gather_value {
  struct ir_op *value;
  size_t field_idx;
};

struct ir_op_gather {
  struct ir_gather_value *values;
  size_t num_values;
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
  IR_OP_BINARY_OP_TY_SMOD,
  IR_OP_BINARY_OP_TY_UMOD,

  IR_OP_BINARY_OP_TY_FADD,
  IR_OP_BINARY_OP_TY_FSUB,
  IR_OP_BINARY_OP_TY_FMUL,
  IR_OP_BINARY_OP_TY_FDIV,
};

bool ir_binary_op_is_comparison(enum ir_op_binary_op_ty ty);
enum ir_op_binary_op_ty ir_invert_binary_comparison(enum ir_op_binary_op_ty ty);
enum ir_op_binary_op_ty ir_flip_binary_comparison(enum ir_op_binary_op_ty ty);

enum ir_op_sign { IR_OP_SIGN_NA, IR_OP_SIGN_SIGNED, IR_OP_SIGN_UNSIGNED };

enum ir_op_sign ir_binary_op_sign(enum ir_op_binary_op_ty);

struct ir_op_binary_op {
  enum ir_op_binary_op_ty ty;
  struct ir_op *lhs;
  struct ir_op *rhs;
};

// IR does not have sign encoded in type (so `int` and `unsigned` are both
// IR_OP_VAR_TY_32) and instead encodes it in operations (e.g there are
// different IR ops for signed and unsigned division)
enum ir_var_primitive_ty {
  IR_VAR_PRIMITIVE_TY_I1,
  IR_VAR_PRIMITIVE_TY_I8,
  IR_VAR_PRIMITIVE_TY_I16,
  IR_VAR_PRIMITIVE_TY_I32,
  IR_VAR_PRIMITIVE_TY_I64,
  IR_VAR_PRIMITIVE_TY_I128,

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

bool ir_is_func_variadic(const struct ir_var_func_ty *ty);

struct ir_var_aggregate_ty {
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
    struct ir_var_aggregate_ty aggregate;
  };
};

extern const struct ir_var_ty IR_VAR_TY_UNKNOWN;
extern const struct ir_var_ty IR_VAR_TY_POINTER;
extern const struct ir_var_ty IR_VAR_TY_NONE;
extern const struct ir_var_ty IR_VAR_TY_I8;
extern const struct ir_var_ty IR_VAR_TY_I16;
extern const struct ir_var_ty IR_VAR_TY_I32;
extern const struct ir_var_ty IR_VAR_TY_I64;
extern const struct ir_var_ty IR_VAR_TY_F16;
extern const struct ir_var_ty IR_VAR_TY_F32;
extern const struct ir_var_ty IR_VAR_TY_F64;
extern const struct ir_var_ty IR_VAR_TY_VARIADIC;

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

enum ir_param_info_ty {
  IR_PARAM_INFO_TY_REGISTER,
  IR_PARAM_INFO_TY_STACK,
  IR_PARAM_INFO_TY_POINTER,
};

struct ir_param_reg {
  // e.g `struct { float[3] }` would have size 4, but `struct { char[16] } would
  // have size 8`

  struct ir_reg reg;
  size_t size;
};

struct ir_param_info {
  enum ir_param_info_ty ty;

  const struct ir_var_ty *var_ty;

  // offset within the param
  size_t offset;

  union {
    struct {
      size_t num_regs;
      struct ir_param_reg regs[8]; // also used for pointer
    };
    size_t stack_offset;
  };
};

enum ir_call_info_flags {
  IR_CALL_INFO_FLAG_NONE = 0,

  // place the number of variadic arguments into the specified register
  IR_CALL_INFO_FLAG_NUM_VARIADIC = 1,
};

struct ir_call_info {
  size_t stack_size;

  size_t num_params;
  struct ir_param_info *params;

  struct ir_param_info *ret;

  size_t num_variadics;
  enum ir_call_info_flags flags;

  union {
    struct ir_reg num_variadics_reg;
  };
};

struct ir_func_info {
  struct ir_var_func_ty func_ty;
  struct ir_call_info call_info;
};

struct ir_op_call {
  struct ir_var_ty func_ty;
  struct ir_func_info func_info;

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

// enum ir_mem_copy_flags {
//   IR_MEM_COPY_FLAG_NONE = 0,

//   // the instruction is free to use any volatile registers
//   IR_MEM_COPY_FLAG_VOLATILE = 1
// };

struct ir_op_mem_copy {
  struct ir_op *source;
  struct ir_op *dest;

  size_t length;

  // enum ir_mem_copy_flags flags;
};

bool ir_reg_eq(struct ir_reg left, struct ir_reg right);
bool ir_var_ty_has_reg(const struct ir_var_ty var_ty);
enum ir_reg_ty ir_reg_ty_for_var_ty(const struct ir_var_ty var_ty);

bool ir_primitive_ty_is_integral(enum ir_var_primitive_ty ty);
bool ir_primitive_ty_is_fp(enum ir_var_primitive_ty ty);

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
  IR_OP_FLAG_SIDE_EFFECTS = 128,

  // op has been spilled and all consumers must reload it
  IR_OP_FLAG_SPILLED = 256,

  // this op is a mov used to eliminate phis
  IR_OP_FLAG_PHI_MOV = 512,

  // this op reads from dest reg, so it cannot be overwritten
  IR_OP_FLAG_READS_DEST = 1024,

  // is a promoted value
  IR_OP_FLAG_PROMOTED = 2048,

  // HACK: makes op live forever
  IR_OP_FLAG_ETERNAL = 4096,
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
    struct ir_op_mem_copy mem_copy;
    struct ir_op_store store;
    struct ir_op_load load;
    struct ir_op_store_bitfield store_bitfield;
    struct ir_op_load_bitfield load_bitfield;
    struct ir_op_bitfield_extract bitfield_extract;
    struct ir_op_bitfield_insert bitfield_insert;
    struct ir_op_addr addr;
    struct ir_op_addr_offset addr_offset;
    struct ir_op_gather gather;
    struct ir_op_br_cond br_cond;
    struct ir_op_br_switch br_switch;
    /* br has no entry, as its target is on `ir_basicblock` and it has no
     * condition */
    struct ir_op_phi phi;
    struct ir_op_mov mov;
  };

  struct ir_lcl *lcl;

  // only meaningful post register-allocation
  struct ir_reg reg;
  void *metadata;

  // contains any registers other than `reg` which this instruction writes to
  struct ir_op_write_info write_info;

  const char *comment;
};

enum ir_stmt_flags {
  IR_STMT_FLAG_NONE = 0,

  // contains phi and only phi nodes
  IR_STMT_FLAG_PHI = 1 << 0,

  // contains param and only param nodes
  IR_STMT_FLAG_PARAM = 1 << 1,

  // phi moves
  IR_STMT_FLAG_PHI_MOV = 1 << 2,
};

// set of ops with no SEQ_POINTs
struct ir_stmt {
  size_t id;

  // a NULL bb means a pruned stmt
  struct ir_basicblock *basicblock;

  struct ir_stmt *pred;
  struct ir_stmt *succ;

  enum ir_stmt_flags flags;

  // the links between ops (`pred` & `succ`) have no significance to the
  // compilation and are just for traversal. meaningful links between operations
  // are with in the op data, such as `ir_op->ret.value`, which points to the op
  // whos result is returned
  struct ir_op *first;

  // last is the dominating op of the statement, and can be used as its "root".
  // all other ops in the statement are reachable from it
  struct ir_op *last;

  const char *comment;
};

// ensures the basic block ends with an appropriate branch and does not contain
// any within it
bool ir_valid_basicblock(struct ir_basicblock *basicblock);

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
  struct ir_func *func;

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

  struct cg_basicblock *cg_basicblock;

  void *metadata;
  const char *comment;
};

enum ir_func_flags {
  IR_FUNC_FLAG_NONE = 0,
  IR_FUNC_FLAG_MAKES_CALL = 1,
  IR_FUNC_FLAG_NEEDS_SSP = 2
};

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

struct ir_var_str {
  const char *value;
  size_t len;
};

struct ir_var_value {
  enum ir_var_value_ty ty;
  struct ir_var_ty var_ty;

  union {
    struct ir_var_str str_value;
    unsigned long long int_value;
    long double flt_value;
    struct ir_var_addr addr;
    struct ir_var_value_list value_list;
  };
};

struct ir_var {
  struct ir_unit *unit;

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
  IR_LCL_FLAG_SPILL = 1 << 0,

  // local has been zeroed
  IR_LCL_FLAG_ZEROED = 1 << 1,

  // local is actually a param
  IR_LCL_FLAG_PARAM = 1 << 2,

  // local has been promoted, but still exists as it is a param, and so `lower`
  // is free to remove it if the ABI keeps the param in registers
  IR_LCL_FLAG_PROMOTED = 1 << 5,

  // saving a register across a call boundary
  IR_LCL_FLAG_CALL_SAVE = 1 << 6,
};

enum ir_lcl_alloc_ty {
  IR_LCL_ALLOC_TY_NONE,

  IR_LCL_ALLOC_TY_NORMAL,

  // fixed offset from stack pointer
  // this is used for stack arguments and return values
  IR_LCL_ALLOC_TY_FIXED,
};

struct ir_lcl_alloc {
  ssize_t offset;

  size_t size;
  size_t padding; // padding used before `offset`. this means removing the alloc
                  // is removing `size` and `padding`
};

struct ir_lcl {
  size_t id;

  struct ir_func *func;

  struct ir_var_ty var_ty;

  struct ir_lcl *pred;
  struct ir_lcl *succ;

  enum ir_lcl_flags flags;

  // each local is effectively in SSA, where it is stored to only once
  struct ir_op *store;

  void *metadata;

  // HACK: this sucks, stores the current offset of the local but means they
  // cannot be compacted
  enum ir_lcl_alloc_ty alloc_ty;
  union {
    struct ir_lcl_alloc alloc;
  };
};

struct ir_reg_info {
  struct ir_reg reg;
  struct ir_op *live;
};

struct ir_reg_state {
  size_t num_gp_registers;
  struct ir_reg_info *gp_registers;

  size_t num_fp_registers;
  struct ir_reg_info *fp_registers;
};

struct ir_reg_usage {
  size_t num_nonvolatile_used;
  struct ir_reg *nonvolatile_used;
};

struct ir_func {
  struct ir_unit *unit;

  const char *name;

  struct ir_var_func_ty func_ty;
  struct ir_call_info call_info;

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

  size_t lcl_count;
  size_t next_lcl_id;
  struct ir_lcl *first_lcl;
  struct ir_lcl *last_lcl;

  struct ir_reg_usage reg_usage;

  // number of stack local variables
  size_t total_locals_size;

  // amount of stack needed for calls, i.e that can't be taken up by locals
  size_t caller_stack_needed;
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
    struct ir_glb *memcpy;
    struct ir_glb *memset;
  } well_known_glbs;
};

enum ir_well_known_glb {
  IR_WELL_KNOWN_GLB_MEMMOVE,
  IR_WELL_KNOWN_GLB_MEMCPY,
  IR_WELL_KNOWN_GLB_MEMSET,
};

enum ir_func_iter_flags { IR_FUNC_ITER_FLAG_NONE = 0 };

struct ir_func_iter {
  struct ir_func *func;
  struct ir_op *op;
  enum ir_func_iter_flags flags;
};

enum ir_object_ty {
  IR_OBJECT_TY_GLB,
  IR_OBJECT_TY_LCL,

  IR_OBJECT_TY_FUNC,
  IR_OBJECT_TY_VAR,

  IR_OBJECT_TY_BASICBLOCK,
  IR_OBJECT_TY_STMT,
  IR_OBJECT_TY_OP,
};

struct ir_object {
  enum ir_object_ty ty;
  union {
    struct ir_glb *glb;
    struct ir_lcl *lcl;

    struct ir_func *func;
    struct ir_var *var;

    struct ir_basicblock *basicblock;
    struct ir_stmt *stmt;
    struct ir_op *op;
  };
};

#define DETACHED_BASICBLOCK (SIZE_MAX)
#define DETACHED_OP (SIZE_MAX)
#define DETACHED_STMT (SIZE_MAX)
#define DETACHED_LCL (SIZE_MAX)
#define DETACHED_GLB (SIZE_MAX)

// TODO: is this well defined? the casts
#define IR_MK_OBJECT(obj)                                                      \
  (_Generic((obj),                                                             \
      struct ir_glb *: (struct ir_object){.ty = IR_OBJECT_TY_GLB,              \
                                          .glb = (void *)(obj)},               \
      struct ir_lcl *: (struct ir_object){.ty = IR_OBJECT_TY_LCL,              \
                                          .lcl = (void *)(obj)},               \
                                                                               \
      struct ir_func *: (struct ir_object){.ty = IR_OBJECT_TY_FUNC,            \
                                           .func = (void *)(obj)},             \
      struct ir_var *: (struct ir_object){.ty = IR_OBJECT_TY_VAR,              \
                                          .var = (void *)(obj)},               \
      struct ir_basicblock *: (struct ir_object){.ty =                         \
                                                     IR_OBJECT_TY_BASICBLOCK,  \
                                                 .basicblock = (void *)(obj)}, \
                                                                               \
      struct ir_stmt *: (struct ir_object){.ty = IR_OBJECT_TY_STMT,            \
                                           .stmt = (void *)(obj)},             \
      struct ir_op *: (struct ir_object){.ty = IR_OBJECT_TY_OP,                \
                                         .op = (void *)(obj)}))

struct ir_func_iter ir_func_iter(struct ir_func *func,
                                 enum ir_func_iter_flags flags);
bool ir_func_iter_next(struct ir_func_iter *iter, struct ir_op **op);

struct ir_basicblock_succ_iter {
  struct ir_basicblock *basicblock;
  size_t idx;
};

struct ir_basicblock_succ_iter
ir_basicblock_succ_iter(struct ir_basicblock *basicblock);
bool ir_basicblock_succ_iter_next(struct ir_basicblock_succ_iter *iter,
                                  struct ir_basicblock **basicblock);

struct ir_glb *ir_add_well_known_global(struct ir_unit *iru,
                                        enum ir_well_known_glb glb);

enum ir_op_use_ty {
  IR_OP_USE_TY_READ,
  IR_OP_USE_TY_DEREF, // e.g `store.addr [%1], ...`
};

typedef void(ir_walk_op_uses_callback)(struct ir_op **op,
                                       enum ir_op_use_ty use_ty,
                                       void *metadata);
typedef void(ir_walk_op_callback)(struct ir_op *op, void *metadata);

bool ir_op_has_side_effects(const struct ir_op *ty);
bool ir_op_produces_value(const struct ir_op *ty);
bool ir_op_is_branch(enum ir_op_ty ty);

void ir_walk_stmt(struct ir_stmt *stmt, ir_walk_op_callback *cb,
                  void *cb_metadata);
void ir_walk_op(struct ir_op *op, ir_walk_op_callback *cb, void *cb_metadata);
void ir_walk_op_uses(struct ir_op *op, ir_walk_op_uses_callback *cb,
                     void *cb_metadata);

bool ir_stmt_is_empty(struct ir_stmt *stmt);
bool ir_basicblock_is_empty(struct ir_basicblock *basicblock);

void ir_prune_globals(struct ir_unit *iru);

void ir_prune_basicblocks(struct ir_func *irb);
void ir_prune_stmts(struct ir_func *irb, struct ir_basicblock *basicblock);

void ir_order_basicblocks(struct ir_func *func);

void ir_rebuild_flags(struct ir_func *func);

enum ir_eliminate_redundant_ops_flags {
  IR_ELIMINATE_REDUNDANT_OPS_FLAG_NONE = 0,

  // removes branches to the next basicblock
  // TODO: we should remove this and instead have codegen simply not generate a
  // branch if not needed
  IR_ELIMINATE_REDUNDANT_OPS_FLAG_ELIM_BRANCHES = 1 << 0,

  // allows more aggressive elimination of moves
  IR_ELIMINATE_REDUNDANT_OPS_FLAG_ELIM_MOVS = 1 << 1,

  // this is a negative flag because most passes _do_ want to eliminate locals
  // it is only late-stage passes which can't do this, because they have been
  // allocated or have target specific meanings
  IR_ELIMINATE_REDUNDANT_OPS_FLAG_DONT_ELIM_LCLS = 1 << 2,
};

void ir_eliminate_redundant_ops(struct ir_func *func,
                                enum ir_eliminate_redundant_ops_flags flags);

void ir_clear_metadata(struct ir_func *irb);
void ir_rebuild_glb_ids(struct ir_unit *iru);
void ir_rebuild_func_ids(struct ir_func *irb);

struct ir_lcl *ir_add_local(struct ir_func *irb,
                            const struct ir_var_ty *var_ty);
void ir_detach_local(struct ir_func *irb, struct ir_lcl *lcl);
void ir_alloc_locals(struct ir_func *func);

void ir_detach_global(struct ir_unit *iru, struct ir_glb *glb);
struct ir_glb *ir_add_global(struct ir_unit *iru, enum ir_glb_ty ty,
                             const struct ir_var_ty *var_ty,
                             enum ir_glb_def_ty def_ty, const char *name);

struct ir_op *ir_alloc_op(struct ir_func *irb, struct ir_stmt *stmt);

// clones an op so it can be marked contained
// else we would need to ensure all consumers can contain it
struct ir_op *ir_alloc_contained_op(struct ir_func *irb, struct ir_op *op,
                                    struct ir_op *consumer);

// fixing an op is difficult, because fixed ops with overlapping lifetimes won't
// be able to allocate so to fix an op, you provide the point it must be fixed
// at (`consumer`) and we insert a mov before then and fix that we take `**op`
// so that we can then reassign the consumer op to the fixed move
struct ir_op *ir_alloc_fixed_reg_dest_op(struct ir_func *irb, struct ir_op **op,
                                         struct ir_op *consumer,
                                         struct ir_reg reg);

struct ir_op *ir_alloc_fixed_reg_source_op(struct ir_func *irb,
                                           struct ir_op *producer,
                                           struct ir_reg reg);

void ir_mk_zero_constant(struct ir_unit *iru, struct ir_op *op,
                         struct ir_var_ty *var_ty);

void ir_mk_floating_zero_constant(struct ir_unit *iru, struct ir_op *op,
                                  enum ir_var_primitive_ty ty);

void ir_mk_integral_constant(struct ir_unit *iru, struct ir_op *op,
                             enum ir_var_primitive_ty ty,
                             unsigned long long value);
void ir_mk_pointer_constant(struct ir_unit *iru, struct ir_op *op,
                            unsigned long long value);

struct ir_op *ir_build_addr(struct ir_func *irb, struct ir_op *op);

struct ir_stmt *ir_alloc_stmt(struct ir_func *irb,
                              struct ir_basicblock *basicblock);

struct ir_basicblock *ir_alloc_basicblock(struct ir_func *irb);

void ir_add_pred_to_basicblock(struct ir_func *irb,
                               struct ir_basicblock *basicblock,
                               struct ir_basicblock *pred);

// NOTE: does NOT connect the blocks to the end (return) block, must be done
// manually
struct ir_basicblock *
ir_insert_basicblocks_after_op(struct ir_func *irb, struct ir_op *insert_after,
                               struct ir_basicblock *first);

// we should probably make this implicitly called by below functions
void ir_remove_basicblock_successors(struct ir_basicblock *basicblock);

void ir_make_basicblock_split(struct ir_func *irb,
                              struct ir_basicblock *basicblock,
                              struct ir_basicblock *true_target,
                              struct ir_basicblock *false_target);

void ir_make_basicblock_merge(struct ir_func *irb,
                              struct ir_basicblock *basicblock,
                              struct ir_basicblock *target);

void ir_make_basicblock_switch(struct ir_func *irb,
                               struct ir_basicblock *basicblock,
                               size_t num_cases, struct ir_split_case *cases,
                               struct ir_basicblock *default_target);

enum ir_detach_ir_basicblock_flags {
  DETACH_IR_BASICBLOCK_FLAG_NONE = 0,
  DETACH_IR_BASICBLOCK_FLAG_ALLOW_PREDS = 1,
};

void ir_detach_basicblock(struct ir_func *irb, struct ir_basicblock *basicblock,
                          enum ir_detach_ir_basicblock_flags flags);

void ir_detach_stmt(struct ir_func *irb, struct ir_stmt *stmt);
void ir_detach_op(struct ir_func *irb, struct ir_op *op);

void ir_initialise_basicblock(struct ir_basicblock *basicblock, size_t id);

// Helper method that ensures the essential fields in IR op are initialised
void ir_initialise_stmt(struct ir_stmt *stmt, size_t id);

void ir_initialise_op(struct ir_op *op, size_t id, enum ir_op_ty ty,
                      struct ir_var_ty var_ty, struct ir_reg,
                      struct ir_lcl *lcl);

void ir_insert_basicblock_chain(struct ir_func *irb,
                                struct ir_basicblock *chain,
                                struct ir_basicblock *insert_after,
                                struct ir_basicblock *first_succ);

void ir_move_after_basicblock(struct ir_func *irb,
                              struct ir_basicblock *basicblock,
                              struct ir_basicblock *move_after);
void ir_move_before_basicblock(struct ir_func *irb,
                               struct ir_basicblock *basicblock,
                               struct ir_basicblock *move_before);

void ir_move_after_stmt(struct ir_func *irb, struct ir_stmt *stmt,
                        struct ir_stmt *move_after);
void ir_move_before_stmt(struct ir_func *irb, struct ir_stmt *stmt,
                         struct ir_stmt *move_before);

void ir_move_after_op(struct ir_func *irb, struct ir_op *op,
                      struct ir_op *move_after);
void ir_move_before_op(struct ir_func *irb, struct ir_op *op,
                       struct ir_op *move_before);

void ir_attach_basicblock(struct ir_func *irb, struct ir_basicblock *basicblock,
                          struct ir_basicblock *pred,
                          struct ir_basicblock *succ);

void ir_attach_stmt(struct ir_func *irb, struct ir_stmt *stmt,
                    struct ir_basicblock *basicblock, struct ir_stmt *pred,
                    struct ir_stmt *succ);

void ir_attach_op(struct ir_func *irb, struct ir_op *op, struct ir_stmt *stmt,
                  struct ir_op *pred, struct ir_op *succ);

// swaps ops but does NOT swap their uses - expressions pointing to `left` will
// now point to `right`
void ir_swap_ops(struct ir_func *irb, struct ir_op *left, struct ir_op *right);

// swaps ops AND their uses
void ir_swap_ops_in_place(struct ir_func *irb, struct ir_op *left,
                          struct ir_op *right);

struct ir_op *ir_replace_op(struct ir_func *irb, struct ir_op *op,
                            enum ir_op_ty ty, struct ir_var_ty var_ty);

struct ir_stmt *ir_insert_before_stmt(struct ir_func *irb,
                                      struct ir_stmt *insert_before);

struct ir_stmt *ir_insert_after_stmt(struct ir_func *irb,
                                     struct ir_stmt *insert_after);

struct ir_basicblock *
ir_insert_before_basicblock(struct ir_func *irb,
                            struct ir_basicblock *insert_before);

struct ir_basicblock *
ir_insert_after_basicblock(struct ir_func *irb,
                           struct ir_basicblock *insert_after);

struct ir_op *ir_insert_before_op(struct ir_func *irb,
                                  struct ir_op *insert_before, enum ir_op_ty ty,
                                  struct ir_var_ty var_ty);

struct ir_op *ir_insert_after_op(struct ir_func *irb,
                                 struct ir_op *insert_after, enum ir_op_ty ty,
                                 struct ir_var_ty var_ty);

struct ir_op *ir_append_op(struct ir_func *irb, struct ir_stmt *stmt,
                           enum ir_op_ty ty, struct ir_var_ty var_ty);

struct ir_op *ir_insert_phi(struct ir_func *irb,
                            struct ir_basicblock *basicblock,
                            struct ir_var_ty var_ty);

bool ir_basicblock_is_pred(struct ir_basicblock *basicblock,
                           struct ir_basicblock *pred);

struct ir_field_info {
  struct ir_var_ty var_ty;
  size_t offset;
};

struct ir_var_ty_flattened {
  struct ir_field_info *fields;
  size_t num_fields;
};

struct ir_var_ty_flattened ir_var_ty_info_flat(struct ir_unit *iru,
                                               const struct ir_var_ty *ty);

struct ir_var_ty_info {
  size_t size;
  size_t alignment;

  size_t num_fields;
  size_t *offsets;
};

struct ir_var_ty_info ir_var_ty_info(struct ir_unit *iru,
                                     const struct ir_var_ty *ty);

enum ir_var_primitive_ty ir_var_ty_pointer_primitive_ty(struct ir_unit *iru);

struct ir_var_ty ir_var_ty_for_pointer_size(struct ir_unit *iru);
struct ir_var_ty ir_var_ty_make_array(struct ir_unit *iru,
                                      const struct ir_var_ty *underlying,
                                      size_t num_elements);

bool ir_var_ty_is_primitive(const struct ir_var_ty *var_ty,
                            enum ir_var_primitive_ty primitive);
bool ir_var_ty_is_integral(const struct ir_var_ty *var_ty);
bool ir_var_ty_is_fp(const struct ir_var_ty *var_ty);
bool ir_var_ty_is_aggregate(const struct ir_var_ty *var_ty);

bool ir_var_ty_eq(struct ir_unit *iru, const struct ir_var_ty *l,
                  const struct ir_var_ty *r);

struct ir_op *ir_spill_op(struct ir_func *irb, struct ir_op *op);

struct ir_op_use {
  enum ir_op_use_ty ty;

  struct ir_op *consumer;
  struct ir_op **op;
};

struct ir_op_usage {
  struct ir_op *op;

  struct ir_op_use *uses;
  size_t num_uses;
};

struct ir_lcl_usage {
  struct ir_lcl *lcl;

  struct ir_op **consumers;
  size_t num_consumers;
};

struct ir_op_use_map {
  struct ir_op_usage *op_use_datas;
  size_t num_op_use_datas;

  struct ir_lcl_usage *lcl_use_datas;
  size_t num_lcl_use_datas;
};

// struct ir_rewrite_op {
//   struct ir_op *op;
//   struct ir_op *new;
// };

// struct ir_rewrite_info {
//   struct ir_op *
// };

struct ir_op_use_map ir_build_op_uses_map(struct ir_func *func);

size_t ir_unique_idx_for_reg(struct ir_reg reg);
struct ir_reg ir_reg_for_unique_idx(size_t idx);

struct ir_dominance_frontier {
  // FIXME: exposing vector in API ugly
  struct ir_basicblock **idoms;
  struct vector /* `struct ir_basicblock *` */ **idom_children;
  struct vector /* `struct ir_basicblock *` */ **domfs;
  struct vector /* `struct ir_basicblock *` */ **dom_trees;
};

struct ir_dominance_frontier
ir_compute_dominance_frontier(struct ir_func *func);

void ir_simplify_phis(struct ir_func *func);

// TODO: this should really be somewhere else

struct location {
  size_t idx;
  void *metadata[2];
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
