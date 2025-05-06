#include "interp.h"

#include "../alloc.h"
#include "../hashtbl.h"
#include "../log.h"
#include "../vector.h"
#include "ir.h"
#include "prettyprint.h"

#include <stddef.h>

struct ir_interp_glb_data {
  char *data;
  size_t len;
};

struct ir_interp {
  struct arena_allocator *arena;

  struct ir_unit *unit;
  struct ir_glb *entrypoint;

  // linearised globals for lookup
  struct ir_glb **glbs;
  struct ir_interp_glb_data *glb_data;
  size_t num_glbs;

  struct vector *stack_frames;
};

typedef bool i1;
typedef uint8_t i8;
typedef uint16_t i16;
typedef uint32_t i32;
typedef uint64_t i64;
typedef uint128_t i128;

typedef int8_t s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;
typedef int128_t s128;

typedef __fp16 f16;
typedef float f32;
typedef double f64;

typedef size_t ptr_t;

enum ir_interp_ptr_ty {
  IR_INTERP_PTR_TY_LCL,
  IR_INTERP_PTR_TY_GLB,
  IR_INTERP_PTR_TY_ALLOC,
};

struct ir_interp_ptr {
  enum ir_interp_ptr_ty ty : 2;
  i32 idx : 31;
  i32 offset : 31;
};

static struct ir_interp_ptr ptr_from_addr(ptr_t addr) {
  static_assert(sizeof(ptr_t) == 8);

  enum ir_interp_ptr_ty ty = addr >> 62;

  i32 idx = addr & 0x7FFFFFFF;
  i32 offset = (addr >> 31) & 0x7FFFFFFF;

  return (struct ir_interp_ptr){.ty = ty, .idx = idx, .offset = offset};
}

static ptr_t addr_from_ptr(struct ir_interp_ptr ptr) {
  return (ptr_t)ptr.ty << 62 | (ptr.offset << 31) | ptr.idx;
}

enum ir_slot_ty {
  IR_SLOT_TY_I1 = IR_VAR_PRIMITIVE_TY_I1,
  IR_SLOT_TY_I8 = IR_VAR_PRIMITIVE_TY_I8,
  IR_SLOT_TY_I16 = IR_VAR_PRIMITIVE_TY_I16,
  IR_SLOT_TY_I32 = IR_VAR_PRIMITIVE_TY_I32,
  IR_SLOT_TY_I64 = IR_VAR_PRIMITIVE_TY_I64,
  IR_SLOT_TY_I128 = IR_VAR_PRIMITIVE_TY_I128,

  IR_SLOT_TY_F16 = IR_VAR_PRIMITIVE_TY_F16,
  IR_SLOT_TY_F32 = IR_VAR_PRIMITIVE_TY_F32,
  IR_SLOT_TY_F64 = IR_VAR_PRIMITIVE_TY_F64,

  IR_SLOT_TY_PTR,
  IR_SLOT_TY_AGGREGATE,
};

struct ir_ssa_slot {
  struct ir_op *op;

  enum ir_slot_ty ty;
  union {
    i1 i1;
    i8 i8;
    i16 i16;
    i32 i32;
    i64 i64;
    i128 i128;

    f16 f16;
    f32 f32;
    f64 f64;

    ptr_t ptr;

    struct {
      void *data;
      size_t len;
    } aggregate;

    struct {
      char buf[16];
    } buf;
  };
};

#define LCL_COMPRESS_SZ 16

struct ir_lcl_slot {
  struct ir_lcl *lcl;

  size_t len;

  // helps us not allocate for the general case
  char buf[LCL_COMPRESS_SZ];
  char *data;
};

#define LCL_COMPRESSED(lcl) ((lcl)->len <= LCL_COMPRESS_SZ)

static_assert(sizeof(((struct ir_ssa_slot){0}).buf) ==
                  sizeof(struct ir_ssa_slot) -
                      offsetof(struct ir_ssa_slot, buf),
              "buf too small to cover whole union");

struct ir_stack_frame {
  struct ir_func *func;
  struct ir_ssa_slot *ret_slot;
  bool ret_exec;

  struct ir_ssa_slot *args;
  size_t num_args;
  size_t num_used;

  struct ir_lcl_slot *lcls;
  struct ir_ssa_slot *ssas;

  struct ir_basicblock *pred;
  struct ir_basicblock *cur_basicblock;
};

static struct ir_stack_frame *ir_new_stack_frame(struct ir_interp *interp,
                                                 struct ir_func *func,
                                                 struct ir_ssa_slot *args,
                                                 size_t num_args,
                                                 struct ir_ssa_slot *ret_slot) {
  struct ir_stack_frame *stack_frame =
      vector_push_back(interp->stack_frames, NULL);

  *stack_frame = (struct ir_stack_frame){
      .func = func,

      .args = args,
      .num_args = num_args,
      .num_used = 0,

      .ret_slot = ret_slot,
      .ret_exec = false,

      .lcls =
          arzalloc(interp->arena, sizeof(*stack_frame->lcls) * func->lcl_count),
      .ssas =
          arzalloc(interp->arena, sizeof(*stack_frame->ssas) * func->op_count),
  };

  return stack_frame;
}

static void ir_diagnostic(UNUSED struct ir_interp *interp,
                          struct ir_stack_frame *stack_frame, const char *msg) {

  warn("ir_interp: in func %s: %s", stack_frame->func->name, msg);
}

#define INTERP_WARN(...) ir_diagnostic(interp, stack_frame, __VA_ARGS__)
#define INTERP_ERR(...)                                                        \
  do {                                                                         \
    ir_diagnostic(interp, stack_frame, __VA_ARGS__);                           \
    return;                                                                    \
  } while (0)

static void ir_write_ssa_i1(struct ir_ssa_slot *slot, i1 value) {
  slot->ty = IR_SLOT_TY_I1;
  slot->i1 = value;
}
static void ir_write_ssa_i8(struct ir_ssa_slot *slot, i8 value) {
  slot->ty = IR_SLOT_TY_I8;
  slot->i8 = value;
}
static void ir_write_ssa_i16(struct ir_ssa_slot *slot, i16 value) {
  slot->ty = IR_SLOT_TY_I16;
  slot->i16 = value;
}
static void ir_write_ssa_i32(struct ir_ssa_slot *slot, i32 value) {
  slot->ty = IR_SLOT_TY_I32;
  slot->i32 = value;
}
static void ir_write_ssa_i64(struct ir_ssa_slot *slot, i64 value) {
  slot->ty = IR_SLOT_TY_I64;
  slot->i64 = value;
}
static void ir_write_ssa_i128(struct ir_ssa_slot *slot, i128 value) {
  slot->ty = IR_SLOT_TY_I128;
  slot->i128 = value;
}

static void ir_write_ssa_f16(struct ir_ssa_slot *slot, f16 value) {
  slot->ty = IR_SLOT_TY_F16;
  slot->f16 = value;
}
static void ir_write_ssa_f32(struct ir_ssa_slot *slot, f32 value) {
  slot->ty = IR_SLOT_TY_F32;
  slot->f32 = value;
}
static void ir_write_ssa_f64(struct ir_ssa_slot *slot, f64 value) {
  slot->ty = IR_SLOT_TY_F64;
  slot->f64 = value;
}

static void ir_write_ssa_ptr(struct ir_ssa_slot *slot, ptr_t value) {
  slot->ty = IR_SLOT_TY_PTR;
  slot->ptr = value;
}

#define INVALID_BYTE (0xbe)
static void ir_write_ssa_invalid(struct ir_ssa_slot *slot) {
  memset(&slot->buf, INVALID_BYTE, sizeof(slot->buf));
}

#define ir_write_ssa_slot(slot, value)                                         \
  _Generic((value),                                                            \
      i1: ir_write_ssa_i1,                                                     \
      i8: ir_write_ssa_i8,                                                     \
      i16: ir_write_ssa_i16,                                                   \
      i32: ir_write_ssa_i32,                                                   \
      i64: ir_write_ssa_i64,                                                   \
      i128: ir_write_ssa_i128,                                                 \
      f16: ir_write_ssa_f16,                                                   \
      f32: ir_write_ssa_f32,                                                   \
      f64: ir_write_ssa_f64,                                                   \
      void *: ir_write_ssa_ptr)((slot), (value))

static void ir_write_slot_int(struct ir_ssa_slot *slot, enum ir_slot_ty ty,
                              i64 value) {
  slot->ty = ty;
  slot->i64 = value;
}

static void ir_copy_slot(struct ir_ssa_slot *dest,
                         const struct ir_ssa_slot *src) {
  dest->ty = src->ty;
  dest->buf = src->buf;
}

static s64 ir_read_slot_int_signed(const struct ir_ssa_slot *slot) {
  switch (slot->ty) {
  case IR_SLOT_TY_I1:
    return slot->i1;
  case IR_SLOT_TY_I8:
    return (s8)slot->i8;
  case IR_SLOT_TY_I16:
    return (s16)slot->i16;
  case IR_SLOT_TY_I32:
    return (s32)slot->i32;
  case IR_SLOT_TY_I64:
    return (s64)slot->i64;
  case IR_SLOT_TY_I128:
    TODO("128");
    // return slot->i128;
  case IR_SLOT_TY_PTR:
    return slot->ptr;
  case IR_SLOT_TY_AGGREGATE:
  case IR_SLOT_TY_F16:
  case IR_SLOT_TY_F32:
  case IR_SLOT_TY_F64:
    BUG("not valid");
  }
}

static enum ir_slot_ty slot_for_var_ty(const struct ir_var_ty *var_ty) {
  if (var_ty->ty != IR_VAR_TY_TY_PRIMITIVE) {
    TODO("non prims");
  }

  switch (var_ty->primitive) {
  case IR_VAR_PRIMITIVE_TY_I1:
    return IR_SLOT_TY_I1;
  case IR_VAR_PRIMITIVE_TY_I8:
    return IR_SLOT_TY_I8;
  case IR_VAR_PRIMITIVE_TY_I16:
    return IR_SLOT_TY_I16;
  case IR_VAR_PRIMITIVE_TY_I32:
    return IR_SLOT_TY_I32;
  case IR_VAR_PRIMITIVE_TY_I64:
    return IR_SLOT_TY_I64;
  case IR_VAR_PRIMITIVE_TY_I128:
    return IR_SLOT_TY_I128;
  case IR_VAR_PRIMITIVE_TY_F16:
    return IR_SLOT_TY_F16;
  case IR_VAR_PRIMITIVE_TY_F32:
    return IR_SLOT_TY_F32;
  case IR_VAR_PRIMITIVE_TY_F64:
    return IR_SLOT_TY_F64;
  }
}

static void ir_exec_op_unknown(UNUSED struct ir_interp *interp,
                               UNUSED struct ir_stack_frame *stack_frame,
                               UNUSED struct ir_op *op) {
  BUG("unknown op");
}

static void ir_exec_op_phi(UNUSED struct ir_interp *interp,
                           struct ir_stack_frame *stack_frame,
                           struct ir_op *op) {
  struct ir_op_phi *phi = &op->phi;
  struct ir_ssa_slot *slot = &stack_frame->ssas[op->id];

  for (size_t i = 0; i < phi->num_values; i++) {
    if (phi->values[i].basicblock == stack_frame->cur_basicblock) {
      struct ir_ssa_slot *value = &stack_frame->ssas[phi->values[i].value->id];
      ir_copy_slot(slot, value);
    }
  }
}

static void ir_exec_op_undf(UNUSED struct ir_interp *interp,
                            struct ir_stack_frame *stack_frame,
                            struct ir_op *op) {
  struct ir_ssa_slot *slot = &stack_frame->ssas[op->id];
  ir_write_ssa_invalid(slot);
}

static void ir_exec_op_mov(UNUSED struct ir_interp *interp,
                           struct ir_stack_frame *stack_frame,
                           struct ir_op *op) {
  struct ir_op_mov *mov = &op->mov;
  struct ir_ssa_slot *slot = &stack_frame->ssas[op->id];

  if (op->flags & IR_OP_FLAG_PARAM) {
    DEBUG_ASSERT(stack_frame->num_used < stack_frame->num_args,
                 "ran out of args");

    ir_copy_slot(slot, &stack_frame->args[stack_frame->num_used++]);
    return;
  }

  ir_copy_slot(slot, &stack_frame->ssas[mov->value->id]);
}

static void ir_exec_op_cnst(struct ir_interp *interp,
                            struct ir_stack_frame *stack_frame,
                            struct ir_op *op) {
  struct ir_var_ty *var_ty = &op->var_ty;
  struct ir_op_cnst *cnst = &op->cnst;
  struct ir_ssa_slot *slot = &stack_frame->ssas[op->id];

  switch (cnst->ty) {
  case IR_OP_CNST_TY_FLT:
    if (!ir_var_ty_is_fp(var_ty)) {
      ir_diagnostic(interp, stack_frame,
                    "expected float-type cnst op to have fp var ty");

      ir_write_ssa_invalid(slot);
      return;
    }

    switch (var_ty->primitive) {
    case IR_VAR_PRIMITIVE_TY_F16:
      ir_write_ssa_slot(slot, (f16)cnst->flt_value);
      break;
    case IR_VAR_PRIMITIVE_TY_F32:
      ir_write_ssa_slot(slot, (f32)cnst->flt_value);
      break;
    case IR_VAR_PRIMITIVE_TY_F64:
      ir_write_ssa_slot(slot, (f64)cnst->flt_value);
      break;
    default:
      unreachable();
    }
    break;
  case IR_OP_CNST_TY_INT:
    if (!ir_var_ty_is_integral(var_ty)) {
      ir_diagnostic(interp, stack_frame,
                    "expected int-type cnst op to have integral var ty");

      ir_write_ssa_invalid(slot);
      return;
    }

    switch (var_ty->primitive) {
    case IR_VAR_PRIMITIVE_TY_I1:
      ir_write_ssa_slot(slot, (i1)cnst->int_value);
      break;
    case IR_VAR_PRIMITIVE_TY_I8:
      ir_write_ssa_slot(slot, (i8)cnst->int_value);
      break;
    case IR_VAR_PRIMITIVE_TY_I16:
      ir_write_ssa_slot(slot, (i16)cnst->int_value);
      break;
    case IR_VAR_PRIMITIVE_TY_I32:
      ir_write_ssa_slot(slot, (i32)cnst->int_value);
      break;
    case IR_VAR_PRIMITIVE_TY_I64:
      ir_write_ssa_slot(slot, (i64)cnst->int_value);
      break;
    case IR_VAR_PRIMITIVE_TY_I128:
      ir_write_ssa_slot(slot, (i128)cnst->int_value);
      break;
    default:
      unreachable();
    }
  }
}

static void ir_exec_op_unary_op(struct ir_interp *interp,
                                struct ir_stack_frame *stack_frame,
                                struct ir_op *op);

static i64 ir_read_slot_int_unsigned(const struct ir_ssa_slot *slot) {
  switch (slot->ty) {
  case IR_SLOT_TY_I1:
    return slot->i1;
  case IR_SLOT_TY_I8:
    return slot->i8;
  case IR_SLOT_TY_I16:
    return slot->i16;
  case IR_SLOT_TY_I32:
    return slot->i32;
  case IR_SLOT_TY_I64:
    return slot->i64;
  case IR_SLOT_TY_I128:
    TODO("128");
    // return slot->i128;
  case IR_SLOT_TY_PTR:
    return slot->ptr;
  case IR_SLOT_TY_AGGREGATE:
  case IR_SLOT_TY_F16:
  case IR_SLOT_TY_F32:
  case IR_SLOT_TY_F64:
    BUG("not valid");
  }
}

static void ir_exec_op_cast_op(struct ir_interp *interp,
                               struct ir_stack_frame *stack_frame,
                               struct ir_op *op) {
  struct ir_var_ty *var_ty = &op->var_ty;
  struct ir_op_cast_op *cast_op = &op->cast_op;
  struct ir_var_ty *from_var_ty = &cast_op->value->var_ty;

  struct ir_ssa_slot *source = &stack_frame->ssas[cast_op->value->id];
  struct ir_ssa_slot *dest = &stack_frame->ssas[op->id];

  DEBUG_ASSERT(var_ty->primitive != IR_VAR_PRIMITIVE_TY_I128 &&
                   from_var_ty->primitive != IR_VAR_PRIMITIVE_TY_I128,
               "TODO: i128");

  switch (cast_op->ty) {
  case IR_OP_CAST_OP_TY_SEXT: {
    s64 value = ir_read_slot_int_signed(source);

    enum ir_slot_ty slot_ty = slot_for_var_ty(var_ty);
    ir_write_slot_int(dest, slot_ty, value);
    break;
  }
  case IR_OP_CAST_OP_TY_ZEXT: {
    i64 value = ir_read_slot_int_unsigned(source);

    enum ir_slot_ty slot_ty = slot_for_var_ty(var_ty);
    ir_write_slot_int(dest, slot_ty, value);
    break;
  }
  case IR_OP_CAST_OP_TY_TRUNC: {
    // we are truncating so its safe to read as unsigned
    i64 value = ir_read_slot_int_unsigned(source);

    switch (var_ty->primitive) {
    case IR_VAR_PRIMITIVE_TY_I1:
      value &= (1ul << 1) - 1;
      break;
    case IR_VAR_PRIMITIVE_TY_I8:
      value &= (1ul << 8) - 1;
      break;
    case IR_VAR_PRIMITIVE_TY_I16:
      value &= (1ul << 1) - 1;
      break;
    case IR_VAR_PRIMITIVE_TY_I32:
      value &= (1ul << 32) - 1;
      break;
    case IR_VAR_PRIMITIVE_TY_I64:
      // FIXME: when add 128, add trunc here
      value &= ((i128)1ul << 64) - 1;
      break;
    case IR_VAR_PRIMITIVE_TY_I128:
      TODO("i128");
      // value &= (1ul << 128) - 1;
    default:
      unreachable();
    }

    enum ir_slot_ty slot_ty = slot_for_var_ty(var_ty);
    ir_write_slot_int(dest, slot_ty, value);
    break;
  }
  case IR_OP_CAST_OP_TY_CONV: {
    switch (from_var_ty->primitive) {
    case IR_VAR_PRIMITIVE_TY_F16: {
      switch (var_ty->primitive) {
      case IR_VAR_PRIMITIVE_TY_F16:
        INTERP_ERR("same type cast");
        return;
      case IR_VAR_PRIMITIVE_TY_F32:
        ir_write_ssa_slot(dest, (f32)source->f16);
        break;
      case IR_VAR_PRIMITIVE_TY_F64:
        ir_write_ssa_slot(dest, (f64)source->f16);
        break;
      default:
        unreachable();
      }
      break;
    }
    case IR_VAR_PRIMITIVE_TY_F32: {
      switch (var_ty->primitive) {
      case IR_VAR_PRIMITIVE_TY_F16:
        ir_write_ssa_slot(dest, (f16)source->f32);
        break;
      case IR_VAR_PRIMITIVE_TY_F32:
        INTERP_ERR("same type cast");
        return;
      case IR_VAR_PRIMITIVE_TY_F64:
        ir_write_ssa_slot(dest, (f64)source->f32);
        break;
      default:
        unreachable();
      }
      break;
    }
    case IR_VAR_PRIMITIVE_TY_F64: {
      switch (var_ty->primitive) {
      case IR_VAR_PRIMITIVE_TY_F16:
        ir_write_ssa_slot(dest, (f16)source->f64);
        break;
      case IR_VAR_PRIMITIVE_TY_F32:
        ir_write_ssa_slot(dest, (f32)source->f64);
        break;
      case IR_VAR_PRIMITIVE_TY_F64:
        INTERP_ERR("same type cast");
        return;
      default:
        unreachable();
      }
      break;
    }
    default:
      unreachable();
    }
    break;
  }
  case IR_OP_CAST_OP_TY_UCONV: {
    switch (from_var_ty->primitive) {
    case IR_VAR_PRIMITIVE_TY_I32:
      switch (var_ty->primitive) {
      case IR_VAR_PRIMITIVE_TY_F16:
        ir_write_ssa_slot(dest, (f16)source->i32);
        break;
      case IR_VAR_PRIMITIVE_TY_F32:
        ir_write_ssa_slot(dest, (f32)source->i32);
        break;
      case IR_VAR_PRIMITIVE_TY_F64:
        ir_write_ssa_slot(dest, (f64)source->i32);
        break;
      default:
        unreachable();
      }
      break;
    case IR_VAR_PRIMITIVE_TY_I64:
      switch (var_ty->primitive) {
      case IR_VAR_PRIMITIVE_TY_F16:
        ir_write_ssa_slot(dest, (f16)source->i64);
        break;
      case IR_VAR_PRIMITIVE_TY_F32:
        ir_write_ssa_slot(dest, (f32)source->i64);
        break;
      case IR_VAR_PRIMITIVE_TY_F64:
        ir_write_ssa_slot(dest, (f64)source->i64);
        break;
      default:
        unreachable();
      }
      break;
    case IR_VAR_PRIMITIVE_TY_I128:
      TODO("i128");
    case IR_VAR_PRIMITIVE_TY_F16:
      switch (var_ty->primitive) {
      case IR_VAR_PRIMITIVE_TY_I32:
        ir_write_ssa_slot(dest, (i32)source->f16);
        break;
      case IR_VAR_PRIMITIVE_TY_I64:
        ir_write_ssa_slot(dest, (i64)source->f16);
        break;
      default:
        unreachable();
      }
      break;
    case IR_VAR_PRIMITIVE_TY_F32:
      switch (var_ty->primitive) {
      case IR_VAR_PRIMITIVE_TY_I32:
        ir_write_ssa_slot(dest, (i32)source->f32);
        break;
      case IR_VAR_PRIMITIVE_TY_I64:
        ir_write_ssa_slot(dest, (i64)source->f32);
        break;
      default:
        unreachable();
      }
      break;
    case IR_VAR_PRIMITIVE_TY_F64:
      switch (var_ty->primitive) {
      case IR_VAR_PRIMITIVE_TY_I32:
        ir_write_ssa_slot(dest, (i32)source->f64);
        break;
      case IR_VAR_PRIMITIVE_TY_I64:
        ir_write_ssa_slot(dest, (i64)source->f64);
        break;
      default:
        unreachable();
      }
      break;
    default:
      unreachable();
    }
    break;
  }
  case IR_OP_CAST_OP_TY_SCONV: {
    switch (from_var_ty->primitive) {
    case IR_VAR_PRIMITIVE_TY_I32:
      switch (var_ty->primitive) {
      case IR_VAR_PRIMITIVE_TY_F16:
        ir_write_ssa_slot(dest, (f16)(s32)source->i32);
        break;
      case IR_VAR_PRIMITIVE_TY_F32:
        ir_write_ssa_slot(dest, (f32)(s32)source->i32);
        break;
      case IR_VAR_PRIMITIVE_TY_F64:
        ir_write_ssa_slot(dest, (f64)(s32)source->i32);
        break;
      default:
        unreachable();
      }
      break;
    case IR_VAR_PRIMITIVE_TY_I64:
      switch (var_ty->primitive) {
      case IR_VAR_PRIMITIVE_TY_F16:
        ir_write_ssa_slot(dest, (f16)(s64)source->i64);
        break;
      case IR_VAR_PRIMITIVE_TY_F32:
        ir_write_ssa_slot(dest, (f32)(s64)source->i64);
        break;
      case IR_VAR_PRIMITIVE_TY_F64:
        ir_write_ssa_slot(dest, (f64)(s64)source->i64);
        break;
      default:
        unreachable();
      }
      break;
    case IR_VAR_PRIMITIVE_TY_I128:
      TODO("i128");
    case IR_VAR_PRIMITIVE_TY_F16:
      switch (var_ty->primitive) {
      case IR_VAR_PRIMITIVE_TY_I32:
        ir_write_ssa_slot(dest, (i32)(s32)source->f16);
        break;
      case IR_VAR_PRIMITIVE_TY_I64:
        ir_write_ssa_slot(dest, (i64)(s64)source->f16);
        break;
      default:
        unreachable();
      }
      break;
    case IR_VAR_PRIMITIVE_TY_F32:
      switch (var_ty->primitive) {
      case IR_VAR_PRIMITIVE_TY_I32:
        ir_write_ssa_slot(dest, (i32)(s32)source->f32);
        break;
      case IR_VAR_PRIMITIVE_TY_I64:
        ir_write_ssa_slot(dest, (i64)(s64)source->f32);
        break;
      default:
        unreachable();
      }
      break;
    case IR_VAR_PRIMITIVE_TY_F64:
      switch (var_ty->primitive) {
      case IR_VAR_PRIMITIVE_TY_I32:
        ir_write_ssa_slot(dest, (i32)(s32)source->f64);
        break;
      case IR_VAR_PRIMITIVE_TY_I64:
        ir_write_ssa_slot(dest, (i64)(s64)source->f64);
        break;
      default:
        unreachable();
      }
      break;
    default:
      unreachable();
    }
  }
  }
}

static bool ir_eval_cond(const struct ir_ssa_slot *slot) {
  switch (slot->ty) {
  case IR_SLOT_TY_I1:
    return (bool)slot->i1;
  case IR_SLOT_TY_I8:
    return (bool)slot->i8;
  case IR_SLOT_TY_I16:
    return (bool)slot->i16;
  case IR_SLOT_TY_I32:
    return (bool)slot->i32;
  case IR_SLOT_TY_I64:
    return (bool)slot->i64;
  case IR_SLOT_TY_I128:
    return (bool)slot->i128;
  case IR_SLOT_TY_F16:
    return (bool)slot->f16;
  case IR_SLOT_TY_F32:
    return (bool)slot->f32;
  case IR_SLOT_TY_F64:
    return (bool)slot->f64;
  case IR_SLOT_TY_PTR:
    return (bool)slot->ptr;
  case IR_SLOT_TY_AGGREGATE:
    BUG("emit diag");
  }
}

static void ir_exec_op_select(UNUSED struct ir_interp *interp,
                              struct ir_stack_frame *stack_frame,
                              struct ir_op *op) {
  struct ir_op_select *select = &op->select;

  struct ir_ssa_slot *cond = &stack_frame->ssas[select->cond->id];
  struct ir_ssa_slot *true_op = &stack_frame->ssas[select->true_op->id];
  struct ir_ssa_slot *false_op = &stack_frame->ssas[select->false_op->id];

  struct ir_ssa_slot *slot = &stack_frame->ssas[op->id];

  if (ir_eval_cond(cond)) {
    ir_copy_slot(slot, true_op);
  } else {
    ir_copy_slot(slot, false_op);
  }
}

enum ir_get_raw_addr_mode {
  IR_GET_RAW_ADDR_MODE_READ,
  IR_GET_RAW_ADDR_MODE_WRITE,
};

// gives us the actual pointer we can write or read from, or NULL on failure
static void *ir_get_raw_addr(struct ir_interp *interp,
                             struct ir_stack_frame *stack_frame,
                             struct ir_interp_ptr ptr, size_t size,
                             UNUSED enum ir_get_raw_addr_mode mode) {
  switch (ptr.ty) {
  case IR_INTERP_PTR_TY_LCL: {
    struct ir_lcl_slot *lcl = &stack_frame->lcls[ptr.idx];

    size_t len = ptr.offset > lcl->len ? 0 : lcl->len - ptr.offset;
    if (len > size) {
      INTERP_WARN("out-of-bounds lcl read");
      return NULL;
    }

    if (LCL_COMPRESSED(lcl)) {
      return &lcl->buf[ptr.offset];
    } else {
      return &lcl->data[ptr.offset];
    }
  }
  case IR_INTERP_PTR_TY_GLB: {
    struct ir_interp_glb_data *glb = &interp->glb_data[ptr.idx];

    size_t len = ptr.offset > glb->len ? 0 : glb->len - ptr.offset;
    if (len > size) {
      INTERP_WARN("out-of-bounds glb read");
      return NULL;
    }

    return &glb->data[ptr.offset];
  }
  case IR_INTERP_PTR_TY_ALLOC: {
    TODO("alloc addrs");
  }
  }
}

#define LCL_PTR(p)                                                             \
  ((struct ir_interp_ptr){.ty = IR_INTERP_PTR_TY_LCL, .idx = (p->id)})
#define GLB_PTR(p)                                                             \
  ((struct ir_interp_ptr){.ty = IR_INTERP_PTR_TY_GLB, .idx = (p->id)})

static void ir_exec_op_store(struct ir_interp *interp,
                             struct ir_stack_frame *stack_frame,
                             struct ir_op *op) {
  struct ir_op_store *store = &op->store;

  struct ir_ssa_slot *value = &stack_frame->ssas[store->value->id];

  struct ir_interp_ptr ptr;

  switch (store->ty) {
  case IR_OP_STORE_TY_LCL:
    ptr = LCL_PTR(store->lcl);
    break;
  case IR_OP_STORE_TY_GLB:
    ptr = GLB_PTR(store->glb);
    break;
  case IR_OP_STORE_TY_ADDR: {
    struct ir_ssa_slot *addr = &stack_frame->ssas[store->addr->id];

    if (addr->ty != IR_SLOT_TY_PTR) {
      ir_diagnostic(interp, stack_frame, "expected ptr for store");
      return;
    }

    ptr = ptr_from_addr(addr->ptr);
  }
  }

  size_t size = ir_var_ty_info(interp->unit, &store->value->var_ty).size;
  char *p = ir_get_raw_addr(interp, stack_frame, ptr, size,
                            IR_GET_RAW_ADDR_MODE_WRITE);

  if (!p) {
    return;
  }

  PUSH_NO_WARN("-Wcast-align");

  switch (value->ty) {
  case IR_SLOT_TY_I1:
    *p = value->i1;
    break;
  case IR_SLOT_TY_I8:
    *p = value->i8;
    break;
  case IR_SLOT_TY_I16:
    *(i16 *)p = value->i16;
    break;
  case IR_SLOT_TY_I32:
    *(i32 *)p = value->i32;
    break;
  case IR_SLOT_TY_I64:
    *(i64 *)p = value->i64;
    break;
  case IR_SLOT_TY_I128:
    *(i128 *)p = value->i128;
    break;
  case IR_SLOT_TY_F16:
    *(f16 *)p = value->f16;
    break;
  case IR_SLOT_TY_F32:
    *(f32 *)p = value->f32;
    break;
  case IR_SLOT_TY_F64:
    *(f64 *)p = value->f64;
    break;
  case IR_SLOT_TY_PTR:
    *(ptr_t *)p = value->ptr;
    break;
  case IR_SLOT_TY_AGGREGATE:
    memcpy(p, value->aggregate.data, value->aggregate.len);
    break;
  }

  POP_NO_WARN();
}

static void ir_exec_op_load(struct ir_interp *interp,
                            struct ir_stack_frame *stack_frame,
                            struct ir_op *op) {
  struct ir_var_ty *var_ty = &op->var_ty;
  struct ir_op_load *load = &op->load;
  struct ir_ssa_slot *slot = &stack_frame->ssas[op->id];

  struct ir_interp_ptr ptr;
  switch (load->ty) {
  case IR_OP_LOAD_TY_LCL:
    ptr = LCL_PTR(load->lcl);
    break;
  case IR_OP_LOAD_TY_GLB:
    ptr = GLB_PTR(load->glb);
    break;
  case IR_OP_LOAD_TY_ADDR: {
    struct ir_ssa_slot *addr = &stack_frame->ssas[load->addr->id];

    if (addr->ty != IR_SLOT_TY_PTR) {
      ir_diagnostic(interp, stack_frame, "expected ptr for load");
      return;
    }

    ptr = ptr_from_addr(addr->ptr);
  }
  }

  size_t size = ir_var_ty_info(interp->unit, var_ty).size;
  char *p = ir_get_raw_addr(interp, stack_frame, ptr, size,
                            IR_GET_RAW_ADDR_MODE_READ);

  if (!p) {
    return;
  }

  if (var_ty->ty == IR_VAR_TY_TY_PRIMITIVE) {
    switch (var_ty->primitive) {
    case IR_VAR_PRIMITIVE_TY_I1:
      slot->ty = IR_SLOT_TY_I1;
      memcpy(&slot->i1, p, size);
      break;
    case IR_VAR_PRIMITIVE_TY_I8:
      slot->ty = IR_SLOT_TY_I8;
      memcpy(&slot->i8, p, size);
      break;
    case IR_VAR_PRIMITIVE_TY_I16:
      slot->ty = IR_SLOT_TY_I16;
      memcpy(&slot->i16, p, size);
      break;
    case IR_VAR_PRIMITIVE_TY_I32:
      slot->ty = IR_SLOT_TY_I32;
      memcpy(&slot->i32, p, size);
      break;
    case IR_VAR_PRIMITIVE_TY_I64:
      slot->ty = IR_SLOT_TY_I64;
      memcpy(&slot->i64, p, size);
      break;
    case IR_VAR_PRIMITIVE_TY_I128:
      slot->ty = IR_SLOT_TY_I128;
      memcpy(&slot->i128, p, size);
      break;
    case IR_VAR_PRIMITIVE_TY_F16:
      slot->ty = IR_SLOT_TY_F16;
      memcpy(&slot->f16, p, size);
      break;
    case IR_VAR_PRIMITIVE_TY_F32:
      slot->ty = IR_SLOT_TY_F32;
      memcpy(&slot->f32, p, size);
      break;
    case IR_VAR_PRIMITIVE_TY_F64:
      slot->ty = IR_SLOT_TY_F64;
      memcpy(&slot->f64, p, size);
      break;
    }
  } else {
    TODO("non prim load");
  }
}

static void ir_exec_op_store_bitfield(UNUSED struct ir_interp *interp,
                                      UNUSED struct ir_stack_frame *stack_frame,
                                      UNUSED struct ir_op *op) {
  // struct ir_var_ty *var_ty = &op->var_ty;
  // struct ir_op_store_bitfield *store_bitfield = &op->store_bitfield;
  // struct ir_ssa_slot *slot = &stack_frame->ssas[op->id];
  TODO(__func__);
}

static void ir_exec_op_load_bitfield(UNUSED struct ir_interp *interp,
                                     UNUSED struct ir_stack_frame *stack_frame,
                                     UNUSED struct ir_op *op) {
  // struct ir_var_ty *var_ty = &op->var_ty;
  // struct ir_op_load_bitfield *load_bitfield = &op->load_bitfield;
  // struct ir_ssa_slot *slot = &stack_frame->ssas[op->id];
  TODO(__func__);
}

static void ir_exec_op_bitfield_extract(UNUSED struct ir_interp *interp,
                                        struct ir_stack_frame *stack_frame,
                                        struct ir_op *op) {
  struct ir_op_bitfield_extract *bitfield_extract = &op->bitfield_extract;
  struct ir_bitfield bitfield = bitfield_extract->bitfield;

  struct ir_ssa_slot *slot = &stack_frame->ssas[op->id];

  i64 value = ir_read_slot_int_unsigned(
      &stack_frame->ssas[bitfield_extract->value->id]);

  if (bitfield.offset) {
    value >>= bitfield.offset;
  }

  value &= (1ul << bitfield.width) - 1;

  // preserve initial type
  slot->i64 = value;
}

static void
ir_exec_op_bitfield_insert(UNUSED struct ir_interp *interp,
                           UNUSED struct ir_stack_frame *stack_frame,
                           UNUSED struct ir_op *op) {
  // struct ir_var_ty *var_ty = &op->var_ty;
  // struct ir_op_bitfield_insert *bitfield_insert = &op->bitfield_insert;
  // struct ir_ssa_slot *slot = &stack_frame->ssas[op->id];
  TODO(__func__);
}

static void ir_exec_op_mem_set(struct ir_interp *interp,
                               struct ir_stack_frame *stack_frame,
                               struct ir_op *op) {
  struct ir_op_mem_set *mem_set = &op->mem_set;

  size_t size = mem_set->length;
  struct ir_interp_ptr ptr =
      ptr_from_addr(stack_frame->ssas[mem_set->addr->id].ptr);

  char *p = ir_get_raw_addr(interp, stack_frame, ptr, size,
                            IR_GET_RAW_ADDR_MODE_WRITE);

  if (!p) {
    return;
  }

  memset(p, mem_set->value, size);
}

static void ir_exec_op_mem_copy(struct ir_interp *interp,
                                struct ir_stack_frame *stack_frame,
                                struct ir_op *op) {
  struct ir_op_mem_copy *mem_copy = &op->mem_copy;

  size_t size = mem_copy->length;
  struct ir_interp_ptr source_ptr =
      ptr_from_addr(stack_frame->ssas[mem_copy->source->id].ptr);
  struct ir_interp_ptr dest_ptr =
      ptr_from_addr(stack_frame->ssas[mem_copy->dest->id].ptr);

  char *source = ir_get_raw_addr(interp, stack_frame, source_ptr, size,
                                 IR_GET_RAW_ADDR_MODE_READ);

  char *dest = ir_get_raw_addr(interp, stack_frame, dest_ptr, size,
                               IR_GET_RAW_ADDR_MODE_WRITE);

  if (!source || !dest) {
    return;
  }

  memcpy(dest, source, size);
}

static void ir_exec_op_mem_eq(struct ir_interp *interp,
                              struct ir_stack_frame *stack_frame,
                              struct ir_op *op) {
  struct ir_op_mem_eq *mem_eq = &op->mem_eq;
  struct ir_ssa_slot *slot = &stack_frame->ssas[op->id];

  size_t size = mem_eq->length;
  struct ir_interp_ptr lhs_ptr =
      ptr_from_addr(stack_frame->ssas[mem_eq->lhs->id].ptr);
  struct ir_interp_ptr rhs_ptr =
      ptr_from_addr(stack_frame->ssas[mem_eq->rhs->id].ptr);

  char *lhs = ir_get_raw_addr(interp, stack_frame, lhs_ptr, size,
                              IR_GET_RAW_ADDR_MODE_READ);

  char *rhs = ir_get_raw_addr(interp, stack_frame, rhs_ptr, size,
                              IR_GET_RAW_ADDR_MODE_WRITE);

  if (!lhs || !rhs) {
    return;
  }

  bool eq = memcmp(lhs, rhs, size) == 0;
  ir_write_ssa_slot(slot, eq);
}

static void ir_exec_op_addr(UNUSED struct ir_interp *interp,
                            struct ir_stack_frame *stack_frame,
                            struct ir_op *op) {
  struct ir_op_addr *addr = &op->addr;
  struct ir_ssa_slot *slot = &stack_frame->ssas[op->id];

  switch (addr->ty) {
  case IR_OP_ADDR_TY_LCL:
    ir_write_ssa_ptr(slot, addr_from_ptr(LCL_PTR(addr->lcl)));
    break;
  case IR_OP_ADDR_TY_GLB:
    ir_write_ssa_ptr(slot, addr_from_ptr(GLB_PTR(addr->glb)));
    break;
  }
}

static void ir_exec_op_addr_offset(UNUSED struct ir_interp *interp,
                                   struct ir_stack_frame *stack_frame,
                                   struct ir_op *op) {
  struct ir_op_addr_offset *addr_offset = &op->addr_offset;

  struct ir_ssa_slot *base = &stack_frame->ssas[addr_offset->base->id];

  struct ir_interp_ptr ptr = ptr_from_addr(base->ptr);

  if (addr_offset->index) {
    struct ir_ssa_slot *index = &stack_frame->ssas[addr_offset->index->id];
    ptr.offset += index->ptr * addr_offset->scale;
  }

  ptr.offset += addr_offset->offset;

  struct ir_ssa_slot *slot = &stack_frame->ssas[op->id];
  ir_write_ssa_ptr(slot, addr_from_ptr(ptr));
}

static void ir_exec_op_br(UNUSED struct ir_interp *interp,
                          struct ir_stack_frame *stack_frame,
                          UNUSED struct ir_op *op) {
  stack_frame->pred = stack_frame->cur_basicblock;
  stack_frame->cur_basicblock = stack_frame->cur_basicblock->merge.target;
}

static void ir_exec_op_br_cond(UNUSED struct ir_interp *interp,
                               struct ir_stack_frame *stack_frame,
                               struct ir_op *op) {
  struct ir_op_br_cond *br_cond = &op->br_cond;

  struct ir_ssa_slot *value = &stack_frame->ssas[br_cond->cond->id];

  stack_frame->pred = stack_frame->cur_basicblock;
  if (ir_eval_cond(value)) {
    stack_frame->cur_basicblock =
        stack_frame->cur_basicblock->split.true_target;
  } else {
    stack_frame->cur_basicblock =
        stack_frame->cur_basicblock->split.false_target;
  }
}

static void ir_exec_op_br_switch(UNUSED struct ir_interp *interp,
                                 UNUSED struct ir_stack_frame *stack_frame,
                                 UNUSED struct ir_op *op) {
  // struct ir_var_ty *var_ty = &op->var_ty;
  // struct ir_op_br_switch *br_switch = &op->br_switch;
  // struct ir_ssa_slot *slot = &stack_frame->ssas[op->id];
  TODO(__func__);
}

static void ir_exec_op_ret(UNUSED struct ir_interp *interp,
                           struct ir_stack_frame *stack_frame,
                           struct ir_op *op) {
  struct ir_op_ret *ret = &op->ret;

  stack_frame->ret_exec = true;
  stack_frame->cur_basicblock = NULL;

  if (ret->value) {
    struct ir_ssa_slot *value = &stack_frame->ssas[ret->value->id];

    ir_copy_slot(stack_frame->ret_slot, value);
  } else {
    ir_write_ssa_invalid(stack_frame->ret_slot);
  }
}

static void ir_interp_exec_func(struct ir_interp *interp, struct ir_func *func,
                                struct ir_ssa_slot *args, size_t num_args,
                                struct ir_ssa_slot *ret_slot);

static void ir_exec_op_call_undef(struct ir_interp *interp,
                                  struct ir_stack_frame *stack_frame,
                                  struct ir_op *op, struct ir_glb *target) {
  if (!strcmp(target->name, "printf")) {

  } else {
    TODO("undef call %s", target->name);
  }
}

static void ir_exec_op_call(struct ir_interp *interp,
                            struct ir_stack_frame *stack_frame,
                            struct ir_op *op) {
  struct ir_op_call *call = &op->call;
  struct ir_ssa_slot *slot = &stack_frame->ssas[op->id];

  struct ir_ssa_slot *target = &stack_frame->ssas[call->target->id];
  struct ir_interp_ptr target_ptr = ptr_from_addr(target->ptr);

  if (target_ptr.ty == IR_INTERP_PTR_TY_GLB) {
    DEBUG_ASSERT(target_ptr.offset == 0, "call w offset?");
    struct ir_glb *glb = interp->glbs[target_ptr.idx];

    switch (glb->def_ty) {
    case IR_GLB_DEF_TY_UNDEFINED:
      ir_exec_op_call_undef(interp, stack_frame, op, glb);
      break;
    default:
      DEBUG_ASSERT(call->num_args < 32, "TODO: more than 32 args");
      struct ir_ssa_slot args[32];
      for (size_t i = 0; i < call->num_args; i++) {
        ir_copy_slot(&args[i], &stack_frame->ssas[call->args[i]->id]);
      }

      struct ir_ssa_slot ret_slot;
      ir_interp_exec_func(interp, glb->func, args, call->num_args, &ret_slot);

      if (op->var_ty.ty != IR_VAR_TY_TY_NONE) {
        ir_copy_slot(slot, &ret_slot);
      }
      break;
    }
  } else {
    TODO("indir calls");
  }
}

static void ir_exec_op_gather(UNUSED struct ir_interp *interp,
                              UNUSED struct ir_stack_frame *stack_frame,
                              UNUSED struct ir_op *op) {
  // struct ir_var_ty *var_ty = &op->var_ty;
  // struct ir_op_gather *gather = &op->gather;
  // struct ir_ssa_slot *slot = &stack_frame->ssas[op->id];
  TODO(__func__);
}

static void ir_exec_op_va_start(UNUSED struct ir_interp *interp,
                                UNUSED struct ir_stack_frame *stack_frame,
                                UNUSED struct ir_op *op) {
  // struct ir_var_ty *var_ty = &op->var_ty;
  // struct ir_op_va_start *va_start = &op->va_start;
  // struct ir_ssa_slot *slot = &stack_frame->ssas[op->id];
  TODO(__func__);
}

static void ir_exec_op_va_arg(UNUSED struct ir_interp *interp,
                              UNUSED struct ir_stack_frame *stack_frame,
                              UNUSED struct ir_op *op) {
  // struct ir_var_ty *var_ty = &op->var_ty;
  // struct ir_op_va_arg *va_arg = &op->va_arg;
  // struct ir_ssa_slot *slot = &stack_frame->ssas[op->id];
  TODO(__func__);
}

static void ir_exec_op_binary_op(struct ir_interp *interp,
                                 struct ir_stack_frame *stack_frame,
                                 struct ir_op *op);

UNUSED static void debug_print_ssa_slot(FILE *file,
                                        const struct ir_ssa_slot *slot) {
  switch (slot->ty) {
  case IR_SLOT_TY_I1:
    fprintf(file, "%zu", (size_t)slot->i1);
    break;
  case IR_SLOT_TY_I8:
    fprintf(file, "%zu", (size_t)slot->i8);
    break;
  case IR_SLOT_TY_I16:
    fprintf(file, "%zu", (size_t)slot->i16);
    break;
  case IR_SLOT_TY_I32:
    fprintf(file, "%zu", (size_t)slot->i32);
    break;
  case IR_SLOT_TY_I64:
    fprintf(file, "%zu", (size_t)slot->i64);
    break;
  case IR_SLOT_TY_I128:
    TODO("i128");
  case IR_SLOT_TY_F16:
    fprintf(file, "%f", (double)slot->f16);
    break;
  case IR_SLOT_TY_F32:
    fprintf(file, "%f", (double)slot->f32);
    break;
  case IR_SLOT_TY_F64:
    fprintf(file, "%f", (double)slot->f64);
    break;
  case IR_SLOT_TY_PTR: {
    struct ir_interp_ptr ptr = ptr_from_addr(slot->ptr);

    switch (ptr.ty) {
    case IR_INTERP_PTR_TY_LCL:
      fprintf(file, "addr LCL(%u) + #%u", (i32)ptr.idx, (i32)ptr.offset);
      break;
    case IR_INTERP_PTR_TY_GLB:
      fprintf(file, "addr GLB(%u) + #%u", (i32)ptr.idx, (i32)ptr.offset);
      break;
    case IR_INTERP_PTR_TY_ALLOC:
      TODO("alloc");
    }
    break;
  }
  case IR_SLOT_TY_AGGREGATE:
    TODO("aggregate");
  }
}

static void ir_interp_exec_func(struct ir_interp *interp, struct ir_func *func,
                                struct ir_ssa_slot *args, size_t num_args,
                                struct ir_ssa_slot *ret_slot) {
  struct ir_stack_frame *stack_frame =
      ir_new_stack_frame(interp, func, args, num_args, ret_slot);

  struct ir_basicblock *basicblock = func->first;

  if (!basicblock) {
    return;
  }

  struct ir_basicblock *cur = NULL;
  stack_frame->cur_basicblock = basicblock;

  while (!stack_frame->ret_exec) {
    if (stack_frame->cur_basicblock == cur) {
      BUG("ran out of stmts!");
    }

    struct ir_stmt *stmt = stack_frame->cur_basicblock->first;

    while (stmt) {
      while (!stmt->first && stmt) {
        stmt = stmt->succ;
      }

      if (!stmt) {
        BUG("bb ended");
      }

      struct ir_op *op = stmt->first;

      while (op) {
        switch (op->ty) {
        case IR_OP_TY_UNKNOWN:
          ir_exec_op_unknown(interp, stack_frame, op);
          break;
        case IR_OP_TY_PHI:
          ir_exec_op_phi(interp, stack_frame, op);
          break;
        case IR_OP_TY_UNDF:
          ir_exec_op_undf(interp, stack_frame, op);
          break;
        case IR_OP_TY_MOV:
          ir_exec_op_mov(interp, stack_frame, op);
          break;
        case IR_OP_TY_CNST:
          ir_exec_op_cnst(interp, stack_frame, op);
          break;
        case IR_OP_TY_BINARY_OP:
          ir_exec_op_binary_op(interp, stack_frame, op);
          break;
        case IR_OP_TY_UNARY_OP:
          ir_exec_op_unary_op(interp, stack_frame, op);
          break;
        case IR_OP_TY_CAST_OP:
          ir_exec_op_cast_op(interp, stack_frame, op);
          break;
        case IR_OP_TY_SELECT:
          ir_exec_op_select(interp, stack_frame, op);
          break;
        case IR_OP_TY_STORE:
          ir_exec_op_store(interp, stack_frame, op);
          break;
        case IR_OP_TY_LOAD:
          ir_exec_op_load(interp, stack_frame, op);
          break;
        case IR_OP_TY_STORE_BITFIELD:
          ir_exec_op_store_bitfield(interp, stack_frame, op);
          break;
        case IR_OP_TY_LOAD_BITFIELD:
          ir_exec_op_load_bitfield(interp, stack_frame, op);
          break;
        case IR_OP_TY_BITFIELD_EXTRACT:
          ir_exec_op_bitfield_extract(interp, stack_frame, op);
          break;
        case IR_OP_TY_BITFIELD_INSERT:
          ir_exec_op_bitfield_insert(interp, stack_frame, op);
          break;
        case IR_OP_TY_MEM_SET:
          ir_exec_op_mem_set(interp, stack_frame, op);
          break;
        case IR_OP_TY_MEM_COPY:
          ir_exec_op_mem_copy(interp, stack_frame, op);
          break;
        case IR_OP_TY_MEM_EQ:
          ir_exec_op_mem_eq(interp, stack_frame, op);
          break;
        case IR_OP_TY_ADDR:
          ir_exec_op_addr(interp, stack_frame, op);
          break;
        case IR_OP_TY_ADDR_OFFSET:
          ir_exec_op_addr_offset(interp, stack_frame, op);
          break;
        case IR_OP_TY_BR:
          ir_exec_op_br(interp, stack_frame, op);
          break;
        case IR_OP_TY_BR_COND:
          ir_exec_op_br_cond(interp, stack_frame, op);
          break;
        case IR_OP_TY_BR_SWITCH:
          ir_exec_op_br_switch(interp, stack_frame, op);
          break;
        case IR_OP_TY_RET:
          ir_exec_op_ret(interp, stack_frame, op);
          break;
        case IR_OP_TY_CALL:
          ir_exec_op_call(interp, stack_frame, op);
          break;
        case IR_OP_TY_GATHER:
          ir_exec_op_gather(interp, stack_frame, op);
          break;
        case IR_OP_TY_VA_START:
          ir_exec_op_va_start(interp, stack_frame, op);
          break;
        case IR_OP_TY_VA_ARG:
          ir_exec_op_va_arg(interp, stack_frame, op);
          break;
        }

        // {
        //   debug_print_op(stderr, op, DEBUG_PRINT_IR_OPTS_DEFAULT);
        //   fprintf(stderr, "\n");
        //   fprintf(stderr, "%%%zu = ", op->id);
        //   debug_print_ssa_slot(stderr, &stack_frame->ssas[op->id]);
        //   fprintf(stderr, "\n\n");
        // }

        op = op->succ;
      }

      stmt = stmt->succ;
    }
  }
}

// basically stolen from codegen.c
static void ir_interp_write_var_value(struct ir_interp *interp, size_t offset,
                                      struct ir_var_value *value, char *data) {
  if (!value || value->ty == IR_VAR_VALUE_TY_ZERO) {
    return;
  }

#define COPY(ty, fld)                                                          \
  ty tmp##ty = (ty)value->fld;                                                 \
  memcpy(data, &tmp##ty, sizeof(tmp##ty))
  switch (value->var_ty.ty) {
  case IR_VAR_TY_TY_NONE:
  case IR_VAR_TY_TY_VARIADIC:
    break;
  case IR_VAR_TY_TY_PRIMITIVE: {
    switch (value->var_ty.primitive) {
    case IR_VAR_PRIMITIVE_TY_I1:
    case IR_VAR_PRIMITIVE_TY_I8:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT, "expected int");
      COPY(uint8_t, int_value);
      break;
    case IR_VAR_PRIMITIVE_TY_F16:
    case IR_VAR_PRIMITIVE_TY_I16:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT ||
                       value->ty == IR_VAR_VALUE_TY_FLT,
                   "expected int/flt");
      COPY(uint16_t, int_value);
      break;
    case IR_VAR_PRIMITIVE_TY_I32:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT, "expected int");
      COPY(uint32_t, int_value);
      break;
    case IR_VAR_PRIMITIVE_TY_I64:
    case IR_VAR_PRIMITIVE_TY_I128:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT, "expected int");
      COPY(uint64_t, int_value);
      break;
    // currently the literal is only 64 bits
    // case IR_VAR_PRIMITIVE_TY_I128:
    //   DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT, "expected int");
    //   COPY(uint128_t, int_value);
    //   break;
    case IR_VAR_PRIMITIVE_TY_F32:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_FLT, "expected flt");
      COPY(float, flt_value);
      break;
    case IR_VAR_PRIMITIVE_TY_F64:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_FLT, "expected flt");
      COPY(double, flt_value);
      break;
    }
    break;
  }
#undef COPY

  case IR_VAR_TY_TY_FUNC:
    BUG("func can not have data as a global var");

    // FIXME: some bugs here around using compiler-ptr-size when we mean to use
    // target-ptr-size

  case IR_VAR_TY_TY_POINTER:
  case IR_VAR_TY_TY_ARRAY:
    switch (value->ty) {
    case IR_VAR_VALUE_TY_ZERO:
    case IR_VAR_VALUE_TY_FLT:
      BUG("doesn't make sense");
    case IR_VAR_VALUE_TY_ADDR: {
      const struct ir_var_addr *addr = &value->addr;

      struct ir_interp_ptr ptr = {.ty = IR_INTERP_PTR_TY_GLB,
                                  .idx = addr->glb->id,
                                  .offset = addr->offset};

      memcpy(data, &ptr, sizeof(ptr));
      break;
    }
    case IR_VAR_VALUE_TY_INT:
      memcpy(data, &value->int_value, sizeof(void *));
      break;
    case IR_VAR_VALUE_TY_STR: {
      struct ir_var_ty var_ty = value->var_ty;
      DEBUG_ASSERT(var_ty.ty == IR_VAR_TY_TY_ARRAY,
                   "expected IR_VAR_VALUE_TY_STR to be an array");

      struct ir_var_ty ch_ty = *var_ty.array.underlying;

      // can't copy whole array as the string may be shorter and part of it
      // so copy sizeof(ch) * num_chr
      size_t len =
          value->str_value.len * ir_var_ty_info(interp->unit, &ch_ty).size;
      memcpy(data, value->str_value.value, len);
      break;
    }
    case IR_VAR_VALUE_TY_VALUE_LIST:
      for (size_t i = 0; i < value->value_list.num_values; i++) {
        size_t value_offset = value->value_list.offsets[i];
        ir_interp_write_var_value(interp, offset + value_offset,
                                  &value->value_list.values[i],
                                  &data[value_offset]);
      }
      break;
    }
    break;

  case IR_VAR_TY_TY_STRUCT:
  case IR_VAR_TY_TY_UNION:
    DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_VALUE_LIST,
                 "expected value list");
    for (size_t i = 0; i < value->value_list.num_values; i++) {
      size_t field_offset = value->value_list.offsets[i];
      ir_interp_write_var_value(interp, offset + field_offset,
                                &value->value_list.values[i],
                                &data[field_offset]);
    }
  }
}

static struct ir_interp_glb_data
ir_interp_define_glb_data(struct ir_interp *interp, struct ir_glb *glb) {
  if (glb->def_ty != IR_GLB_DEF_TY_DEFINED) {
    return (struct ir_interp_glb_data){.data = NULL, .len = 0};
  }

  struct ir_var_ty_info info = ir_var_ty_info(interp->unit, &glb->var->var_ty);

  size_t len = info.size;

  char *data = aralloc(interp->arena, len);
  memset(data, 0, len);

  ir_interp_write_var_value(interp, 0, &glb->var->value, data);

  return (struct ir_interp_glb_data){.data = data, .len = len};
}

struct ir_interp_exec_info ir_interp_exec(struct ir_interp *interp) {
  struct ir_ssa_slot args[2];
  ir_write_ssa_i32(&args[0], 0);
  ir_write_ssa_ptr(&args[1], 0);

  struct ir_ssa_slot ret = {0};

  ir_interp_exec_func(interp, interp->entrypoint->func, args, ARR_LENGTH(args),
                      &ret);

  return (struct ir_interp_exec_info){.exc = ret.i8};
}

struct ir_interp *ir_interp_create(struct ir_interp_create_args args) {
  struct arena_allocator *arena;

  arena_allocator_create("ir-interp", &arena);

  struct ir_interp *interp = aralloc(arena, sizeof(*interp));

  // when using ustr_t, will need to change to ustr_keyed hashtbl
  static_assert(sizeof(args.unit->first_global->name) == sizeof(char *));

  struct ir_unit *unit = args.unit;

  *interp = (struct ir_interp){
      .arena = arena,
      .unit = unit,
      .glbs = aralloc(arena, sizeof(*interp->glbs) * unit->glb_count),
      .glb_data = aralloc(arena, sizeof(*interp->glb_data) * unit->glb_count),
      .stack_frames =
          vector_create_in_arena(sizeof(struct ir_stack_frame), arena),
      .num_glbs = unit->glb_count};

  struct ir_glb *glb = args.unit->first_global;

  size_t idx = 0;
  while (glb) {
    interp->glb_data[idx] = ir_interp_define_glb_data(interp, glb);
    interp->glbs[idx++] = glb;

    if (ustr_eq(args.entrypoint, glb->name)) {
      interp->entrypoint = glb;
    }

    glb = glb->succ;
  }

  if (!interp->entrypoint ||
      interp->entrypoint->def_ty != IR_GLB_DEF_TY_DEFINED ||
      interp->entrypoint->ty != IR_GLB_TY_FUNC) {
    BUG("invalid entrypoint");
  }

  return interp;
}

void ir_interp_free(struct ir_interp **interp) {
  struct arena_allocator *arena = (*interp)->arena;

  arena_allocator_free(&arena);

  *interp = NULL;
}

// the macros mess up treesitter so put at bottom of file

static void ir_exec_op_unary_op(UNUSED struct ir_interp *interp,
                                struct ir_stack_frame *stack_frame,
                                struct ir_op *op) {
  struct ir_op_unary_op *unary_op = &op->unary_op;
  struct ir_ssa_slot *slot = &stack_frame->ssas[op->id];

  struct ir_ssa_slot *value = &stack_frame->ssas[unary_op->value->id];

#define UNNOP_INTEGRAL(name, op)                                               \
  case IR_OP_UNARY_OP_TY_##name: {                                             \
    switch (value->ty) {                                                       \
    case IR_SLOT_TY_I1:                                                        \
      ir_write_ssa_i1(slot, (i1)(op value->i1));                               \
      break;                                                                   \
    case IR_SLOT_TY_I8:                                                        \
      ir_write_ssa_i8(slot, (i8)(op value->i8));                               \
      break;                                                                   \
    case IR_SLOT_TY_I16:                                                       \
      ir_write_ssa_i16(slot, (i16)(op value->i16));                            \
      break;                                                                   \
    case IR_SLOT_TY_I32:                                                       \
      ir_write_ssa_i32(slot, (i32)(op value->i32));                            \
      break;                                                                   \
    case IR_SLOT_TY_I64:                                                       \
      ir_write_ssa_i64(slot, (i64)(op value->i64));                            \
      break;                                                                   \
    case IR_SLOT_TY_I128:                                                      \
      ir_write_ssa_i128(slot, (i128)(op value->i128));                         \
      break;                                                                   \
    case IR_SLOT_TY_F16:                                                       \
    default:                                                                   \
      unreachable();                                                           \
    }                                                                          \
    break;                                                                     \
  }

  switch (unary_op->ty) {
  case IR_OP_UNARY_OP_TY_FNEG:
  case IR_OP_UNARY_OP_TY_FSQRT:
  case IR_OP_UNARY_OP_TY_FABS:
    TODO("fp unary");

    UNNOP_INTEGRAL(NEG, -);
    UNNOP_INTEGRAL(LOGICAL_NOT, !);

    PUSH_NO_WARN("-Wbool-operation")
    UNNOP_INTEGRAL(NOT, ~);
    POP_NO_WARN()

  case IR_OP_UNARY_OP_TY_POPCNT:
  case IR_OP_UNARY_OP_TY_CLZ:
  case IR_OP_UNARY_OP_TY_CTZ:
  case IR_OP_UNARY_OP_TY_REV:
    TODO("other unaries");
  }
}

static void ir_exec_op_binary_op(UNUSED struct ir_interp *interp,
                                 struct ir_stack_frame *stack_frame,
                                 struct ir_op *op) {
  struct ir_op_binary_op *binary_op = &op->binary_op;
  struct ir_ssa_slot *slot = &stack_frame->ssas[op->id];

  struct ir_ssa_slot *lhs = &stack_frame->ssas[binary_op->lhs->id];
  struct ir_ssa_slot *rhs = &stack_frame->ssas[binary_op->rhs->id];

#define BINOP_INTEGRAL(name, op)                                               \
  case IR_OP_BINARY_OP_TY_##name: {                                            \
    switch (lhs->ty) {                                                         \
    case IR_SLOT_TY_I1:                                                        \
      ir_write_ssa_i1(slot, (i1)(lhs->i1 op rhs->i1));                         \
      break;                                                                   \
    case IR_SLOT_TY_I8:                                                        \
      ir_write_ssa_i8(slot, (i8)(lhs->i8 op rhs->i8));                         \
      break;                                                                   \
    case IR_SLOT_TY_I16:                                                       \
      ir_write_ssa_i16(slot, (i16)(lhs->i16 op rhs->i16));                     \
      break;                                                                   \
    case IR_SLOT_TY_I32:                                                       \
      ir_write_ssa_i32(slot, (i32)(lhs->i32 op rhs->i32));                     \
      break;                                                                   \
    case IR_SLOT_TY_I64:                                                       \
      ir_write_ssa_i64(slot, (i64)(lhs->i64 op rhs->i64));                     \
      break;                                                                   \
    case IR_SLOT_TY_I128:                                                      \
      ir_write_ssa_i128(slot, (i128)(lhs->i128 op rhs->i128));                 \
      break;                                                                   \
    case IR_SLOT_TY_F16:                                                       \
    default:                                                                   \
      unreachable();                                                           \
    }                                                                          \
    break;                                                                     \
  }

  switch (binary_op->ty) {
    BINOP_INTEGRAL(EQ, ==);
    BINOP_INTEGRAL(NEQ, !=);
    BINOP_INTEGRAL(UGT, >);
    BINOP_INTEGRAL(SGT, >);
    BINOP_INTEGRAL(UGTEQ, >=);
    BINOP_INTEGRAL(SGTEQ, >=);
    BINOP_INTEGRAL(ULT, <);
    BINOP_INTEGRAL(SLT, <);
    BINOP_INTEGRAL(ULTEQ, <=);
    BINOP_INTEGRAL(SLTEQ, <=);
    BINOP_INTEGRAL(LSHIFT, <<);
    BINOP_INTEGRAL(SRSHIFT, >>);
    BINOP_INTEGRAL(URSHIFT, >>);
    BINOP_INTEGRAL(AND, &);
    BINOP_INTEGRAL(OR, |);
    BINOP_INTEGRAL(XOR, ^);
    BINOP_INTEGRAL(ADD, +);
    BINOP_INTEGRAL(SUB, -);
    BINOP_INTEGRAL(MUL, *);
    BINOP_INTEGRAL(SDIV, /);
    BINOP_INTEGRAL(UDIV, /);
    BINOP_INTEGRAL(SMOD, %);
    BINOP_INTEGRAL(UMOD, %);
    BINOP_INTEGRAL(FADD, +);
    BINOP_INTEGRAL(FSUB, -);
    BINOP_INTEGRAL(FMUL, *);
    BINOP_INTEGRAL(FDIV, /);
    // FIXME:
    BINOP_INTEGRAL(FMAX, ==);
    BINOP_INTEGRAL(FMIN, ==);

    BINOP_INTEGRAL(FEQ, ==);
    BINOP_INTEGRAL(FNEQ, !=);
    BINOP_INTEGRAL(FGT, >);
    BINOP_INTEGRAL(FGTEQ, >=);
    BINOP_INTEGRAL(FLT, <);
    BINOP_INTEGRAL(FLTEQ, <=);
  }
}
