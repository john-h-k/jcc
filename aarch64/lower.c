#include "lower.h"

#include "../aarch64.h"
#include "../bit_twiddle.h"
#include "../ir/build.h"
#include "../ir/var_refs.h"
#include "../util.h"

#include <mach/arm/vm_types.h>
#include <math.h>

static void lower_logical_not(struct ir_builder *func, struct ir_op *op) {
  debug_assert(op->ty == IR_OP_TY_UNARY_OP &&
                   op->binary_op.ty == IR_OP_UNARY_OP_TY_LOGICAL_NOT,
               "called on invalid op");

  struct ir_op *zero = insert_before_ir_op(func, op, IR_OP_TY_CNST, op->var_ty);
  zero->cnst.ty = IR_OP_CNST_TY_INT;
  zero->cnst.int_value = 0;

  op->ty = IR_OP_TY_BINARY_OP;
  op->binary_op.ty = IR_OP_BINARY_OP_TY_EQ;
  op->binary_op.lhs = op->unary_op.value;
  op->binary_op.rhs = zero;
}

// ARM has no quotient function
// so instead of `x = a % b` we do
// `c = a / b; x = a - (c * b)`
static void lower_quot(struct ir_builder *func, struct ir_op *op) {
  debug_assert(op->ty == IR_OP_TY_BINARY_OP &&
                   (op->binary_op.ty == IR_OP_BINARY_OP_TY_UQUOT ||
                    op->binary_op.ty == IR_OP_BINARY_OP_TY_SQUOT),
               "lower_quot called on invalid op");

  enum ir_op_binary_op_ty div_ty;
  enum ir_op_sign sign = binary_op_sign(op->binary_op.ty);
  switch (sign) {
  case IR_OP_SIGN_NA:
    bug("trying to `lower_quot` but `binary_op_sign` return `IR_OP_SIGN_NA`");
    break;
  case IR_OP_SIGN_SIGNED:
    div_ty = IR_OP_BINARY_OP_TY_SDIV;
    break;
  case IR_OP_SIGN_UNSIGNED:
    div_ty = IR_OP_BINARY_OP_TY_UDIV;
    break;
  }

  // we could directly generate an MSUB here but we instead rely on fusing later

  // c = a / b

  struct ir_op *div =
      insert_before_ir_op(func, op, IR_OP_TY_BINARY_OP, op->var_ty);
  div->binary_op.ty = div_ty;
  div->binary_op.lhs = op->binary_op.lhs;
  div->binary_op.rhs = op->binary_op.rhs;

  // y = c * b

  struct ir_op *mul =
      insert_after_ir_op(func, div, IR_OP_TY_BINARY_OP, op->var_ty);
  mul->binary_op.ty = IR_OP_BINARY_OP_TY_MUL;
  mul->binary_op.lhs = div;
  mul->binary_op.rhs = op->binary_op.rhs;

  // x = a - y

  // Now we replace `op` with `sub` (as `sub` is the op that actually produces
  // the value) this preserves links, as other ops pointing to the div will now
  // point at the sub
  op->ty = IR_OP_TY_BINARY_OP;
  op->var_ty = op->var_ty;
  op->binary_op.ty = IR_OP_BINARY_OP_TY_SUB;
  op->binary_op.lhs = op->binary_op.lhs;
  op->binary_op.rhs = mul;
}

static void lower_comparison(struct ir_builder *irb, struct ir_op *op) {
  UNUSED_ARG(irb);

  invariant_assert(op->ty == IR_OP_TY_BINARY_OP &&
                       binary_op_is_comparison(op->binary_op.ty),
                   "non comparison op");

  // mark it as writing to flag reg so register allocator doesn't intefere with
  // it
  op->reg = REG_FLAGS;

  if (op->succ && op->succ->ty == IR_OP_TY_BR_COND) {
    // don't need to insert `mov` because emitter understands how to emit a
    // branch depending on REG_FLAGS
    return;
  }

  // emitter understands how to emit a `mov` from a comparison into REG_FLAGS
  // insert a new op after the current one, move this op into it, then make that
  // op a `mov` this turns all the ops pointing to the branch into pointing to
  // the mov, as we want
  struct ir_op *new_br = insert_before_ir_op(irb, op, op->ty, op->var_ty);
  new_br->binary_op = op->binary_op;
  new_br->reg = REG_FLAGS;

  op->ty = IR_OP_TY_MOV;
  op->mov.value = new_br;
  op->reg = NO_REG;
}

static void lower_store_glb(struct ir_builder *func, struct ir_op *op) {
  struct ir_op_var_ty pointer_ty = var_ty_make_pointer(func, &op->var_ty);

  struct ir_op *addr = insert_before_ir_op(func, op, IR_OP_TY_ADDR, pointer_ty);
  addr->addr =
      (struct ir_op_addr){.ty = IR_OP_ADDR_TY_GLB, .glb = op->load_glb.glb};

  struct ir_op *value = op->store_glb.value;

  op->ty = IR_OP_TY_STORE_ADDR;
  op->store_addr = (struct ir_op_store_addr){.addr = addr, .value = value};
}

static void lower_load_glb(struct ir_builder *func, struct ir_op *op) {
  struct ir_op_var_ty pointer_ty = var_ty_make_pointer(func, &op->var_ty);

  struct ir_op *addr = insert_before_ir_op(func, op, IR_OP_TY_ADDR, pointer_ty);
  addr->addr =
      (struct ir_op_addr){.ty = IR_OP_ADDR_TY_GLB, .glb = op->load_glb.glb};

  op->ty = IR_OP_TY_LOAD_ADDR;
  op->load_addr = (struct ir_op_load_addr){.addr = addr};
}

// actually more than this but we don't support that yet
#define MAX_REG_SIZE (8)

static void lower_load_lcl(struct ir_builder *func, struct ir_op *op) {
  // look for store after, in case this is a copy
  // FIXME: not sure if this is perfect logic (could there be ops in between?)
  struct ir_op *nxt_store = op->succ;

  if (!nxt_store || nxt_store->ty != IR_OP_TY_STORE_LCL) {
    return;
  }

  struct ir_var_ty_info info = var_ty_info(func->unit, &op->var_ty);

  bool simple_copy = true;
  enum ir_op_var_primitive_ty simple_copy_ty;
  switch (info.size) {
  case 1:
    simple_copy_ty = IR_OP_VAR_PRIMITIVE_TY_I8;
    break;
  case 2:
    simple_copy_ty = IR_OP_VAR_PRIMITIVE_TY_I16;
    break;
  case 4:
    simple_copy_ty = IR_OP_VAR_PRIMITIVE_TY_I32;
    break;
  case 8:
    simple_copy_ty = IR_OP_VAR_PRIMITIVE_TY_I64;
    break;

  default:
    simple_copy = false;
  }

  if (simple_copy) {
    op->var_ty = (struct ir_op_var_ty){.ty = IR_OP_VAR_TY_TY_PRIMITIVE,
                                       .primitive = simple_copy_ty};
    nxt_store->var_ty = (struct ir_op_var_ty){.ty = IR_OP_VAR_TY_TY_PRIMITIVE,
                                              .primitive = simple_copy_ty};

    return;
  }

  if (info.size < MAX_REG_SIZE) {
    todo("non-pow2 copies < MAX_REG_SIZE");
  }

  struct ir_op_var_ty copy_ty = var_ty_for_pointer_size(func);
  struct ir_op_var_ty pointer_copy_ty = var_ty_make_pointer(func, &copy_ty);

  struct ir_lcl *src_lcl = op->load_lcl.lcl;
  struct ir_lcl *dest_lcl = nxt_store->lcl;

  struct ir_op *base_src_addr = op;
  struct ir_op *base_dest_addr = nxt_store;

  base_src_addr->ty = IR_OP_TY_ADDR;
  base_src_addr->var_ty = pointer_copy_ty;
  base_src_addr->addr =
      (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = src_lcl};

  base_dest_addr->ty = IR_OP_TY_ADDR;
  base_dest_addr->var_ty = pointer_copy_ty;
  base_dest_addr->addr =
      (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = dest_lcl};

  struct ir_op *last = base_dest_addr;

  size_t size_left = info.size;
  size_t offset = 0;
  while (size_left >= MAX_REG_SIZE) {
    struct ir_op *offset_cnst =
        insert_after_ir_op(func, last, IR_OP_TY_CNST, copy_ty);
    make_pointer_constant(func, offset_cnst, offset);

    struct ir_op *src_addr = insert_after_ir_op(
        func, offset_cnst, IR_OP_TY_BINARY_OP, pointer_copy_ty);
    src_addr->binary_op = (struct ir_op_binary_op){
        .ty = IR_OP_BINARY_OP_TY_ADD, .lhs = base_src_addr, .rhs = offset_cnst};

    struct ir_op *load =
        insert_after_ir_op(func, src_addr, IR_OP_TY_LOAD_ADDR, copy_ty);
    load->load_addr = (struct ir_op_load_addr){.addr = src_addr};

    struct ir_op *dest_addr =
        insert_after_ir_op(func, load, IR_OP_TY_BINARY_OP, pointer_copy_ty);
    dest_addr->binary_op =
        (struct ir_op_binary_op){.ty = IR_OP_BINARY_OP_TY_ADD,
                                 .lhs = base_dest_addr,
                                 .rhs = offset_cnst};

    struct ir_op *store = insert_after_ir_op(
        func, dest_addr, IR_OP_TY_STORE_ADDR, IR_OP_VAR_TY_NONE);
    store->store_addr =
        (struct ir_op_store_addr){.addr = dest_addr, .value = load};

    last = store;
    size_left -= MAX_REG_SIZE;
    offset += MAX_REG_SIZE;
  }

  // now we have to do the last trailing load
  // because size is >= MAX_REG_SIZE,
  // we can just do a whole-reg copy starting from end-MAX_REG_SIZE
  if (size_left) {
    size_t offset = MAX_REG_SIZE - size_left;

    struct ir_op *offset_cnst =
        insert_after_ir_op(func, last, IR_OP_TY_CNST, copy_ty);
    make_pointer_constant(func, offset_cnst, offset);

    struct ir_op *src_addr = insert_after_ir_op(
        func, offset_cnst, IR_OP_TY_BINARY_OP, pointer_copy_ty);
    src_addr->binary_op = (struct ir_op_binary_op){
        .ty = IR_OP_BINARY_OP_TY_ADD, .lhs = base_src_addr, .rhs = offset_cnst};

    struct ir_op *load =
        insert_after_ir_op(func, src_addr, IR_OP_TY_LOAD_ADDR, copy_ty);
    load->load_addr = (struct ir_op_load_addr){.addr = src_addr};

    struct ir_op *dest_addr =
        insert_after_ir_op(func, load, IR_OP_TY_BINARY_OP, pointer_copy_ty);
    dest_addr->binary_op =
        (struct ir_op_binary_op){.ty = IR_OP_BINARY_OP_TY_ADD,
                                 .lhs = base_dest_addr,
                                 .rhs = offset_cnst};

    struct ir_op *store = insert_after_ir_op(
        func, dest_addr, IR_OP_TY_STORE_ADDR, IR_OP_VAR_TY_NONE);
    store->store_addr =
        (struct ir_op_store_addr){.addr = dest_addr, .value = load};
  }
}

static void lower_fp_cnst(struct ir_builder *func, struct ir_op *op) {
  // transform into creating an integer, and then mov to float reg

  struct ir_op_var_ty int_ty;
  unsigned long long int_value;

  debug_assert(var_ty_is_fp(&op->var_ty), "float constant not fp type?");

  switch (op->var_ty.primitive) {
  case IR_OP_VAR_PRIMITIVE_TY_F32: {
    int_ty = IR_OP_VAR_TY_I32;

    union {
      float f;
      unsigned u;
    } v;
    v.f = (float)op->cnst.flt_value;
    int_value = v.u;

    break;
  }
  case IR_OP_VAR_PRIMITIVE_TY_F64: {
    int_ty = IR_OP_VAR_TY_I64;

    union {
      double d;
      unsigned long long ull;
    } v;
    v.d = (double)op->cnst.flt_value;
    int_value = v.ull;

    break;
  }
  default:
    unreachable("impossible type");
  }

  struct ir_op *int_mov = insert_before_ir_op(func, op, IR_OP_TY_CNST, int_ty);
  int_mov->cnst =
      (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = int_value};

  op->ty = IR_OP_TY_MOV;
  op->mov = (struct ir_op_mov){.value = int_mov};
}

void aarch64_lower(struct ir_unit *unit) {
  struct ir_glb *glb = unit->first_global;
  while (glb) {
    if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
      glb = glb->succ;
      continue;
    }

    switch (glb->ty) {
    case IR_GLB_TY_DATA:
      break;
    case IR_GLB_TY_FUNC: {
      struct ir_builder *func = glb->func;
      struct ir_basicblock *basicblock = func->first;
      while (basicblock) {
        struct ir_stmt *stmt = basicblock->first;

        while (stmt) {
          struct ir_op *op = stmt->first;

          while (op) {
            switch (op->ty) {
            case IR_OP_TY_UNKNOWN:
              bug("unknown op!");
            case IR_OP_TY_UNDF:
            case IR_OP_TY_CUSTOM:
            case IR_OP_TY_PHI:
              break;
            case IR_OP_TY_CNST: {
              if (op->cnst.ty == IR_OP_CNST_TY_FLT) {
                lower_fp_cnst(func, op);
                break;
              }

              break;
            }
            case IR_OP_TY_STORE_GLB:
              lower_store_glb(func, op);
              break;
            case IR_OP_TY_LOAD_GLB:
              lower_load_glb(func, op);
              break;
            case IR_OP_TY_STORE_LCL:
              break;
            case IR_OP_TY_LOAD_LCL:
              lower_load_lcl(func, op);
              break;
            case IR_OP_TY_STORE_ADDR:
            case IR_OP_TY_LOAD_ADDR:
            case IR_OP_TY_ADDR:
            case IR_OP_TY_BR:
            case IR_OP_TY_BR_COND:
            case IR_OP_TY_MOV:
            case IR_OP_TY_RET:
            case IR_OP_TY_CAST_OP:
              break;
            case IR_OP_TY_CALL:
              if (op->call.target->ty == IR_OP_TY_ADDR) {
                op->call.target->flags |= IR_OP_FLAG_CONTAINED;
              }
              break;
            case IR_OP_TY_UNARY_OP:
              if (op->binary_op.ty == IR_OP_UNARY_OP_TY_LOGICAL_NOT) {
                lower_logical_not(func, op);
              }
              break;
            case IR_OP_TY_BINARY_OP:
              if (op->binary_op.ty == IR_OP_BINARY_OP_TY_UQUOT ||
                  op->binary_op.ty == IR_OP_BINARY_OP_TY_SQUOT) {
                lower_quot(func, op);
              }
              break;
            }

            op = op->succ;
          }

          stmt = stmt->succ;
        }

        basicblock = basicblock->succ;
      }

      // now we lower comparisons as the above can generate them (via
      // logical_not)
      basicblock = func->first;
      while (basicblock) {
        struct ir_stmt *stmt = basicblock->first;

        while (stmt) {
          struct ir_op *op = stmt->first;

          while (op) {
            switch (op->ty) {
            case IR_OP_TY_UNKNOWN:
              bug("unknown op!");
            case IR_OP_TY_UNDF:
            case IR_OP_TY_CUSTOM:
            case IR_OP_TY_PHI:
            case IR_OP_TY_CNST:
            case IR_OP_TY_STORE_GLB:
            case IR_OP_TY_LOAD_GLB:
            case IR_OP_TY_STORE_LCL:
            case IR_OP_TY_LOAD_LCL:
            case IR_OP_TY_STORE_ADDR:
            case IR_OP_TY_LOAD_ADDR:
            case IR_OP_TY_ADDR:
            case IR_OP_TY_BR:
            case IR_OP_TY_BR_COND:
            case IR_OP_TY_MOV:
            case IR_OP_TY_RET:
            case IR_OP_TY_CALL:
            case IR_OP_TY_CAST_OP:
            case IR_OP_TY_UNARY_OP:
              break;
            case IR_OP_TY_BINARY_OP:
              if (binary_op_is_comparison(op->binary_op.ty)) {
                lower_comparison(func, op);
              }
              break;
            }

            op = op->succ;
          }

          stmt = stmt->succ;
        }

        basicblock = basicblock->succ;
      }

      // Final step: turn all TY_POINTER into TY_I64 as the information is no
      // longer needed
      basicblock = func->first;
      while (basicblock) {
        struct ir_stmt *stmt = basicblock->first;

        while (stmt) {
          struct ir_op *op = stmt->first;

          while (op) {
            if (op->var_ty.ty == IR_OP_VAR_TY_TY_POINTER) {
              op->var_ty = var_ty_for_pointer_size(func);
            }

            op = op->succ;
          }

          stmt = stmt->succ;
        }

        basicblock = basicblock->succ;
      }
      break;
    }
    }

    glb = glb->succ;
  }
}
