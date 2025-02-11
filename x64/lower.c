#include "lower.h"

#include "../bit_twiddle.h"
#include "../util.h"

#define IR_REG_IDX_AX (6)
#define IR_REG_IDX_CX (3)
#define IR_REG_IDX_DX (2)
#define IR_REG_IDX_SI (1)
#define IR_REG_IDX_DI (0)

enum load_bitfield {
  LOAD_BITFIELD_MASK_IN,
  LOAD_BITFIELD_MASK_OUT,
};

static struct ir_op *get_unshifted_bitfield(struct ir_func *func,
                                            struct ir_op *op,
                                            struct ir_bitfield bitfield,
                                            enum load_bitfield load_bitfield) {
  unsigned int mask_val;

  switch (load_bitfield) {
  case LOAD_BITFIELD_MASK_IN:
    mask_val =
        ~MASK_OUT(unsigned, bitfield.width + bitfield.offset, bitfield.offset);
    break;
  case LOAD_BITFIELD_MASK_OUT:
    mask_val =
        MASK_OUT(unsigned, bitfield.width + bitfield.offset, bitfield.offset);
    break;
  }

  // printf("mask lo %zu = %u\n", offset, bitfield.width, MASK_HI(unsigned,
  // bitfield.width + bitfield.offset, bitfield.offset)); printf("mask hi %zu =
  // %u\n", bitfield.offset, bitfield.width, MASK_LO(unsigned, bitfield.width +
  // bitfield.offset, bitfield.offset)); bug("mask (%zu, %zu) = %u",
  // bitfield.offset, bitfield.width, mask_val);

  struct ir_op *mask_cnst =
      insert_after_ir_op(func, op, IR_OP_TY_CNST, IR_VAR_TY_I32);
  mask_cnst->cnst =
      (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = mask_val};

  struct ir_op *mask =
      insert_after_ir_op(func, mask_cnst, IR_OP_TY_BINARY_OP, IR_VAR_TY_I32);
  mask->binary_op = (struct ir_op_binary_op){
      .ty = IR_OP_BINARY_OP_TY_AND, .lhs = op, .rhs = mask_cnst};

  return mask;
}

// TODO: signs and stuff are wrong
static void lower_bitfield_insert(struct ir_func *func, struct ir_op *op) {
  struct ir_op_bitfield_insert *insert = &op->bitfield_insert;
  struct ir_bitfield bitfield = op->store_bitfield.bitfield;

  struct ir_op *masked_out = get_unshifted_bitfield(
      func, insert->target, bitfield, LOAD_BITFIELD_MASK_OUT);

  struct ir_op *shifted_op;
  if (bitfield.offset) {
    struct ir_op *shift_cnst =
        insert_after_ir_op(func, insert->target, IR_OP_TY_CNST, IR_VAR_TY_I32);
    shift_cnst->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT,
                                           .int_value = bitfield.offset};
    shifted_op =
        insert_after_ir_op(func, shift_cnst, IR_OP_TY_BINARY_OP, IR_VAR_TY_I32);
    shifted_op->binary_op =
        (struct ir_op_binary_op){.ty = IR_OP_BINARY_OP_TY_LSHIFT,
                                 .lhs = insert->value,
                                 .rhs = shift_cnst};
  } else {
    shifted_op = insert->value;
  }

  struct ir_op *mask_in =
      replace_ir_op(func, op, IR_OP_TY_BINARY_OP, IR_VAR_TY_I32);

  mask_in->binary_op = (struct ir_op_binary_op){
      .ty = IR_OP_BINARY_OP_TY_OR, .lhs = masked_out, .rhs = shifted_op};
}

static void lower_bitfield_extract(struct ir_func *func, struct ir_op *op) {
  struct ir_op_bitfield_extract *extract = &op->bitfield_extract;
  struct ir_bitfield bitfield = op->load_bitfield.bitfield;

  struct ir_op *masked_in = get_unshifted_bitfield(
      func, extract->value, bitfield, LOAD_BITFIELD_MASK_IN);

  if (bitfield.offset) {
    struct ir_op *shift_cnst =
        insert_after_ir_op(func, masked_in, IR_OP_TY_CNST, IR_VAR_TY_I32);
    shift_cnst->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT,
                                           .int_value = bitfield.offset};
    op->ty = IR_OP_TY_BINARY_OP;
    op->var_ty = IR_VAR_TY_I32;
    op->binary_op = (struct ir_op_binary_op){
        .ty = IR_OP_BINARY_OP_TY_URSHIFT, .lhs = masked_in, .rhs = shift_cnst};
  } else {
    // ugly
    op->ty = IR_OP_TY_MOV;
    op->var_ty = IR_VAR_TY_I32;
    op->mov = (struct ir_op_mov){.value = masked_in};
  }
}

static void lower_logical_not(struct ir_func *func, struct ir_op *op) {
  DEBUG_ASSERT(op->ty == IR_OP_TY_UNARY_OP &&
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

// variable shifts require both operands to be the same size, as they use the
// same register this is fine, because we can just "fake" the type required and
// get the correct behaviour
static void lower_shift(UNUSED struct ir_func *func, struct ir_op *op) {
  struct ir_op_binary_op *binary_op = &op->binary_op;

  binary_op->rhs->var_ty = binary_op->lhs->var_ty;
}

static void lower_comparison(struct ir_func *irb, struct ir_op *op) {
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

static void lower_store(struct ir_func *func, struct ir_op *op) {
  struct ir_op *source = op->store.value;

  const struct ir_var_ty *var_ty = &source->var_ty;

  if (var_ty_is_integral(var_ty) || var_ty_is_fp(var_ty)) {
    return;
  }

  struct ir_var_ty_info info = var_ty_info(func->unit, var_ty);

  if (source->ty != IR_OP_TY_LOAD) {
    BUG("non-primitive store occured out of a non-load op?");
  }

  struct ir_op *source_addr = build_addr(func, source);
  struct ir_op *dest_addr = build_addr(func, op);

  struct ir_op *size =
      insert_after_ir_op(func, dest_addr, IR_OP_TY_CNST, IR_VAR_TY_NONE);
  mk_pointer_constant(func->unit, size, info.size);

  struct ir_glb *memmove =
      add_well_known_global(func->unit, IR_WELL_KNOWN_GLB_MEMMOVE);

  struct ir_var_ty ptr_int = var_ty_for_pointer_size(func->unit);
  struct ir_op *memmove_addr =
      insert_after_ir_op(func, size, IR_OP_TY_ADDR, ptr_int);
  memmove_addr->flags |= IR_OP_FLAG_CONTAINED;
  memmove_addr->addr = (struct ir_op_addr){
      .ty = IR_OP_ADDR_TY_GLB,
      .glb = memmove,
  };

  size_t num_args = 3;
  struct ir_op **args =
      arena_alloc(func->arena, sizeof(struct ir_op *) * num_args);

  args[0] = dest_addr;
  args[1] = source_addr;
  args[2] = size;

  op->ty = IR_OP_TY_CALL;
  op->var_ty = *memmove->var_ty.func.ret_ty;
  op->call = (struct ir_op_call){
      .target = memmove_addr,
      .num_args = num_args,
      .args = args,
      .func_ty = memmove->var_ty,
  };
}

//   // look for store after, in case this is a copy
//   // FIXME: not sure if this is perfect logic (could there be ops in
//   between?) struct ir_op *nxt_store = op->succ;

//   if (!nxt_store || nxt_store->ty != IR_OP_TY_STORE_LCL) {
//     return;
//   }

//   bool simple_copy = true;
//   enum ir_var_primitive_ty simple_copy_ty;
//   switch (info.size) {
//   case 1:
//     simple_copy_ty = IR_VAR_PRIMITIVE_TY_I8;
//     break;
//   case 2:
//     simple_copy_ty = IR_VAR_PRIMITIVE_TY_I16;
//     break;
//   case 4:
//     simple_copy_ty = IR_VAR_PRIMITIVE_TY_I32;
//     break;
//   case 8:
//     simple_copy_ty = IR_VAR_PRIMITIVE_TY_I64;
//     break;

//   default:
//     simple_copy = false;
//   }

//   if (simple_copy) {
//     op->var_ty = (struct ir_var_ty){.ty = IR_VAR_TY_TY_PRIMITIVE,
//                                     .primitive = simple_copy_ty};
//     return;
//   }

//   if (info.size < MAX_REG_SIZE) {
//     todo("non-pow2 copies < MAX_REG_SIZE");
//   }

//   struct ir_var_ty copy_ty = var_ty_for_pointer_size(func->unit);

//   struct ir_lcl *src_lcl = op->load_lcl.lcl;
//   struct ir_lcl *dest_lcl = nxt_store->store.lcl;

//   struct ir_op *base_src_addr = op;
//   struct ir_op *base_dest_addr = nxt_store;

//   base_src_addr->ty = IR_OP_TY_ADDR;
//   base_src_addr->var_ty = copy_ty;
//   base_src_addr->addr =
//       (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = src_lcl};

//   base_dest_addr->ty = IR_OP_TY_ADDR;
//   base_dest_addr->var_ty = copy_ty;
//   base_dest_addr->addr =
//       (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = dest_lcl};

//   struct ir_op *last = base_dest_addr;

//   size_t size_left = info.size;
//   size_t offset = 0;
//   while (size_left >= MAX_REG_SIZE) {
//     struct ir_op *offset_cnst =
//         insert_after_ir_op(func, last, IR_OP_TY_CNST, copy_ty);
//     make_pointer_constant(func->unit, offset_cnst, offset);

//     struct ir_op *src_addr =
//         insert_after_ir_op(func, offset_cnst, IR_OP_TY_BINARY_OP, copy_ty);
//     src_addr->binary_op = (struct ir_op_binary_op){
//         .ty = IR_OP_BINARY_OP_TY_ADD, .lhs = base_src_addr, .rhs =
//         offset_cnst};

//     struct ir_op *load =
//         insert_after_ir_op(func, src_addr, IR_OP_TY_LOAD_ADDR, copy_ty);
//     load->load_addr = (struct ir_op_load_addr){.addr = src_addr};

//     struct ir_op *dest_addr =
//         insert_after_ir_op(func, load, IR_OP_TY_BINARY_OP, copy_ty);
//     dest_addr->binary_op =
//         (struct ir_op_binary_op){.ty = IR_OP_BINARY_OP_TY_ADD,
//                                  .lhs = base_dest_addr,
//                                  .rhs = offset_cnst};

//     struct ir_op *store = insert_after_ir_op(
//         func, dest_addr, IR_OP_TY_STORE_ADDR, IR_VAR_TY_NONE);
//     store->store_addr =
//         (struct ir_op_store_addr){.addr = dest_addr, .value = load};

//     last = store;
//     size_left -= MAX_REG_SIZE;
//     offset += MAX_REG_SIZE;
//   }

//   // now we have to do the last trailing load
//   // because size is >= MAX_REG_SIZE,
//   // we can just do a whole-reg copy starting from end-MAX_REG_SIZE
//   if (size_left) {
//     size_t trailing_offset = MAX_REG_SIZE - size_left;

//     struct ir_op *offset_cnst =
//         insert_after_ir_op(func, last, IR_OP_TY_CNST, copy_ty);
//     make_pointer_constant(func->unit, offset_cnst, trailing_offset);

//     struct ir_op *src_addr =
//         insert_after_ir_op(func, offset_cnst, IR_OP_TY_BINARY_OP, copy_ty);
//     src_addr->binary_op = (struct ir_op_binary_op){
//         .ty = IR_OP_BINARY_OP_TY_ADD, .lhs = base_src_addr, .rhs =
//         offset_cnst};

//     struct ir_op *load =
//         insert_after_ir_op(func, src_addr, IR_OP_TY_LOAD_ADDR, copy_ty);
//     load->load_addr = (struct ir_op_load_addr){.addr = src_addr};

//     struct ir_op *dest_addr =
//         insert_after_ir_op(func, load, IR_OP_TY_BINARY_OP, copy_ty);
//     dest_addr->binary_op =
//         (struct ir_op_binary_op){.ty = IR_OP_BINARY_OP_TY_ADD,
//                                  .lhs = base_dest_addr,
//                                  .rhs = offset_cnst};

//     struct ir_op *store = insert_after_ir_op(
//         func, dest_addr, IR_OP_TY_STORE_ADDR, IR_VAR_TY_NONE);
//     store->store_addr =
//         (struct ir_op_store_addr){.addr = dest_addr, .value = load};
//   }
// }

static void lower_fp_cnst(struct ir_func *func, struct ir_op *op) {
  // transform into creating an integer, and then mov to float reg

  struct ir_var_ty int_ty;
  unsigned long long int_value;

  DEBUG_ASSERT(var_ty_is_fp(&op->var_ty), "float constant not fp type?");

  switch (op->var_ty.primitive) {
  case IR_VAR_PRIMITIVE_TY_F32: {
    int_ty = IR_VAR_TY_I32;

    union {
      float f;
      unsigned u;
    } v;
    v.f = (float)op->cnst.flt_value;
    int_value = v.u;

    break;
  }
  case IR_VAR_PRIMITIVE_TY_F64: {
    int_ty = IR_VAR_TY_I64;

    union {
      double d;
      unsigned long long ull;
    } v;
    v.d = (double)op->cnst.flt_value;
    int_value = v.ull;

    break;
  }
  default:
    unreachable();
  }

  struct ir_op *int_mov = insert_before_ir_op(func, op, IR_OP_TY_CNST, int_ty);
  int_mov->cnst =
      (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = int_value};

  op->ty = IR_OP_TY_MOV;
  op->mov = (struct ir_op_mov){.value = int_mov};
}

static void lower_load_to_addr(struct ir_op *op) {
  switch (op->load.ty) {
  case IR_OP_LOAD_TY_LCL: {
    struct ir_lcl *lcl = op->load.lcl;

    op->ty = IR_OP_TY_ADDR;
    op->var_ty = IR_VAR_TY_I64;
    op->addr = (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = lcl};
    break;
  }
  case IR_OP_LOAD_TY_ADDR: {
    struct ir_op *addr = op->load.addr;

    op->ty = IR_OP_TY_MOV;
    op->var_ty = IR_VAR_TY_I64;
    op->mov = (struct ir_op_mov){.value = addr};
    break;
  }
  case IR_OP_LOAD_TY_GLB:
    BUG("load.glb should be gone by now");
  }
}

static void lower_call(struct ir_func *func, struct ir_op *op) {
  for (size_t i = 0; i < op->call.num_args; i++) {
    struct ir_op *arg = op->call.args[i];

    if ((arg->ty != IR_OP_TY_LOAD) || !var_ty_is_aggregate(&arg->var_ty)) {
      continue;
    }

    lower_load_to_addr(arg);
  }

  if (var_ty_is_aggregate(&op->var_ty)) {
    if (op->succ && op->succ->ty == IR_OP_TY_STORE &&
        op->succ->store.ty == IR_OP_STORE_TY_LCL &&
        op->succ->store.value == op) {
      op->lcl = op->succ->store.lcl;
      detach_ir_op(func, op->succ);
    }

    // just switch to pointer type, let codegen handle
    op->var_ty = IR_VAR_TY_I64;
  }
}

static void lower_ret(UNUSED struct ir_func *func, struct ir_op *op) {
  if (!op->ret.value) {
    return;
  }

  struct ir_op *value = op->ret.value;

  if (value->ty != IR_OP_TY_LOAD || !var_ty_is_aggregate(&value->var_ty)) {
    return;
  }

  lower_load_to_addr(value);
}

void x64_lower(struct ir_unit *unit) {
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
      struct ir_func *func = glb->func;

      struct ir_basicblock *basicblock = func->first;
      while (basicblock) {
        struct ir_stmt *stmt = basicblock->first;

        while (stmt) {
          struct ir_op *op = stmt->first;

          while (op) {
            switch (op->ty) {
            case IR_OP_TY_UNKNOWN:
              BUG("unknown op!");
            case IR_OP_TY_UNDF:
            case IR_OP_TY_CUSTOM:
            case IR_OP_TY_PHI:
              break;
            case IR_OP_TY_RET:
              lower_ret(func, op);
              break;
            case IR_OP_TY_CNST: {
              if (op->cnst.ty == IR_OP_CNST_TY_FLT) {
                lower_fp_cnst(func, op);
                break;
              }

              break;
            }
            case IR_OP_TY_STORE:
              lower_store(func, op);
              break;
            case IR_OP_TY_LOAD:
            case IR_OP_TY_ADDR:
            case IR_OP_TY_BR_SWITCH:
            case IR_OP_TY_BR:
            case IR_OP_TY_BR_COND:
            case IR_OP_TY_MOV:
            case IR_OP_TY_CAST_OP:
            case IR_OP_TY_STORE_BITFIELD:
            case IR_OP_TY_LOAD_BITFIELD:
              break;
            case IR_OP_TY_BITFIELD_EXTRACT:
              lower_bitfield_extract(func, op);
              break;
            case IR_OP_TY_BITFIELD_INSERT:
              lower_bitfield_insert(func, op);
              break;
            case IR_OP_TY_CALL:
              if (op->call.target->ty == IR_OP_TY_ADDR &&
                  op->call.target->addr.ty == IR_OP_ADDR_TY_GLB &&
                  !(op->call.target->flags & IR_OP_FLAG_CONTAINED)) {
                op->call.target =
                    alloc_contained_ir_op(func, op->call.target, op);
              }

              lower_call(func, op);
              break;
            case IR_OP_TY_UNARY_OP:
              if (op->binary_op.ty == IR_OP_UNARY_OP_TY_LOGICAL_NOT) {
                lower_logical_not(func, op);
              }
              break;
            case IR_OP_TY_BINARY_OP:
              switch (op->binary_op.ty) {
              case IR_OP_BINARY_OP_TY_SRSHIFT:
              case IR_OP_BINARY_OP_TY_LSHIFT:
              case IR_OP_BINARY_OP_TY_URSHIFT:
                lower_shift(func, op);
                break;
              default:
                break;
              }
              break;
            default:
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
              BUG("unknown op!");
            case IR_OP_TY_UNDF:
            case IR_OP_TY_CUSTOM:
            case IR_OP_TY_PHI:
            case IR_OP_TY_CNST:
            case IR_OP_TY_STORE:
              // try_contain_store(func, op);
              break;
            case IR_OP_TY_LOAD:
              // try_contain_load(func, op);
              break;
            case IR_OP_TY_STORE_BITFIELD:
            case IR_OP_TY_LOAD_BITFIELD:
            case IR_OP_TY_BITFIELD_EXTRACT:
            case IR_OP_TY_BITFIELD_INSERT:
            case IR_OP_TY_ADDR:
            case IR_OP_TY_BR:
            case IR_OP_TY_BR_COND:
            case IR_OP_TY_BR_SWITCH:
            case IR_OP_TY_MOV:
            case IR_OP_TY_RET:
            case IR_OP_TY_CALL:
            case IR_OP_TY_CAST_OP:
            case IR_OP_TY_MEM_SET:
              break;
            case IR_OP_TY_BINARY_OP:
              switch (op->binary_op.ty) {
              case IR_OP_BINARY_OP_TY_LSHIFT:
              case IR_OP_BINARY_OP_TY_SRSHIFT:
              case IR_OP_BINARY_OP_TY_URSHIFT:
                op->flags |= IR_OP_FLAG_READS_DEST;
                alloc_fixed_reg_dest_ir_op(
                    func, &op->binary_op.rhs, op,
                    (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                    .idx = IR_REG_IDX_CX});
                break;
              case IR_OP_BINARY_OP_TY_AND:
              case IR_OP_BINARY_OP_TY_OR:
              case IR_OP_BINARY_OP_TY_XOR:
              case IR_OP_BINARY_OP_TY_ADD:
              case IR_OP_BINARY_OP_TY_SUB:
                // TODO: only do this where actually applicable
                op->flags |= IR_OP_FLAG_READS_DEST;
                break;
              case IR_OP_BINARY_OP_TY_MUL:
              case IR_OP_BINARY_OP_TY_SDIV:
              case IR_OP_BINARY_OP_TY_UDIV:
                op->flags |= IR_OP_FLAG_READS_DEST;

                op->write_info = (struct ir_op_write_info){
                    .num_reg_writes = 1,
                    .writes[0] = {.ty = IR_REG_TY_INTEGRAL,
                                  .idx = IR_REG_IDX_DX},
                };

                alloc_fixed_reg_dest_ir_op(
                    func, &op->binary_op.lhs, op,
                    (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                    .idx = IR_REG_IDX_AX});
                alloc_fixed_reg_source_ir_op(
                    func, op,
                    (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                    .idx = IR_REG_IDX_AX});

                break;
              case IR_OP_BINARY_OP_TY_SQUOT:
              case IR_OP_BINARY_OP_TY_UQUOT:
                op->flags |= IR_OP_FLAG_READS_DEST;
                alloc_fixed_reg_dest_ir_op(
                    func, &op->binary_op.lhs, op,
                    (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                    .idx = IR_REG_IDX_AX});
                alloc_fixed_reg_source_ir_op(
                    func, op,
                    (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                    .idx = IR_REG_IDX_DX});

                break;
              default:
                if (binary_op_is_comparison(op->binary_op.ty)) {
                  lower_comparison(func, op);
                }
                break;
              }

              break;
            case IR_OP_TY_UNARY_OP:
              switch (op->unary_op.ty) {
              case IR_OP_UNARY_OP_TY_NEG:
              case IR_OP_UNARY_OP_TY_LOGICAL_NOT:
              case IR_OP_UNARY_OP_TY_NOT:
                op->flags |= IR_OP_FLAG_READS_DEST;
                break;
              default:
                break;
              }
              break;
            case IR_OP_TY_ADDR_OFFSET:
              if (op->addr_offset.index && popcntl(op->addr_offset.scale) != 1) {
                // do mul beforehand and set scale to 1
                struct ir_op *cnst = insert_before_ir_op(
                    func, op, IR_OP_TY_BINARY_OP, IR_VAR_TY_POINTER);
                mk_integral_constant(unit, cnst, IR_VAR_PRIMITIVE_TY_I64,
                                     op->addr_offset.scale);

                struct ir_op *mul = insert_before_ir_op(
                    func, op, IR_OP_TY_BINARY_OP, IR_VAR_TY_POINTER);
                mul->binary_op =
                    (struct ir_op_binary_op){.ty = IR_OP_BINARY_OP_TY_MUL,
                                             .lhs = op->addr_offset.index,
                                             .rhs = cnst};

                mul->flags |= IR_OP_FLAG_READS_DEST;

                mul->write_info = (struct ir_op_write_info){
                    .num_reg_writes = 1,
                    .writes[0] = {.ty = IR_REG_TY_INTEGRAL,
                                  .idx = IR_REG_IDX_DX},
                };

                alloc_fixed_reg_dest_ir_op(
                    func, &mul->binary_op.lhs, mul,
                    (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                    .idx = IR_REG_IDX_AX});
                alloc_fixed_reg_source_ir_op(
                    func, mul,
                    (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                    .idx = IR_REG_IDX_AX});

                op->addr_offset.scale = 1;
                op->addr_offset.index = mul;
              }
              break;
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
