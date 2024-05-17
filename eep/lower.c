#include "lower.h"

#include "../ir/build.h"
#include "../util.h"

// EEP has no mul, div, or quot functions
// mul:
//   `
// quot:
//   `x = a % b` -> `c = a / b; x = a - (c * b)`
static void lower_mul(struct ir_builder *func, struct ir_op *op) {
  struct ir_basicblock *orig_bb = op->stmt->basicblock;  

  struct ir_op *op1 = op->binary_op.lhs;
  struct ir_op *op2 = op->binary_op.rhs;

  // TODO: assert same ty
  struct ir_op_var_ty var_ty = op1->var_ty;

  //   T op1, op2, op1_shifted, op2_shifted, sum;
  //   sum = 0;
  //   op1_shifted = op1;
  //   op2_shifted = op2;
  //
  //
  //   while (op1_shifted) {
  //     if (op1_shifted & 1) {
  //       sum = sum + op2_shifted;
  //     }
  //     op2_shifted = op2_shifted << 1;
  //     op1_shifted = op1_shifted >> 1;
  //   }
  //   return sum;

  // 0

  struct ir_basicblock *while_cond_bb = alloc_ir_basicblock(func);
  struct ir_stmt *while_cond_stmt = alloc_ir_stmt(func, while_cond_bb);

  struct ir_basicblock *if_cond_bb = alloc_ir_basicblock(func);
  struct ir_stmt *if_cond_stmt = alloc_ir_stmt(func, if_cond_bb);

  struct ir_basicblock *if_body_bb = alloc_ir_basicblock(func);
  struct ir_stmt *if_body_stmt = alloc_ir_stmt(func, if_body_bb);

  struct ir_basicblock *shift_bb = alloc_ir_basicblock(func);
  struct ir_stmt *shift_stmt = alloc_ir_stmt(func, shift_bb);

  struct ir_basicblock *end_bb = insert_basicblocks_after(func, op, while_cond_bb);
  end_bb->num_preds = 1;
  end_bb->preds = arena_alloc(func->arena, sizeof(struct ir_basicblock *));
  end_bb->preds[0] = while_cond_bb;

  while_cond_bb->ty = IR_BASICBLOCK_TY_SPLIT;
  while_cond_bb->split.true_target = if_cond_bb;
  while_cond_bb->split.false_target = end_bb;

  if_cond_bb->ty = IR_BASICBLOCK_TY_SPLIT;
  if_cond_bb->split.true_target = if_body_bb;
  if_cond_bb->split.false_target = shift_bb;

  if_body_bb->ty = IR_BASICBLOCK_TY_MERGE;
  if_body_bb->merge.target = shift_bb;

  shift_bb->ty = IR_BASICBLOCK_TY_MERGE;
  shift_bb->merge.target = while_cond_bb;


  // struct ir_op *cnst_0 = alloc_ir_op(func, op->stmt);
  struct ir_op *cnst_0 = op;
  cnst_0->ty = IR_OP_TY_CNST;
  cnst_0->var_ty = var_ty;
  cnst_0->cnst.ty = IR_OP_CNST_TY_INT;
  cnst_0->cnst.int_value = 0;

  // 1
  struct ir_op *cnst_1 = alloc_ir_op(func, if_cond_stmt);
  cnst_1->ty = IR_OP_TY_CNST;
  cnst_1->var_ty = var_ty;
  cnst_1->cnst.ty = IR_OP_CNST_TY_INT;
  cnst_1->cnst.int_value = 1;

  struct ir_op *op1_shifted = alloc_ir_op(func, while_cond_stmt);
  struct ir_op *op1_shifted_r1 = alloc_ir_op(func, shift_stmt);
  op1_shifted->ty = IR_OP_TY_PHI;
  op1_shifted->var_ty = var_ty;
  op1_shifted->phi.num_values = 2;
  op1_shifted->phi.values = arena_alloc(func->arena, sizeof(struct ir_op *) * op1_shifted->phi.num_values);
  op1_shifted->phi.values[0] = op1;
  op1_shifted->phi.values[1] = op1_shifted_r1;

  op1_shifted_r1->ty = IR_OP_TY_BINARY_OP;
  op1_shifted_r1->var_ty = var_ty;
  // FIXME: arith/logic?
  op1_shifted_r1->binary_op.ty = IR_OP_BINARY_OP_TY_URSHIFT;
  op1_shifted_r1->binary_op.lhs = op1_shifted;
  op1_shifted_r1->binary_op.rhs = cnst_1;

  struct ir_op *op2_shifted = alloc_ir_op(func, if_cond_stmt);
  struct ir_op *op2_shifted_l1 = alloc_ir_op(func, shift_stmt);
  op2_shifted->ty = IR_OP_TY_PHI;
  op2_shifted->var_ty = var_ty;
  op2_shifted->phi.num_values = 2;
  op2_shifted->phi.values = arena_alloc(func->arena, sizeof(struct ir_op *) * op2_shifted->phi.num_values);
  op2_shifted->phi.values[0] = op2;
  op2_shifted->phi.values[1] = op2_shifted_l1;

  op2_shifted_l1->ty = IR_OP_TY_BINARY_OP;
  op2_shifted_l1->var_ty = var_ty;
  op2_shifted_l1->binary_op.ty = IR_OP_BINARY_OP_TY_LSHIFT;
  op2_shifted_l1->binary_op.lhs = op2_shifted;
  op2_shifted_l1->binary_op.rhs = cnst_1;

  struct ir_op *while_br_cond = alloc_ir_op(func, while_cond_stmt);
  while_br_cond->ty = IR_OP_TY_BR_COND;
  while_br_cond->var_ty = IR_OP_VAR_TY_NONE;
  while_br_cond->br_cond.cond = op1_shifted;
  
  // op1 & 1
  struct ir_op *if_cond = alloc_ir_op(func, if_cond_stmt);
  if_cond->ty = IR_OP_TY_BINARY_OP;
  if_cond->var_ty = var_ty;
  if_cond->binary_op.ty = IR_OP_BINARY_OP_TY_AND;
  if_cond->binary_op.lhs = op1_shifted;
  if_cond->binary_op.rhs = cnst_1;

  // if (op1 & 1)
  struct ir_op *if_br_cond = alloc_ir_op(func, if_cond_stmt);
  if_br_cond->ty = IR_OP_TY_BR_COND;
  if_br_cond->var_ty = IR_OP_VAR_TY_NONE;
  if_br_cond->br_cond.cond = if_cond;
    
  struct ir_op *sum = alloc_ir_op(func, if_body_stmt);
  struct ir_op *sum_plus_op2shifted = alloc_ir_op(func, if_body_stmt);

  // sum
  sum->ty = IR_OP_TY_PHI;
  sum->var_ty = var_ty;
  sum->phi.num_values = 2;
  sum->phi.values = arena_alloc(func->arena, sizeof(struct ir_op *) * op1_shifted->phi.num_values);
  sum->phi.values[0] = cnst_0;
  sum->phi.values[1] = sum_plus_op2shifted;

  // sum + op2_shifted
  sum_plus_op2shifted->ty = IR_OP_TY_BINARY_OP;
  sum_plus_op2shifted->var_ty = var_ty;
  sum_plus_op2shifted->binary_op.ty = IR_OP_BINARY_OP_TY_ADD;
  sum_plus_op2shifted->binary_op.lhs = sum;
  sum_plus_op2shifted->binary_op.rhs = op2_shifted;

  struct ir_op *if_bb_br = alloc_ir_op(func, if_body_stmt);
  if_bb_br->ty = IR_OP_TY_BR;

  struct ir_op *shift_bb_br = alloc_ir_op(func, shift_stmt);
  shift_bb_br->ty = IR_OP_TY_BR;

  struct ir_op *orig_bb_br = alloc_ir_op(func, orig_bb->last);
  orig_bb_br->ty = IR_OP_TY_BR;
}

static void lower_div(struct ir_builder *func, struct ir_op *op) {
  struct ir_basicblock *orig_bb = op->stmt->basicblock;  

  struct ir_op *op1 = op->binary_op.lhs;
  struct ir_op *op2 = op->binary_op.rhs;

  // TODO: assert same ty
  struct ir_op_var_ty var_ty = op1->var_ty;

  //     if (divisor == 0) {
  //         return INT_MAX; // handle division by zero
  //     }
    
  //     int sign = ((dividend < 0) ^ (divisor < 0)) ? -1 : 1;
    
  //     T absDividend = abs(dividend);
  //     T absDivisor = abs(divisor);
  //     T temp = 1;
  //     T quotient = 0;
    
  //     while (absDividend >= absDivisor) {
  //         absDivisor <<= 1;
  //         temo <<= 1;
  //     }

  //     while (temp > 1) {
  //         absDivisor >>= 1;
  //         temp >>= 1;

  //         if (absDividend >= absDivisor) {
  //             absDivident -= absDivisor;
  //             quotient += temp;
  //         }
  //     }
    
  //     return sign * quotient;

  // 0

  // struct ir_basicblock *div_zero_cond_bb = alloc_ir_basicblock(func);
  // struct ir_stmt *div_zero_cond_stmt = alloc_ir_stmt(func, div_zero_cond_bb);

  // struct ir_basicblock *div_zero_body_bb = alloc_ir_basicblock(func);
  // struct ir_stmt *div_zero_body_stmt = alloc_ir_stmt(func, div_zero_body_bb);

  

  struct ir_basicblock *while_cond_bb = alloc_ir_basicblock(func);
  struct ir_stmt *while_cond_stmt = alloc_ir_stmt(func, while_cond_bb);

  struct ir_basicblock *if_cond_bb = alloc_ir_basicblock(func);
  struct ir_stmt *if_cond_stmt = alloc_ir_stmt(func, if_cond_bb);

  struct ir_basicblock *if_body_bb = alloc_ir_basicblock(func);
  struct ir_stmt *if_body_stmt = alloc_ir_stmt(func, if_body_bb);

  struct ir_basicblock *shift_bb = alloc_ir_basicblock(func);
  struct ir_stmt *shift_stmt = alloc_ir_stmt(func, shift_bb);

  struct ir_basicblock *end_bb = insert_basicblocks_after(func, op, while_cond_bb);
  end_bb->num_preds = 1;
  end_bb->preds = arena_alloc(func->arena, sizeof(struct ir_basicblock *));
  end_bb->preds[0] = while_cond_bb;

  while_cond_bb->ty = IR_BASICBLOCK_TY_SPLIT;
  while_cond_bb->split.true_target = if_cond_bb;
  while_cond_bb->split.false_target = end_bb;

  if_cond_bb->ty = IR_BASICBLOCK_TY_SPLIT;
  if_cond_bb->split.true_target = if_body_bb;
  if_cond_bb->split.false_target = shift_bb;

  if_body_bb->ty = IR_BASICBLOCK_TY_MERGE;
  if_body_bb->merge.target = shift_bb;

  shift_bb->ty = IR_BASICBLOCK_TY_MERGE;
  shift_bb->merge.target = while_cond_bb;


  // struct ir_op *cnst_0 = alloc_ir_op(func, op->stmt);
  struct ir_op *cnst_0 = op;
  cnst_0->ty = IR_OP_TY_CNST;
  cnst_0->var_ty = var_ty;
  cnst_0->cnst.ty = IR_OP_CNST_TY_INT;
  cnst_0->cnst.int_value = 0;

  // 1
  struct ir_op *cnst_1 = alloc_ir_op(func, if_cond_stmt);
  cnst_1->ty = IR_OP_TY_CNST;
  cnst_1->var_ty = var_ty;
  cnst_1->cnst.ty = IR_OP_CNST_TY_INT;
  cnst_1->cnst.int_value = 1;

  struct ir_op *op1_shifted = alloc_ir_op(func, while_cond_stmt);
  struct ir_op *op1_shifted_r1 = alloc_ir_op(func, shift_stmt);
  op1_shifted->ty = IR_OP_TY_PHI;
  op1_shifted->var_ty = var_ty;
  op1_shifted->phi.num_values = 2;
  op1_shifted->phi.values = arena_alloc(func->arena, sizeof(struct ir_op *) * op1_shifted->phi.num_values);
  op1_shifted->phi.values[0] = op1;
  op1_shifted->phi.values[1] = op1_shifted_r1;

  op1_shifted_r1->ty = IR_OP_TY_BINARY_OP;
  op1_shifted_r1->var_ty = var_ty;
  // FIXME: arith/logic?
  op1_shifted_r1->binary_op.ty = IR_OP_BINARY_OP_TY_URSHIFT;
  op1_shifted_r1->binary_op.lhs = op1_shifted;
  op1_shifted_r1->binary_op.rhs = cnst_1;

  struct ir_op *op2_shifted = alloc_ir_op(func, if_cond_stmt);
  struct ir_op *op2_shifted_l1 = alloc_ir_op(func, shift_stmt);
  op2_shifted->ty = IR_OP_TY_PHI;
  op2_shifted->var_ty = var_ty;
  op2_shifted->phi.num_values = 2;
  op2_shifted->phi.values = arena_alloc(func->arena, sizeof(struct ir_op *) * op2_shifted->phi.num_values);
  op2_shifted->phi.values[0] = op2;
  op2_shifted->phi.values[1] = op2_shifted_l1;

  op2_shifted_l1->ty = IR_OP_TY_BINARY_OP;
  op2_shifted_l1->var_ty = var_ty;
  op2_shifted_l1->binary_op.ty = IR_OP_BINARY_OP_TY_LSHIFT;
  op2_shifted_l1->binary_op.lhs = op2_shifted;
  op2_shifted_l1->binary_op.rhs = cnst_1;

  struct ir_op *while_br_cond = alloc_ir_op(func, while_cond_stmt);
  while_br_cond->ty = IR_OP_TY_BR_COND;
  while_br_cond->var_ty = IR_OP_VAR_TY_NONE;
  while_br_cond->br_cond.cond = op1_shifted;
  
  // op1 & 1
  struct ir_op *if_cond = alloc_ir_op(func, if_cond_stmt);
  if_cond->ty = IR_OP_TY_BINARY_OP;
  if_cond->var_ty = var_ty;
  if_cond->binary_op.ty = IR_OP_BINARY_OP_TY_AND;
  if_cond->binary_op.lhs = op1_shifted;
  if_cond->binary_op.rhs = cnst_1;

  // if (op1 & 1)
  struct ir_op *if_br_cond = alloc_ir_op(func, if_cond_stmt);
  if_br_cond->ty = IR_OP_TY_BR_COND;
  if_br_cond->var_ty = IR_OP_VAR_TY_NONE;
  if_br_cond->br_cond.cond = if_cond;
    
  struct ir_op *sum = alloc_ir_op(func, if_body_stmt);
  struct ir_op *sum_plus_op2shifted = alloc_ir_op(func, if_body_stmt);

  // sum
  sum->ty = IR_OP_TY_PHI;
  sum->var_ty = var_ty;
  sum->phi.num_values = 2;
  sum->phi.values = arena_alloc(func->arena, sizeof(struct ir_op *) * op1_shifted->phi.num_values);
  sum->phi.values[0] = cnst_0;
  sum->phi.values[1] = sum_plus_op2shifted;

  // sum + op2_shifted
  sum_plus_op2shifted->ty = IR_OP_TY_BINARY_OP;
  sum_plus_op2shifted->var_ty = var_ty;
  sum_plus_op2shifted->binary_op.ty = IR_OP_BINARY_OP_TY_ADD;
  sum_plus_op2shifted->binary_op.lhs = sum;
  sum_plus_op2shifted->binary_op.rhs = op2_shifted;

  struct ir_op *if_bb_br = alloc_ir_op(func, if_body_stmt);
  if_bb_br->ty = IR_OP_TY_BR;

  struct ir_op *shift_bb_br = alloc_ir_op(func, shift_stmt);
  shift_bb_br->ty = IR_OP_TY_BR;

  struct ir_op *orig_bb_br = alloc_ir_op(func, orig_bb->last);
  orig_bb_br->ty = IR_OP_TY_BR;
}

static void lower_shift(struct ir_builder *func, struct ir_op *op) {
  UNUSED_ARG(func);

  debug_assert(op->ty == IR_OP_TY_BINARY_OP &&
                   (op->binary_op.ty == IR_OP_BINARY_OP_TY_LSHIFT ||
                    op->binary_op.ty == IR_OP_BINARY_OP_TY_SRSHIFT ||
                    op->binary_op.ty == IR_OP_BINARY_OP_TY_URSHIFT),
               "lower_shift called on invalid op");

  if (op->binary_op.rhs->ty != IR_OP_TY_CNST) {
    todo("EEP does not support shifting by variable amounts (yet...)");
    // FIXME: exit here or smth
    return;
  }

  // does not need a reg as will be inserted into instruction
  // FIXME: this wrongly marks regs used by multiple things as not needing reg
  // op->binary_op.rhs->reg = DONT_GIVE_REG;
}

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

// AArch64 requires turning `br.cond <true> <false>` into 2 instructions
// we represent this as just the `true` part of the `br.cond`, and then a `br`
// after branching to the false target
static void lower_br_cond(struct ir_builder *irb, struct ir_op *op) {
  // ldr/str don't write to flags, so insert `mov <reg>, <reg>` to get flags
  // phi does not guarantee ldr, but the optimiser can always remove it later
  // if (op->br_cond.cond->ty == IR_OP_TY_PHI) {
    struct ir_op *mov =
        insert_before_ir_op(irb, op, IR_OP_TY_MOV, IR_OP_VAR_TY_NONE);
    mov->mov.value = op->br_cond.cond;
    op->br_cond.cond = mov;
  // }

  insert_after_ir_op(irb, op, IR_OP_TY_BR, IR_OP_VAR_TY_NONE);
}

static void lower_comparison(struct ir_builder *irb, struct ir_op *op) {
  UNUSED_ARG(irb);

  return;
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

enum eep_lower_stage { EEP_LOWER_STAGE_QUOT, EEP_LOWER_STAGE_ALL };

void eep_pre_reg_lower(struct ir_builder *func) {
  for (enum eep_lower_stage stage = EEP_LOWER_STAGE_QUOT;
       stage <= EEP_LOWER_STAGE_ALL; stage++) {

    struct ir_basicblock *basicblock = func->first;
    while (basicblock) {
      struct ir_stmt *stmt = basicblock->first;

      while (stmt) {
        struct ir_op *op = stmt->first;

        while (op) {
          if (stage == EEP_LOWER_STAGE_QUOT) {
            if (op->ty == IR_OP_TY_BINARY_OP &&
                (op->binary_op.ty == IR_OP_BINARY_OP_TY_UQUOT ||
                 op->binary_op.ty == IR_OP_BINARY_OP_TY_SQUOT)) {
              lower_quot(func, op);
            }
          } else {

            switch (op->ty) {
            case IR_OP_TY_CUSTOM:
            case IR_OP_TY_GLB:
            case IR_OP_TY_PHI:
            case IR_OP_TY_CNST:
            case IR_OP_TY_RET:
            case IR_OP_TY_STORE_LCL:
            case IR_OP_TY_LOAD_LCL:
            case IR_OP_TY_BR:
            case IR_OP_TY_MOV:
            case IR_OP_TY_UNARY_OP:
            case IR_OP_TY_CAST_OP:
              break;
            case IR_OP_TY_CALL:
              todo("call");
              break;
            case IR_OP_TY_BR_COND:
              if (!(op->succ && op->succ->ty == IR_OP_TY_BR)) {
                lower_br_cond(func, op);
              }
              break;
            case IR_OP_TY_BINARY_OP:
              if (op->binary_op.ty == IR_OP_BINARY_OP_TY_UDIV ||
                  op->binary_op.ty == IR_OP_BINARY_OP_TY_SDIV) {
                lower_div(func, op);
              } else if (op->binary_op.ty == IR_OP_BINARY_OP_TY_LSHIFT ||
                         op->binary_op.ty == IR_OP_BINARY_OP_TY_SRSHIFT ||
                         op->binary_op.ty == IR_OP_BINARY_OP_TY_URSHIFT) {
                lower_shift(func, op);
              } else if (op->binary_op.ty == IR_OP_BINARY_OP_TY_MUL) {
                lower_mul(func, op);
              } else if (binary_op_is_comparison(op->binary_op.ty)) {
                lower_comparison(func, op);
              }
            }
          }

          op = op->succ;
        }

        stmt = stmt->succ;
      }

      basicblock = basicblock->succ;
    }
  }
}
