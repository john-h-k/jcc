#include "lower.h"

#include "../aarch64.h"
#include "../bit_twiddle.h"
#include "../ir/build.h"
#include "../util.h"

static void lower_call(struct ir_builder *func, struct ir_op *op) {
  invariant_assert(op->call.target->var_ty.ty == IR_OP_VAR_TY_TY_FUNC,
                   "non-func in `lower_call`");

  struct ir_op_var_func_ty *func_ty = &op->call.target->var_ty.func;

  invariant_assert(func_ty->num_params == op->call.num_args,
                   "mismatch of function param and arg count");

  invariant_assert(func_ty->num_params <= 8,
                   "`lower_call` doesn't support more than 8 args yet");

  // we need to save registers in-use _after_ call
  unsigned long live_regs = op->succ ? op->succ->live_regs : 0;
  // FIXME: don't hardcode volatile count
  unsigned long live_volatile = live_regs & ((1 << 18) - 1);

  // remove the call-return reg from live-volatile as we know it will be over written by end of cal
  if (func_ty->ret_ty->ty != IR_OP_VAR_TY_TY_NONE) {
    live_volatile &= ~(1 << op->reg);
  }

  size_t max_volatile = sizeof(live_volatile) * 8 - lzcnt(live_volatile);

  unsigned caller_saves = popcntl(live_volatile);
  unsigned new_slots = caller_saves > func->num_call_saves ? caller_saves - func->num_call_saves : 0;

  // TODO: we assume all saves are 8 bytes
  func->total_call_saves_size += 8 * new_slots;
  func->total_locals_size += 8 * new_slots;
  func->num_locals += new_slots;

  // call slots always at bottom
  unsigned next_call_slot = func->num_locals - func->num_call_saves;

  for (size_t i = 0; i < max_volatile; i++) {
    if (!NTH_BIT(live_volatile, i)) {
      continue;
    }
    unsigned call_slot = next_call_slot++;

    struct ir_op *save =
        insert_before_ir_op(func, op, IR_OP_TY_CUSTOM, IR_OP_VAR_TY_NONE);
    save->custom.aarch64 =
        arena_alloc(func->arena, sizeof(*save->custom.aarch64));
    save->custom.aarch64->ty = AARCH64_OP_TY_SAVE_REG;
    save->reg = i;
    save->lcl_idx = call_slot;

    struct ir_op *restore =
        insert_after_ir_op(func, op, IR_OP_TY_CUSTOM, IR_OP_VAR_TY_NONE);
    restore->custom.aarch64 =
        arena_alloc(func->arena, sizeof(*restore->custom.aarch64));
    restore->custom.aarch64->ty = AARCH64_OP_TY_RSTR_REG;
    restore->reg = i;
    restore->lcl_idx = call_slot;
  }

  // FIXME: generalise this to calling conventions
  for (size_t i = 0; i < func_ty->num_params; i++) {
    struct ir_op_var_ty *param = &func_ty->params[i];

    invariant_assert(param->ty == IR_OP_VAR_TY_TY_PRIMITIVE,
                     "`lower_call` doesn't support non-prims");

    size_t arg_reg = i;
    if (op->call.args[i]->reg != arg_reg) {
      struct ir_op *mov = insert_before_ir_op(func, op, IR_OP_TY_MOV, *param);
      mov->mov.value = op->call.args[i];
      mov->reg = arg_reg;
    }
  }


  // FIXME: don't hardcode return reg
  size_t return_reg = 0;
  if (func_ty->ret_ty->ty != IR_OP_VAR_TY_TY_NONE && op->reg != return_reg) {
    size_t target_reg = op->reg;

    // we move the call _back_ and replace it with a mov, by swapping them
    struct ir_op *new_call = insert_before_ir_op(func, op, IR_OP_TY_CALL, op->var_ty);
    new_call->call = op->call;
    new_call->reg = return_reg;

    op->ty = IR_OP_TY_MOV;
    op->mov.value = new_call;
    op->reg = target_reg;
  }
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

// AArch64 requires turning `br.cond <true> <false>` into 2 instructions
// we represent this as just the `true` part of the `br.cond`, and then a `br`
// after branching to the false target
static void lower_br_cond(struct ir_builder *irb, struct ir_op *op) {
  insert_after_ir_op(irb, op, IR_OP_TY_BR, IR_OP_VAR_TY_NONE);
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

void aarch64_pre_reg_lower(struct ir_builder *func) {
  struct ir_basicblock *basicblock = func->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;

    while (stmt) {
      struct ir_op *op = stmt->first;

      while (op) {
        switch (op->ty) {
        case IR_OP_TY_CUSTOM:
        case IR_OP_TY_GLB:
        case IR_OP_TY_PHI:
        case IR_OP_TY_CNST:
        case IR_OP_TY_STORE_LCL:
        case IR_OP_TY_LOAD_LCL:
        case IR_OP_TY_BR:
        case IR_OP_TY_MOV:
        case IR_OP_TY_UNARY_OP:
        case IR_OP_TY_RET:
        case IR_OP_TY_CALL:
        case IR_OP_TY_CAST_OP:
          break;
        case IR_OP_TY_BR_COND:
          if (!(op->succ && op->succ->ty == IR_OP_TY_BR)) {
            lower_br_cond(func, op);
          }
          break;
        case IR_OP_TY_BINARY_OP:
          if (op->binary_op.ty == IR_OP_BINARY_OP_TY_UQUOT ||
              op->binary_op.ty == IR_OP_BINARY_OP_TY_SQUOT) {
            lower_quot(func, op);
          } else if (binary_op_is_comparison(op->binary_op.ty)) {
            lower_comparison(func, op);
          }
        }

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }
}

void aarch64_post_reg_lower(struct ir_builder *func) {
  // TODO: hacky improve this system so it isn't a bunch of magic values
  // signalling to the emitter might need new IR layer

  bool leaf = !(func->nonvolatile_registers_used || func->num_locals ||
                func->flags & IR_BUILDER_FLAG_MAKES_CALL);

  // points to first save so we can use it for restore later
  struct ir_op *saves = NULL;
  if (!leaf) {
    struct ir_op *first_op = func->first->first->first;

    // for x29 and x30 as they aren't working yet with pre-indexing
    func->total_locals_size += 16;

    unsigned long max_nonvol_used = sizeof(func->nonvolatile_registers_used) * 8 - lzcnt(func->nonvolatile_registers_used);
    unsigned long num_saves = popcntl(func->nonvolatile_registers_used);
    // FIXME: assumes 8 bytes
    // unsigned first_slot = func->num_locals;
    unsigned first_slot = (func->total_locals_size + 7) / 8;

    func->num_locals += num_saves;
    func->total_locals_size += num_saves * 8;
    
    for (size_t i = 0; i < max_nonvol_used; i++) {
      // FIXME: loop should start at i=first non volatile
      if (!NTH_BIT(func->nonvolatile_registers_used, i)) {
        continue;
      }

      unsigned slot = first_slot++;
      
      struct ir_op *save =
          insert_before_ir_op(func, first_op, IR_OP_TY_CUSTOM, IR_OP_VAR_TY_NONE);
      save->custom.aarch64 =
          arena_alloc(func->arena, sizeof(*save->custom.aarch64));
      save->custom.aarch64->ty = AARCH64_OP_TY_SAVE_REG;
      save->reg = i;
      save->lcl_idx = slot;

      if (!saves) {
        saves = save;
      }
    }

    func->total_locals_size =
        ROUND_UP(AARCH64_STACK_ALIGNMENT, func->total_locals_size);

    first_op = func->first->first->first;
    if (func->total_locals_size) {
      struct ir_op *sub_stack = insert_before_ir_op(
          func, first_op, IR_OP_TY_CUSTOM, IR_OP_VAR_TY_NONE);
      sub_stack->custom.aarch64 =
          arena_alloc(func->arena, sizeof(*sub_stack->custom.aarch64));
      sub_stack->custom.aarch64->ty = AARCH64_OP_TY_SUB_STACK;
    }

    // need to save x29 and x30
    struct ir_op *save =
        insert_before_ir_op(func, first_op, IR_OP_TY_CUSTOM, IR_OP_VAR_TY_NONE);
    save->custom.aarch64 =
        arena_alloc(func->arena, sizeof(*save->custom.aarch64));
    save->custom.aarch64->ty = AARCH64_OP_TY_SAVE_LR;

    // emitter understands the above magic-mov as it does not have the PARAM
    // flag set
  }

  struct ir_basicblock *basicblock = func->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;

    while (stmt) {
      struct ir_op *op = stmt->first;

      while (op) {
        switch (op->ty) {
        case IR_OP_TY_CUSTOM:
        case IR_OP_TY_GLB:
        case IR_OP_TY_PHI:
        case IR_OP_TY_CNST:
        case IR_OP_TY_STORE_LCL:
        case IR_OP_TY_LOAD_LCL:
        case IR_OP_TY_BR:
        case IR_OP_TY_MOV:
        case IR_OP_TY_UNARY_OP:
        case IR_OP_TY_CAST_OP:
        case IR_OP_TY_BR_COND:
        case IR_OP_TY_BINARY_OP:
          break;
        case IR_OP_TY_CALL:
          lower_call(func, op);
          break;
        case IR_OP_TY_RET: {
          // FIXME: don't hardcode return reg
          if (op->ret.value->reg != 0) {
            struct ir_op *ret =
                insert_before_ir_op(func, op, IR_OP_TY_MOV, IR_OP_VAR_TY_NONE);
            ret->mov.value = op->ret.value;
            ret->reg = 0;
          }

          if (!leaf) {
            struct ir_op *cur_save = saves;

            while (cur_save && cur_save->ty == IR_OP_TY_CUSTOM && cur_save->custom.aarch64->ty == AARCH64_OP_TY_SAVE_REG) {
              struct ir_op *restore =
                  insert_before_ir_op(func, op, IR_OP_TY_CUSTOM, IR_OP_VAR_TY_NONE);
              restore->custom.aarch64 =
                  arena_alloc(func->arena, sizeof(*restore->custom.aarch64));
              restore->custom.aarch64->ty = AARCH64_OP_TY_RSTR_REG;
              restore->reg = cur_save->reg;
              restore->lcl_idx = cur_save->lcl_idx;

              cur_save = cur_save->succ;
            }

            struct ir_op *restore_lr = insert_before_ir_op(
                func, op, IR_OP_TY_CUSTOM, IR_OP_VAR_TY_NONE);
            restore_lr->custom.aarch64 =
                arena_alloc(func->arena, sizeof(*restore_lr->custom.aarch64));
            restore_lr->custom.aarch64->ty = AARCH64_OP_TY_RSTR_LR;

            if (func->total_locals_size) {
              struct ir_op *add_stack = insert_before_ir_op(
                  func, op, IR_OP_TY_CUSTOM, IR_OP_VAR_TY_NONE);
              add_stack->custom.aarch64 =
                  arena_alloc(func->arena, sizeof(*add_stack->custom.aarch64));
              add_stack->custom.aarch64->ty = AARCH64_OP_TY_ADD_STACK;
            }
          }

          break;
        }
        }

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }
}
