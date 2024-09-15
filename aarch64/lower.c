#include "lower.h"

#include "../aarch64.h"
#include "../bit_twiddle.h"
#include "../ir/build.h"
#include "../ir/var_refs.h"
#include "../util.h"

#include <math.h>

// as we add a bunch of new nodes around, `live_regs` can get lost
// we early preserve it in this metadata for use in `lower_call`
struct ir_lower_call_metadata {
  unsigned long post_call_live_regs;
};

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

static void lower_call(struct ir_builder *func, struct ir_op *op) {
  invariant_assert(op->call.target->var_ty.ty == IR_OP_VAR_TY_TY_FUNC,
                   "non-func");

  struct ir_op_var_func_ty *func_ty = &op->call.target->var_ty.func;

  invariant_assert(is_func_variadic(func_ty) ||
                       func_ty->num_params == op->call.num_args,
                   "mismatch of function param (%zu) and arg (%zu) count",
                   func_ty->num_params, op->call.num_args);

  invariant_assert(func_ty->num_params <= 8,
                   "`%s` doesn't support more than 8 args yet", __func__);

  struct ir_lower_call_metadata *metadata =
      (struct ir_lower_call_metadata *)op->metadata;
  unsigned long live_regs = metadata->post_call_live_regs;

  // FIXME: don't hardcode volatile count
  // FIXME: is it safe to consider x15/16 volatile? possibly not as they are IPC
  // registers
  size_t volatile_reg_count = 18;

  unsigned long long live_volatile =
      live_regs & ((1ull << volatile_reg_count) - 1);

  // if the return reg is used by the call then remove the call-return reg
  // from live-volatile as we know it will be over written by end of call
  if (func_ty->ret_ty->ty != IR_OP_VAR_TY_TY_NONE) {
    live_volatile &= ~(1ull << op->reg);

    struct ir_op *ret_mov =
        insert_before_ir_op(func, op, IR_OP_TY_MOV, *func_ty->ret_ty);
    ret_mov->mov.value = op;
    ret_mov->reg = op->reg;

    // swap so usages point at mov
    // not needed but makes IR cleaner
    swap_ir_ops(func, op, ret_mov);
  }

  size_t max_volatile = sizeof(live_volatile) * 8 - lzcnt(live_volatile);

  unsigned caller_saves = popcntl(live_volatile);
  unsigned new_slots = caller_saves > func->num_call_saves
                           ? caller_saves - func->num_call_saves
                           : 0;

  // TODO: we assume all saves are 8 bytes
  func->total_call_saves_size += 8 * new_slots;

  for (size_t i = 0; i < max_volatile; i++) {
    if (!NTH_BIT(live_volatile, i) || i == op->reg) {
      // if not a live-volatile register or the register used by the actual
      // call, skip
      continue;
    }

    // FIXME: this saves entire reg but can sometimes save smaller amounts
    // (depending of the type occupying the live reg)
    struct ir_lcl *lcl = add_local(
        func, &(struct ir_op_var_ty){.ty = IR_OP_VAR_TY_TY_PRIMITIVE,
                                     .primitive = IR_OP_VAR_PRIMITIVE_TY_I64});

    struct ir_op *save =
        insert_before_ir_op(func, op, IR_OP_TY_CUSTOM, IR_OP_VAR_TY_NONE);
    save->custom.aarch64 =
        arena_alloc(func->arena, sizeof(*save->custom.aarch64));
    save->custom.aarch64->ty = AARCH64_OP_TY_SAVE_REG;
    save->reg = i;
    save->lcl = lcl;

    struct ir_op *restore =
        insert_after_ir_op(func, op, IR_OP_TY_CUSTOM, IR_OP_VAR_TY_NONE);
    restore->custom.aarch64 =
        arena_alloc(func->arena, sizeof(*restore->custom.aarch64));
    restore->custom.aarch64->ty = AARCH64_OP_TY_RSTR_REG;
    restore->reg = i;
    restore->lcl = lcl;
  }

  // FIXME: generalise this to calling conventions

  // now we need to move each argument into its correct register
  // it is possible there are no spare registers for this, and so we may need to
  // do a swap

  if (op->call.num_args) {
    unsigned long long free_regs =
        ~op->live_regs & ~((1ull << func_ty->num_params) - 1);
    size_t free_vol_reg = tzcnt(free_regs);

    if (free_vol_reg >= volatile_reg_count) {
      todo("argument moving when no free registers");
    }

    struct ir_op *mov_to_vol =
        insert_before_ir_op(func, op, IR_OP_TY_MOV, func_ty->params[0]);
    mov_to_vol->mov.value = op->call.args[0];
    mov_to_vol->reg = free_vol_reg;

    size_t num_normal_args = is_func_variadic(func_ty) ? func_ty->num_params - 1
                                                       : func_ty->num_params;

    for (size_t head = 0; head < op->call.num_args; head++) {
      size_t i = op->call.num_args - 1 - head;
      struct ir_op_var_ty *var_ty = &op->call.args[i]->var_ty;

      invariant_assert(var_ty->ty == IR_OP_VAR_TY_TY_PRIMITIVE ||
                           var_ty->ty == IR_OP_VAR_TY_TY_POINTER,
                       "`lower_call` doesn't support non-prims");

      struct ir_op *source = i == 0 ? mov_to_vol : op->call.args[i];
      size_t arg_reg = i;
      if (i >= num_normal_args) {
        // we are in a variadic and everything from here on is passed on the
        // stack
        struct ir_lcl *lcl = add_local(
            func,
            &(struct ir_op_var_ty){.ty = IR_OP_VAR_TY_TY_PRIMITIVE,
                                   .primitive = IR_OP_VAR_PRIMITIVE_TY_I64});

        // the stack slot this local must live in
        size_t variadic_arg_idx = i - num_normal_args;
        lcl->metadata = (void *)(variadic_arg_idx + 1);

        struct ir_op *store =
            insert_before_ir_op(func, op, IR_OP_TY_STORE_LCL, *var_ty);
        store->lcl = lcl;
        store->store_lcl.value = source;
        store->flags |= IR_OP_FLAG_VARIADIC_PARAM;
      } else if (i == 0 || op->call.args[i]->reg != arg_reg) {
        struct ir_op *mov =
            insert_before_ir_op(func, op, IR_OP_TY_MOV, *var_ty);

        mov->mov.value = source;
        mov->reg = arg_reg;
      }
    }
  }

  // FIXME: don't hardcode return reg
  size_t return_reg = 0;
  if (func_ty->ret_ty->ty != IR_OP_VAR_TY_TY_NONE && op->reg != return_reg) {
    size_t target_reg = op->reg;

    // we move the call _back_ and replace it with a mov, by swapping them
    struct ir_op *new_call =
        insert_before_ir_op(func, op, IR_OP_TY_CALL, op->var_ty);
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
        case IR_OP_TY_GLB_REF:
        case IR_OP_TY_PHI:
        case IR_OP_TY_CNST:
        case IR_OP_TY_STORE_LCL:
        case IR_OP_TY_LOAD_LCL:
        case IR_OP_TY_BR:
        case IR_OP_TY_MOV:
        case IR_OP_TY_RET:
        case IR_OP_TY_CALL:
        case IR_OP_TY_CAST_OP:
          break;
        case IR_OP_TY_UNARY_OP:
          if (op->binary_op.ty == IR_OP_UNARY_OP_TY_LOGICAL_NOT) {
            lower_logical_not(func, op);
          }
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
          break;
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

  // first we need to insert the metadata we want
  // currently this is just on call nodes
  struct ir_basicblock *basicblock = func->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;

    while (stmt) {
      struct ir_op *op = stmt->first;

      while (op) {
        if (op->ty == IR_OP_TY_CALL) {
          // we need to save registers in-use _after_ call
          struct ir_op *succ = op->succ;
          if (!succ && op->stmt->succ) {
            // call is end of stmt, get live from next stmt
            // a call can not be the final op of the final stmt of a basicblock
            // as that must be a br/ret
            succ = op->stmt->succ->first;
          }

          op->metadata =
              arena_alloc(func->arena, sizeof(struct ir_lower_call_metadata));
          *(struct ir_lower_call_metadata *)op->metadata =
              (struct ir_lower_call_metadata){.post_call_live_regs =
                                                  succ->live_regs};
        }

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }

  bool leaf = !(func->nonvolatile_registers_used || func->num_locals ||
                func->flags & IR_BUILDER_FLAG_MAKES_CALL);

  // points to first save so we can use it for restore later
  struct ir_op *saves = NULL;
  if (!leaf) {
    struct ir_op *first_op = func->first->first->first;

    // for x29 and x30 as they aren't working yet with pre-indexing
    func->total_locals_size += 16;

    unsigned long max_nonvol_used =
        sizeof(func->nonvolatile_registers_used) * 8 -
        lzcnt(func->nonvolatile_registers_used);

    for (size_t i = 0; i < max_nonvol_used; i++) {
      // FIXME: loop should start at i=first non volatile
      if (!NTH_BIT(func->nonvolatile_registers_used, i)) {
        continue;
      }

      struct ir_op *save = insert_before_ir_op(func, first_op, IR_OP_TY_CUSTOM,
                                               IR_OP_VAR_TY_NONE);
      save->custom.aarch64 =
          arena_alloc(func->arena, sizeof(*save->custom.aarch64));
      save->custom.aarch64->ty = AARCH64_OP_TY_SAVE_REG;
      save->reg = i;
      // uses type i64 to save entire reg
      save->lcl = add_local(func, &(struct ir_op_var_ty){
                                      .ty = IR_OP_VAR_TY_TY_PRIMITIVE,
                                      .primitive = IR_OP_VAR_PRIMITIVE_TY_I64});

      if (!saves) {
        saves = save;
      }
    }

    func->total_locals_size =
        ROUND_UP(func->total_locals_size, AARCH64_STACK_ALIGNMENT);

    first_op = func->first->first->first;

    // need to save x29 and x30
    struct ir_op *save =
        insert_before_ir_op(func, first_op, IR_OP_TY_CUSTOM, IR_OP_VAR_TY_NONE);
    save->custom.aarch64 =
        arena_alloc(func->arena, sizeof(*save->custom.aarch64));
    save->custom.aarch64->ty = AARCH64_OP_TY_SAVE_LR;

    struct ir_op *save_add_64 =
        insert_before_ir_op(func, first_op, IR_OP_TY_CUSTOM, IR_OP_VAR_TY_NONE);
    save_add_64->custom.aarch64 =
        arena_alloc(func->arena, sizeof(*save_add_64->custom.aarch64));
    save_add_64->custom.aarch64->ty = AARCH64_OP_TY_SAVE_LR_ADD_64;

    if (func->total_locals_size) {
      struct ir_op *sub_stack = insert_before_ir_op(
          func, first_op, IR_OP_TY_CUSTOM, IR_OP_VAR_TY_NONE);
      sub_stack->custom.aarch64 =
          arena_alloc(func->arena, sizeof(*sub_stack->custom.aarch64));
      sub_stack->custom.aarch64->ty = AARCH64_OP_TY_SUB_STACK;
    }

    // emitter understands the above magic-mov as it does not have the PARAM
    // flag set
  }

  basicblock = func->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;

    while (stmt) {
      struct ir_op *op = stmt->first;

      while (op) {
        switch (op->ty) {
        case IR_OP_TY_STORE_LCL:
        case IR_OP_TY_LOAD_LCL:
        case IR_OP_TY_BR:
        case IR_OP_TY_MOV:
        case IR_OP_TY_UNARY_OP:
        case IR_OP_TY_CAST_OP:
        case IR_OP_TY_BR_COND:
        case IR_OP_TY_GLB_REF:
        case IR_OP_TY_BINARY_OP:
        case IR_OP_TY_CUSTOM:
        case IR_OP_TY_PHI:
          break;

        case IR_OP_TY_CNST: {
          if (op->cnst.ty != IR_OP_CNST_TY_STR) {
            break;
          }

          const char *data = op->cnst.str_value;

          // first insert a GLB_REF to instruct that a reloc must be created
          struct ir_op *ref =
              insert_before_ir_op(func, op, IR_OP_TY_GLB_REF, op->var_ty);
          make_string_ref(func, data, ref, &ref->var_ty);

          // now insert the pair needed to generate the actual address
          struct ir_op *page =
              insert_before_ir_op(func, op, IR_OP_TY_CUSTOM, op->var_ty);
          page->reg = op->reg;
          page->custom.aarch64 =
              arena_alloc(func->arena, sizeof(*page->custom.aarch64));
          page->custom.aarch64->ty = AARCH64_OP_TY_PAGE;
          page->custom.aarch64->page = (struct aarch64_op_page){.glb_ref = ref};

          struct ir_op *page_off = op;
          replace_ir_op(func, page_off, IR_OP_TY_CUSTOM, op->var_ty);
          page_off->reg = op->reg;
          page_off->custom.aarch64 =
              arena_alloc(func->arena, sizeof(*page_off->custom.aarch64));
          page_off->custom.aarch64->ty = AARCH64_OP_TY_PAGE_OFF;
          page_off->custom.aarch64->page_off =
              (struct aarch64_op_page_off){.glb_ref = ref};

          break;
        }
        case IR_OP_TY_CALL:
          lower_call(func, op);
          break;
        case IR_OP_TY_RET: {
          // FIXME: don't hardcode return reg
          if (op->ret.value && op->ret.value->reg != 0) {
            struct ir_op *ret =
                insert_before_ir_op(func, op, IR_OP_TY_MOV, IR_OP_VAR_TY_NONE);
            ret->mov.value = op->ret.value;
            ret->reg = 0;

            op->ret.value = ret;
          }

          if (!leaf) {
            struct ir_op *cur_save = saves;

            while (cur_save && cur_save->ty == IR_OP_TY_CUSTOM &&
                   cur_save->custom.aarch64->ty == AARCH64_OP_TY_SAVE_REG) {
              struct ir_op *restore = insert_before_ir_op(
                  func, op, IR_OP_TY_CUSTOM, IR_OP_VAR_TY_NONE);
              restore->custom.aarch64 =
                  arena_alloc(func->arena, sizeof(*restore->custom.aarch64));
              restore->custom.aarch64->ty = AARCH64_OP_TY_RSTR_REG;
              restore->reg = cur_save->reg;
              restore->lcl = cur_save->lcl;

              cur_save = cur_save->succ;
            }

            if (func->total_locals_size) {
              struct ir_op *add_stack = insert_before_ir_op(
                  func, op, IR_OP_TY_CUSTOM, IR_OP_VAR_TY_NONE);
              add_stack->custom.aarch64 =
                  arena_alloc(func->arena, sizeof(*add_stack->custom.aarch64));
              add_stack->custom.aarch64->ty = AARCH64_OP_TY_ADD_STACK;
            }

            struct ir_op *restore_lr = insert_before_ir_op(
                func, op, IR_OP_TY_CUSTOM, IR_OP_VAR_TY_NONE);
            restore_lr->custom.aarch64 =
                arena_alloc(func->arena, sizeof(*restore_lr->custom.aarch64));
            restore_lr->custom.aarch64->ty = AARCH64_OP_TY_RSTR_LR;
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

  func->total_locals_size =
      ROUND_UP(func->total_locals_size, AARCH64_STACK_ALIGNMENT);
}
