#include "lower.h"

#include "../aarch64.h"
#include "../bit_twiddle.h"
#include "../ir/build.h"
#include "../ir/var_refs.h"
#include "../util.h"

#include <mach/arm/vm_types.h>
#include <math.h>

void aarch64_debug_print_custom_ir_op(FILE *file, const struct ir_builder *func,
                                      const struct ir_op *op) {
  debug_assert(op->ty == IR_OP_TY_CUSTOM, "non custom-op");

  struct aarch64_op *custom = op->custom.aarch64;

  switch (custom->ty) {
  case AARCH64_OP_TY_SAVE_LR:
    fprintf(file, "a64_save_lr");
    break;
  case AARCH64_OP_TY_SAVE_FP:
    fprintf(file, "a64_save_lr_add_64");
    break;
  case AARCH64_OP_TY_RSTR_LR:
    fprintf(file, "a64_rstr_lr");
    break;
  case AARCH64_OP_TY_SUB_STACK:
    fprintf(file, "a64_sub_stack #%zu", func->total_locals_size);
    break;
  case AARCH64_OP_TY_ADD_STACK:
    fprintf(file, "a64_add_stack #%zu", func->total_locals_size);
    break;
  case AARCH64_OP_TY_SAVE_REG:
    fprintf(file, "a64_save_reg R%zu", op->reg.idx);
    break;
  case AARCH64_OP_TY_RSTR_REG:
    fprintf(file, "a64_rstr_reg R%zu", op->reg.idx);
    break;
  case AARCH64_OP_TY_PAGE:
    fprintf(file, "a64_page %%%zu", custom->page.glb_ref->id);
    break;
  case AARCH64_OP_TY_PAGE_OFF:
    fprintf(file, "a64_page_off %%%zu", custom->page_off.glb_ref->id);
    break;
  case AARCH64_OP_TY_STORE_VARIADIC:
    fprintf(file, "a64_store_variadic #%zu, %%%zu", custom->store_variadic.idx,
            custom->store_variadic.value->id);
    break;
  }
}

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

static void lower_str_cnst(struct ir_builder *func, struct ir_op *op) {
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
}

static void call_save_reg(struct ir_builder *func, struct ir_op *call,
                          struct ir_reg reg) {
  // FIXME: this saves entire reg but can sometimes save smaller amounts
  // (depending of the type occupying the live reg)
  struct ir_lcl *lcl = add_local(
      func, &(struct ir_op_var_ty){.ty = IR_OP_VAR_TY_TY_PRIMITIVE,
                                   .primitive = IR_OP_VAR_PRIMITIVE_TY_I64});

  struct ir_op *save =
      insert_before_ir_op(func, call, IR_OP_TY_CUSTOM, IR_OP_VAR_TY_NONE);
  save->custom.aarch64 =
      arena_alloc(func->arena, sizeof(*save->custom.aarch64));
  save->custom.aarch64->ty = AARCH64_OP_TY_SAVE_REG;
  save->reg = reg;
  save->lcl = lcl;

  struct ir_op *restore =
      insert_after_ir_op(func, call, IR_OP_TY_CUSTOM, IR_OP_VAR_TY_NONE);
  restore->custom.aarch64 =
      arena_alloc(func->arena, sizeof(*restore->custom.aarch64));
  restore->custom.aarch64->ty = AARCH64_OP_TY_RSTR_REG;
  restore->reg = reg;
  restore->lcl = lcl;
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

  size_t volatile_integral_reg_count =
      AARCH64_TARGET.reg_info.integral_registers.num_volatile;
  size_t volatile_fp_reg_count =
      AARCH64_TARGET.reg_info.fp_registers.num_volatile;

  unsigned long long live_integral_volatile =
      live_regs & ((1ull << volatile_integral_reg_count) - 1);

  unsigned long long live_fp_volatile =
      live_regs & ((1ull << volatile_fp_reg_count) - 1);

  // if the return reg is used by the call then remove the call-return reg
  // from live-volatile as we know it will be over written by end of call
  if (func_ty->ret_ty->ty != IR_OP_VAR_TY_TY_NONE) {
    switch (op->reg.ty) {
    case IR_REG_TY_INTEGRAL:
      live_integral_volatile &= ~(1ull << op->reg.idx);
      break;
    case IR_REG_TY_FP:
      live_fp_volatile &= ~(1ull << op->reg.idx);
      break;
    default:
      bug("unexpected reg ty");
    }

    struct ir_op *ret_mov =
        insert_before_ir_op(func, op, IR_OP_TY_MOV, *func_ty->ret_ty);
    ret_mov->mov.value = op;
    ret_mov->reg = op->reg;

    // swap so usages point at mov
    // not needed but makes IR cleaner
    swap_ir_ops(func, op, ret_mov);
  }

  size_t max_integral_volatile = sizeof(live_integral_volatile) * 8 - lzcnt(live_integral_volatile);

  for (size_t i = 0; i < max_integral_volatile; i++) {
    if (i == op->reg.idx && op->reg.ty == IR_REG_TY_INTEGRAL) {
      // if not a live-volatile register or the register used by the actual
      // call, skip
      continue;
    }

    if (NTH_BIT(live_integral_volatile, i)) {
      call_save_reg(func, op, (struct ir_reg){ .ty = IR_REG_TY_INTEGRAL, .idx = i  });
    }
  }

  size_t max_fp_volatile = sizeof(live_fp_volatile) * 8 - lzcnt(live_fp_volatile);

  for (size_t i = 0; i < max_fp_volatile; i++) {
    if (i == op->reg.idx && op->reg.ty == IR_REG_TY_FP) {
      // if not a live-volatile register or the register used by the actual
      // call, skip
      continue;
    }

    if (NTH_BIT(live_fp_volatile, i)) {
      call_save_reg(func, op, (struct ir_reg){ .ty = IR_REG_TY_FP, .idx = i  });
    }
  }
  

  // FIXME: generalise this to calling conventions

  // now we need to move each argument into its correct register
  // it is possible there are no spare registers for this, and so we may need to
  // do a swap

  // TODO: handle fp reg
  if (op->call.num_args) {
    unsigned long long free_regs =
        ~op->live_integral_regs & ~((1ull << func_ty->num_params) - 1);
    size_t free_vol_reg = tzcnt(free_regs);

    if (free_vol_reg >= volatile_integral_reg_count) {
      todo("argument moving when no free registers");
    }

    struct ir_op *mov_to_vol =
        insert_before_ir_op(func, op, IR_OP_TY_MOV, func_ty->params[0]);
    mov_to_vol->mov.value = op->call.args[0];
    mov_to_vol->reg = (struct ir_reg){ .ty = IR_REG_TY_INTEGRAL, .idx = free_vol_reg };

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

        struct ir_op *store =
            insert_before_ir_op(func, op, IR_OP_TY_CUSTOM, *var_ty);
        store->lcl = lcl;
        store->custom.aarch64 =
            arena_alloc(func->arena, sizeof(*store->custom.aarch64));
        *store->custom.aarch64 = (struct aarch64_op){
            .ty = AARCH64_OP_TY_STORE_VARIADIC,
            .store_variadic = (struct aarch64_store_variadic){
                .value = source, .idx = variadic_arg_idx}};

        store->flags |= IR_OP_FLAG_VARIADIC_PARAM;
      } else if (i == 0 || op->call.args[i]->reg.idx != arg_reg) {
        struct ir_op *mov =
            insert_before_ir_op(func, op, IR_OP_TY_MOV, *var_ty);

        mov->mov.value = source;
        mov->reg = (struct ir_reg){.ty = IR_REG_TY_INTEGRAL, .idx = arg_reg };
      }
    }
  }

  // FIXME: don't hardcode return reg
  size_t return_reg_idx = 0;
  if (func_ty->ret_ty->ty != IR_OP_VAR_TY_TY_NONE && op->reg.idx != return_reg_idx) {
    struct ir_reg target_reg = op->reg;

    // we move the call _back_ and replace it with a mov, by swapping them
    struct ir_op *new_call =
        insert_before_ir_op(func, op, IR_OP_TY_CALL, op->var_ty);
    new_call->call = op->call;
    new_call->reg = (struct ir_reg){.ty = target_reg.ty, .idx = return_reg_idx};

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

// actually more than this but we don't support that yet
#define MAX_REG_SIZE (8)

static void lower_load_lcl(struct ir_builder *func, struct ir_op *op) {
  // look for store after, in case this is a copy
  // FIXME: not sure if this is perfect logic (could there be ops in between?)
  struct ir_op *nxt_store = op->succ;

  if (!nxt_store || nxt_store->ty != IR_OP_TY_STORE_LCL) {
    return;
  }

  struct ir_var_ty_info info = var_ty_info(func, &op->var_ty);

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

void aarch64_lower(struct ir_builder *func) {
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
        case IR_OP_TY_GLB_REF:
        case IR_OP_TY_PHI:
        case IR_OP_TY_CNST:
        case IR_OP_TY_STORE_LCL:
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
        case IR_OP_TY_CALL:
        case IR_OP_TY_CAST_OP:
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

          // FIXME: floats
          op->metadata =
              arena_alloc(func->arena, sizeof(struct ir_lower_call_metadata));
          *(struct ir_lower_call_metadata *)op->metadata =
              (struct ir_lower_call_metadata){.post_call_live_regs =
                                                  succ->live_integral_regs};
        }

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }

  // points to first save so we can use it for restore later
  // struct ir_op *saves = NULL;
  // struct aarch64_prologue_info info = insert_prologue(func);

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
        case IR_OP_TY_STORE_LCL:
        case IR_OP_TY_LOAD_LCL:
        case IR_OP_TY_STORE_ADDR:
        case IR_OP_TY_LOAD_ADDR:
        case IR_OP_TY_ADDR:
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

          lower_str_cnst(func, op);
          break;
        }
        case IR_OP_TY_CALL:
          lower_call(func, op);
          break;
        case IR_OP_TY_RET: {
          // FIXME: don't hardcode return reg
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
