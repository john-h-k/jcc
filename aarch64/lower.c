#include "lower.h"
#include "../ir.h"

#define RETURN_REG (0)

void lower_binary_op(struct aarch64_emitter *emitter, uint32_t lhs_reg,
                     uint32_t rhs_reg, uint32_t reg,
                     struct ir_op_binary_op *op) {
  switch (op->ty) {
  case IR_OP_BINARY_OP_TY_ADD:
    aarch64_emit_add_32(emitter, lhs_reg, rhs_reg, reg);
    break;
  case IR_OP_BINARY_OP_TY_SUB:
    aarch64_emit_sub_32(emitter, lhs_reg, rhs_reg, reg);
    break;
  case IR_OP_BINARY_OP_TY_MUL:
    aarch64_emit_mul_32(emitter, lhs_reg, rhs_reg, reg);
    break;
  case IR_OP_BINARY_OP_TY_SDIV:
    aarch64_emit_sdiv_32(emitter, lhs_reg, rhs_reg, reg);
    break;
  case IR_OP_BINARY_OP_TY_UDIV:
    aarch64_emit_udiv_32(emitter, lhs_reg, rhs_reg, reg);
    break;
  default:
    todo("unsupported op");
    // case IR_OP_BINARY_OP_TY_QUOT:
    //   aarch64_emit_quot_32(emitter, lhs_reg, rhs_reg, reg);
    //   break;
  }
}

const char *mangle(struct arena_allocator *arena, const char *name) {
  char *dest = alloc(arena, strlen(name) + /* null terminator + '_' char */ 2);

  dest[0] = '_';
  strcpy(dest + 1, name);

  return dest;
}

struct lower_result lower(struct arena_allocator *arena,
                          struct ir_function *func) {
  struct aarch64_emitter *emitter;
  create_aarch64_emitter(&emitter);

  uint32_t *reg_map = nonnull_malloc(sizeof(*reg_map) * func->op_count);
  memset(reg_map, -1, sizeof(*reg_map) * func->op_count);

  uint32_t last_reg = 0;

  struct ir_op *op = func->start;
  while (op) {
    trace("lowering op with id %d, type %d", op->id, op->ty);
    switch (op->ty) {
    case IR_OP_TY_CNST: {
      uint32_t reg = last_reg++;
      reg_map[op->id] = reg;
      aarch64_emit_load_cnst_32(emitter, reg, op->cnst.value);
      break;
    }
    case IR_OP_TY_BINARY_OP: {
      uint32_t lhs_reg = reg_map[op->binary_op.lhs->id];
      uint32_t rhs_reg = reg_map[op->binary_op.rhs->id];
      invariant_assert(lhs_reg != UINT32_MAX && rhs_reg != UINT32_MAX,
                       "bad IR, no reg");

      uint32_t reg = last_reg++;
      reg_map[op->id] = reg;
      lower_binary_op(emitter, lhs_reg, rhs_reg, reg, &op->binary_op);
      break;
    }
    case IR_OP_TY_RET: {
      uint32_t reg = reg_map[op->ret.value->id];
      invariant_assert(reg != UINT32_MAX, "bad IR, no reg");

      if (reg != RETURN_REG) {
        aarch64_emit_mov_32(emitter, reg, RETURN_REG);
      }

      aarch64_emit_ret(emitter);
      break;
    }
    default: {
      todo("unsupported IR OP");
      break;
    }
    }

    op = op->succ;
  }

  size_t len = aarch64_emit_bytesize(emitter);
  void *data = alloc(arena, len);
  aarch64_emit_copy_to(emitter, data);

  free_aarch64_emitter(&emitter);

  struct lower_result result = {
      .name = mangle(arena, func->name), .code = data, .len_code = len};

  return result;
}
