#include "emitter.h"

#include "../alloc.h"
#include "../util.h"
#include <stdlib.h>

struct aarch64_emitter {
  uint32_t *block;
  size_t len;
  size_t head;
};

#define BLOCK_SIZE 4096

void create_aarch64_emitter(struct aarch64_emitter **emitter) {
  *emitter = nonnull_malloc(sizeof(**emitter));

  (*emitter)->block = nonnull_malloc(BLOCK_SIZE);
  (*emitter)->len = BLOCK_SIZE;
  (*emitter)->head = 0;
}

size_t aarch64_emit_bytesize(struct aarch64_emitter *emitter) {
  return emitter->head * sizeof(*emitter->block);
}

void aarch64_emit_copy_to(struct aarch64_emitter *emitter, void *dest) {
  memcpy(dest, emitter->block, emitter->head * sizeof(*emitter->block));
}

void free_aarch64_emitter(struct aarch64_emitter **emitter) {
  free((*emitter)->block);

  free(*emitter);
  *emitter = NULL;
}

#define ZERO_REG_IDX (31)

#define LDR_STR(op0, op1, op2, op3, op4)                                       \
  (uint32_t)(((op0) << 28) | (0b1 << 27) | ((op1) << 26) /* bit 25 is zero */  \
             | ((op2) << 22) | ((op3) << 12) | (op4))

#define LDR_LITERAL(opc, V, imm19, Rt)                                         \
  (uin32_t)(((opc) << 30) | (0b011 << 27) | ((V) << 26) | ((imm19) << 5) | (Rt))

#define LOGICAL_IMM(sf, opc, N, immr, imms, Rn, Rd)                            \
  (uint32_t)(((sf) << 31) | ((opc) << 29) | (0b100100 << 23) | ((N) << 22) |   \
             ((immr) << 16) | ((immr) << 10) | ((Rn) << 5) | (Rd))

#define AND_32_IMM(immr, imms, Rn, Rd)                                         \
  LOGICAL_IMM(0b0, 0b00, 0b0, immr, imms, Rn, Rd)
#define ORR_32_IMM(immr, imms, Rn, Rd)                                         \
  LOGICAL_IMM(0b0, 0b01, 0b0, immr, imms, Rn, Rd)
#define EOR_32_IMM(immr, imms, Rn, Rd)                                         \
  LOGICAL_IMM(0b0, 0b10, 0b0, immr, imms, Rn, Rd)
#define ANDS_32_IMM(immr, imms, Rn, Rd)                                        \
  LOGICAL_IMM(0b0, 0b11, 0b0, immr, imms, Rn, Rd)

#define AND_64_IMM(immr, imms, Rn, Rd)                                         \
  LOGICAL_IMM(0b1, 0b00, 0b0, immr, imms, Rn, Rd)
#define ORR_64_IMM(immr, imms, Rn, Rd)                                         \
  LOGICAL_IMM(0b1, 0b01, 0b0, immr, imms, Rn, Rd)
#define EOR_64_IMM(immr, imms, Rn, Rd)                                         \
  LOGICAL_IMM(0b1, 0b10, 0b0, immr, imms, Rn, Rd)
#define ANDS_64_IMM(immr, imms, Rn, Rd)                                        \
  LOGICAL_IMM(0b1, 0b11, 0b0, immr, imms, Rn, Rd)

#define MOV_WIDE_IMM(sf, opc, hw, imm16, Rd)                                   \
  (uint32_t)(((sf) << 31) | ((opc) << 29) | (0b100101 << 23) | ((hw) << 21) |  \
             ((imm16) << 5) | (Rd))

#define MOVN_32(hw, imm16, Rd) MOV_WIDE_IMM(0b0, 0b00, hw, imm16, Rd)
#define MOVZ_32(hw, imm16, Rd) MOV_WIDE_IMM(0b0, 0b10, hw, imm16, Rd)
#define MOVK_32(hw, imm16, Rd) MOV_WIDE_IMM(0b0, 0b11, hw, imm16, Rd)

#define MOVN_64(hw, imm16, Rd) MOV_WIDE_IMM(0b1, 0b00, hw, imm16, Rd)
#define MOVZ_64(hw, imm16, Rd) MOV_WIDE_IMM(0b1, 0b10, hw, imm16, Rd)
#define MOVK_64(hw, imm16, Rd) MOV_WIDE_IMM(0b1, 0b11, hw, imm16, Rd)

#define LOGICAL_REG(sf, opc, shift, N, Rm, imm6, Rn, Rd)                       \
  (uint32_t)(((sf) << 31) | ((opc) << 29) | (0b01010 << 24) |                  \
             ((shift) << 22) | ((N) << 21) | ((Rm) << 16) | ((imm6) << 10) |   \
             ((Rn) << 5) | (Rd))

#define ADD_SUB_SHIFTED_REG(sf, op, S, shift, Rm, imm6, Rn, Rd)                \
  (uint32_t)(((sf) << 31) | ((op) << 30) | ((S) << 29) | ((0b01011) << 24) |   \
             ((shift) << 22) | ((Rm) << 16) | ((imm6) << 10) | ((Rn) << 5) |   \
             (Rd))

#define ADD_32_REG(shift, imm6, Rm, Rn, Rd)                                    \
  ADD_SUB_SHIFTED_REG(0, 0, 0, shift, Rm, imm6, Rn, Rd)
#define SUB_32_REG(shift, imm6, Rm, Rn, Rd)                                    \
  ADD_SUB_SHIFTED_REG(0, 1, 0, shift, Rm, imm6, Rn, Rd)

#define REG_2_SOURCE(sf, S, opcode, Rm, Rn, Rd)                                \
  (uint32_t)(((sf) << 31) | ((S) << 29) | (0b11010110 << 21) | ((Rm) << 16) |  \
             ((opcode) << 10) | ((Rn) << 5) | (Rd))

#define REG_3_SOURCE(sf, op54, op31, Rm, o0, Ra, Rn, Rd)                       \
  (uint32_t)(((sf) << 31) | ((op54) << 29) | (0b11011 << 24) |                 \
             ((op31) << 21) | ((Rm) << 16) | ((o0) << 15) | ((Ra) << 10) |     \
             ((Rn) << 5) | (Rd))

#define MADD_32(Rm, Ra, Rn, Rd)                                                \
  REG_3_SOURCE(0b0, 0b00, 0b000, Rm, 0b0, Ra, Rn, Rd)
#define MSUB_32(Rm, Ra, Rn, Rd)                                                \
  REG_3_SOURCE(0b0, 0b00, 0b000, Rm, 0b1, Ra, Rn, Rd)

#define MADD_64(Rm, Ra, Rn, Rd)                                                \
  REG_3_SOURCE(0b1, 0b00, 0b000, Rm, 0b0, Ra, Rn, Rd)
#define MSUB_64(Rm, Ra, Rn, Rd)                                                \
  REG_3_SOURCE(0b1, 0b00, 0b000, Rm, 0b1, Ra, Rn, Rd)

#define UDIV_32(Rm, Rn, Rd) REG_2_SOURCE(0b0, 0b0, 0b000010, Rm, Rn, Rd)
#define SDIV_32(Rm, Rn, Rd) REG_2_SOURCE(0b0, 0b0, 0b000011, Rm, Rn, Rd)

#define SHIFT_LSL (0b00)
#define SHIFT_LSR (0b01)
#define SHIFT_ASR (0b10)
#define SHIFT_ROR (0b11)

#define ORR_32_REG(shift, Rm, imm6, Rn, Rd)                                    \
  LOGICAL_REG(0b0, 0b01, shift, 0b0, Rm, imm6, Rn, Rd)
#define ORN_32_REG(shift, Rm, imm6, Rn, Rd)                                    \
  LOGICAL_REG(0b0, 0b01, shift, 0b1, Rm, imm6, Rn, Rd)

#define ORR_64_REG(shift, Rm, imm6, Rn, Rd)                                    \
  LOGICAL_REG(0b1, 0b01, shift, 0b0, Rm, imm6, Rn, Rd)
#define ORN_64_REG(shift, Rm, imm6, Rn, Rd)                                    \
  LOGICAL_REG(0b1, 0b01, shift, 0b1, Rm, imm6, Rn, Rd)

#define MOV_32_REG(Rm, Rd) ORR_32_REG(SHIFT_LSL, Rm, 0b000000, ZERO_REG_IDX, Rd)
#define MOVN_32_REG(Rm, Rd)                                                    \
  ORN_32_REG(SHIFT_LSL, Rm, 0b000000, ZERO_REG_IDX, Rd)

#define MOV_64_REG(Rm, Rd) ORR_64_REG(SHIFT_LSL, Rm, 0b000000, ZERO_REG_IDX, Rd)
#define MOVN_64_REG(Rm, Rd)                                                    \
  ORN_64_REG(SHIFT_LSL, Rm, 0b000000, ZERO_REG_IDX, Rd)

#define BRANCH(op0, op1, op2)                                                  \
  (uint32_t)(((op0) << 29) | (0b101 << 26) | ((op1) << 5) | (op2))

#define UNCOND_BRANCH_REG(opc, op2, op3, Rn, op4)                              \
  BRANCH(0b110,                                                                \
         (0b1 << 20) | ((opc) << 16) | ((op2) << 11) | (op3 << 5) | (Rn), op4)

#define RET(Rn) UNCOND_BRANCH_REG(0b0010, 0b11111, 0b000000, Rn, 00000)

void aarch64_emit(struct aarch64_emitter *emitter, uint32_t instr) {
  if (emitter->head >= emitter->len) {
    todo("emitter reached size limit");
  }

  emitter->block[emitter->head++] = instr;
}

void aarch64_emit_add_32(struct aarch64_emitter *emitter, size_t reg_lhs,
                         size_t reg_rhs, size_t reg_to) {
  aarch64_emit(emitter, ADD_32_REG(0, 0, reg_rhs, reg_lhs, reg_to));
}

void aarch64_emit_sub_32(struct aarch64_emitter *emitter, size_t reg_lhs,
                         size_t reg_rhs, size_t reg_to) {
  aarch64_emit(emitter, SUB_32_REG(0, 0, reg_rhs, reg_lhs, reg_to));
}

void aarch64_emit_mul_32(struct aarch64_emitter *emitter, size_t reg_lhs,
                         size_t reg_rhs, size_t reg_to) {
  aarch64_emit(emitter, MADD_32(reg_rhs, ZERO_REG_IDX, reg_lhs, reg_to));
}

void aarch64_emit_madd_32(struct aarch64_emitter *emitter, size_t reg_lhs,
                         size_t reg_rhs, size_t reg_add, size_t reg_to) {
  aarch64_emit(emitter, MADD_32(reg_rhs, reg_add, reg_lhs, reg_to));
}

void aarch64_emit_msub_32(struct aarch64_emitter *emitter, size_t reg_lhs,
                         size_t reg_rhs, size_t reg_sub, size_t reg_to) {
  aarch64_emit(emitter, MSUB_32(reg_rhs, reg_sub, reg_lhs, reg_to));
}

void aarch64_emit_sdiv_32(struct aarch64_emitter *emitter, size_t reg_lhs,
                          size_t reg_rhs, size_t reg_to) {
  aarch64_emit(emitter, SDIV_32(reg_rhs, reg_lhs, reg_to));
}

void aarch64_emit_udiv_32(struct aarch64_emitter *emitter, size_t reg_lhs,
                          size_t reg_rhs, size_t reg_to) {
  aarch64_emit(emitter, UDIV_32(reg_rhs, reg_lhs, reg_to));
}

void aarch64_emit_mov_32(struct aarch64_emitter *emitter, size_t reg_from,
                         size_t reg_to) {
  aarch64_emit(emitter, MOV_32_REG(reg_from, reg_to));
}

void aarch64_emit_mov_64(struct aarch64_emitter *emitter, size_t reg_from,
                         size_t reg_to) {
  aarch64_emit(emitter, MOV_64_REG(reg_from, reg_to));
}

void aarch64_emit_load_cnst_64(struct aarch64_emitter *emitter, size_t reg_idx,
                               uint64_t cnst) {
  switch (cnst) {
  case 0: {
    aarch64_emit(emitter, MOV_64_REG(ZERO_REG_IDX, reg_idx));
    break;
  }
  case -1: {
    aarch64_emit(emitter, MOVN_64_REG(ZERO_REG_IDX, reg_idx));
    break;
  }
  default: {
    if (cnst <= UINT16_MAX) {
      aarch64_emit(emitter, MOVZ_64(/* no shift */ 0, (uint16_t)cnst, reg_idx));
      break;
    } else {
      todo("mov cnst > 2^16");
    }
  }
  }
}

void aarch64_emit_load_cnst_32(struct aarch64_emitter *emitter, size_t reg_idx,
                               uint32_t cnst) {
  switch (cnst) {
  case 0: {
    aarch64_emit(emitter, MOV_32_REG(ZERO_REG_IDX, reg_idx));
    break;
  }
  case -1: {
    aarch64_emit(emitter, MOVN_32_REG(ZERO_REG_IDX, reg_idx));
    break;
  }
  default: {
    if (cnst <= UINT16_MAX) {
      aarch64_emit(emitter, MOVZ_32(/* no shift */ 0, (uint16_t)cnst, reg_idx));
      break;
    } else {
      todo("mov cnst > 2^16");
    }
  }
  }
}

void aarch64_emit_ret(struct aarch64_emitter *emitter) {
  aarch64_emit(emitter, RET(/* normal reg for return address */ 30));
}