#include "arm64_emitter.h"

#include "alloc.h"
#include "util.h"
#include <stdlib.h>

struct arm64_emitter {
  uint32_t *block;
  size_t len;
  size_t head;
};

#define BLOCK_SIZE 4096

void create_arm64_emitter(struct arm64_emitter **emitter) {
  *emitter = nonnull_malloc(sizeof(**emitter));

  (*emitter)->block = nonnull_malloc(BLOCK_SIZE);
  (*emitter)->len = BLOCK_SIZE;
  (*emitter)->head = 0;
}

size_t arm64_emit_bytesize(struct arm64_emitter *emitter) {
  return emitter->head * sizeof(*emitter->block);
}

void arm64_emit_copy_to(struct arm64_emitter *emitter, void* dest) {
  memcpy(dest, emitter->block, emitter->head * sizeof(*emitter->block));
}

void free_arm64_emitter(struct arm64_emitter **emitter) {
  free((*emitter)->block);
  
  free(*emitter);
  *emitter = NULL;
}

#define ZERO_REG_IDX (31)

#define LDR_STR(op0, op1, op2, op3, op4) (uint32_t)( \
  ((op0) << 28) \
    | (0b1 << 27) \
    | ((op1) << 26) \
    /* bit 25 is zero */ \
    | ((op2) << 22) \
    | ((op3) << 12) \
    | (op4) \
  )

#define LDR_LITERAL(opc, V, imm19, Rt) (uin32_t)( \
  ((opc) << 30) \
    | (0b011 << 27) \
    | ((V) << 26) \
    | ((imm19) << 5) \
    | (Rt) \
  )

#define LOGICAL_IMM(sf, opc, N, immr, imms, Rn, Rd) (uint32_t)( \
  ((sf) << 31) \
    | ((opc) << 29) \
    | (0b100100 << 23) \
    | ((N) << 22) \
    | ((immr) << 16) \
    | ((immr) << 10) \
    | ((Rn) << 5) \
    | (Rd) \
  )

#define AND_32_IMM(immr, imms, Rn, Rd) LOGICAL_IMM(0b0, 0b00, 0b0, immr, imms, Rn, Rd)
#define ORR_32_IMM(immr, imms, Rn, Rd) LOGICAL_IMM(0b0, 0b01, 0b0, immr, imms, Rn, Rd)
#define EOR_32_IMM(immr, imms, Rn, Rd) LOGICAL_IMM(0b0, 0b10, 0b0, immr, imms, Rn, Rd)
#define ANDS_32_IMM(immr, imms, Rn, Rd) LOGICAL_IMM(0b0, 0b11, 0b0, immr, imms, Rn, Rd)

#define AND_64_IMM(immr, imms, Rn, Rd) LOGICAL_IMM(0b1, 0b00, 0b0, immr, imms, Rn, Rd)
#define ORR_64_IMM(immr, imms, Rn, Rd) LOGICAL_IMM(0b1, 0b01, 0b0, immr, imms, Rn, Rd)
#define EOR_64_IMM(immr, imms, Rn, Rd) LOGICAL_IMM(0b1, 0b10, 0b0, immr, imms, Rn, Rd)
#define ANDS_64_IMM(immr, imms, Rn, Rd) LOGICAL_IMM(0b1, 0b11, 0b0, immr, imms, Rn, Rd)

#define MOV_WIDE_IMM(sf, opc, hw, imm16, Rd) (uint32_t)( \
  ((sf) << 31) \
    | ((opc) << 29) \
    | (0b100101 << 23) \
    | ((hw) << 21) \
    | ((imm16) << 5) \
    | (Rd) \
  )

#define MOVN_32(hw, imm16, Rd) MOV_WIDE_IMM(0b0, 0b00, hw, imm16, Rd)
#define MOVZ_32(hw, imm16, Rd) MOV_WIDE_IMM(0b0, 0b10, hw, imm16, Rd)
#define MOVK_32(hw, imm16, Rd) MOV_WIDE_IMM(0b0, 0b11, hw, imm16, Rd)

#define MOVN_64(hw, imm16, Rd) MOV_WIDE_IMM(0b1, 0b00, hw, imm16, Rd)
#define MOVZ_64(hw, imm16, Rd) MOV_WIDE_IMM(0b1, 0b10, hw, imm16, Rd)
#define MOVK_64(hw, imm16, Rd) MOV_WIDE_IMM(0b1, 0b11, hw, imm16, Rd)

#define LOGICAL_REG(sf, opc, shift, N, Rm, imm6, Rn, Rd) (uint32_t)( \
  ((sf) << 31) \
    | ((opc) << 29) \
    | (0b01010 << 24) \
    | ((shift) << 22) \
    | ((N) << 21) \
    | ((Rm) << 16) \
    | ((Rm) << 16) \
    | ((imm6) << 10) \
    | ((Rn) << 5) \
    | (Rd) \
  )


#define SHIFT_LSL (0b00)
#define SHIFT_LSR (0b01)
#define SHIFT_ASR (0b10)
#define SHIFT_ROR (0b11)

#define ORR_32_REG(shift, Rm, imm6, Rn, Rd) LOGICAL_REG(0b0, 0b01, shift, 0b0, Rm, imm6, Rn, Rd)
#define ORN_32_REG(shift, Rm, imm6, Rn, Rd) LOGICAL_REG(0b0, 0b01, shift, 0b1, Rm, imm6, Rn, Rd)

#define ORR_64_REG(shift, Rm, imm6, Rn, Rd) LOGICAL_REG(0b1, 0b01, shift, 0b0, Rm, imm6, Rn, Rd)
#define ORN_64_REG(shift, Rm, imm6, Rn, Rd) LOGICAL_REG(0b1, 0b01, shift, 0b1, Rm, imm6, Rn, Rd)

#define MOV_32_REG(Rm, Rd) ORR_32_REG(SHIFT_LSL, Rm, 0b000000, ZERO_REG_IDX, Rd)
#define MOVN_32_REG(Rm, Rd) ORN_32_REG(SHIFT_LSL, Rm, 0b000000, ZERO_REG_IDX, Rd)

#define MOV_64_REG(Rm, Rd) ORR_64_REG(SHIFT_LSL, Rm, 0b000000, ZERO_REG_IDX, Rd)
#define MOVN_64_REG(Rm, Rd) ORN_64_REG(SHIFT_LSL, Rm, 0b000000, ZERO_REG_IDX, Rd)

#define BRANCH(op0, op1, op2) (uint32_t)( \
  ((op0) << 29) \
    | (0b101 << 26) \
    | ((op1) << 5) \
    | (op2) \
  )

#define UNCOND_BRANCH_REG(opc, op2, op3, Rn, op4) BRANCH( \
  0b110, \
  (0b1 << 20) \
    | ((opc) << 16) \
    | ((op2) << 11) \
    | (op3 << 5) \
    | (Rn) ,\
  op4 \
)

#define RET(Rn) UNCOND_BRANCH_REG(0b0010, 0b11111, 0b000000, Rn, 00000)

void arm64_emit(struct arm64_emitter *emitter, uint32_t instr) {
  if (emitter->head >= emitter->len) {
    todo("emitter reached size limit");
  }
  
  emitter->block[emitter->head++] = instr;
}

void arm64_emit_mov_32(struct arm64_emitter *emitter, size_t reg_from, size_t reg_to) {
  arm64_emit(emitter, MOV_32_REG(reg_from, reg_to));
}

void arm64_emit_mov_64(struct arm64_emitter *emitter, size_t reg_from, size_t reg_to) {
  arm64_emit(emitter, MOV_64_REG(reg_from, reg_to));
}

void arm64_emit_load_cnst_64(struct arm64_emitter *emitter, size_t reg_idx, uint64_t cnst) {
  switch (cnst) {
  case 0: {
    arm64_emit(emitter, MOV_64_REG(ZERO_REG_IDX, reg_idx));
    break;
  }
  case -1: {
    arm64_emit(emitter, MOVN_64_REG(ZERO_REG_IDX, reg_idx));
    break;
  }
  default: {
    if (cnst <= UINT16_MAX) {
      arm64_emit(emitter, MOVZ_64(/* no shift */ 0, (uint16_t)cnst, reg_idx));
      break;
    } else {
      todo("mov cnst > 2^16");
    }
  }
  }
}

void arm64_emit_load_cnst_32(struct arm64_emitter *emitter, size_t reg_idx, uint32_t cnst) {
  switch (cnst) {
  case 0: {
    arm64_emit(emitter, MOV_32_REG(ZERO_REG_IDX, reg_idx));
    break;
  }
  case -1: {
    arm64_emit(emitter, MOVN_32_REG(ZERO_REG_IDX, reg_idx));
    break;
  }
  default: {
    if (cnst <= UINT16_MAX) {
      arm64_emit(emitter, MOVZ_32(/* no shift */ 0, (uint16_t)cnst, reg_idx));
      break;
    } else {
      todo("mov cnst > 2^16");
    }
  }
  }
}

void arm64_emit_ret(struct arm64_emitter *emitter) {
  arm64_emit(emitter, RET(/* normal reg for return address */ 30));
}
