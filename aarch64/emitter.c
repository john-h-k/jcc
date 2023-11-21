#include "emitter.h"
#include "isa.h"

#include "../alloc.h"
#include "../util.h"
#include <stdlib.h>

#define FITS_IN_BITS(value, bitc) ((value & ~((1 << (bitc - 1)) - 1)) == 0)

const struct aarch64_reg RETURN_REG = { 0 };
const struct aarch64_reg ZERO_REG = { 31 };
const struct aarch64_reg STACK_PTR_REG = { 31 };

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

size_t aarch64_emitted_count(struct aarch64_emitter *emitter) {
  return emitter->head;
}

void aarch64_emit_copy_to(struct aarch64_emitter *emitter, void *dest) {
  memcpy(dest, emitter->block, emitter->head * sizeof(*emitter->block));
}

void free_aarch64_emitter(struct aarch64_emitter **emitter) {
  free((*emitter)->block);

  free(*emitter);
  *emitter = NULL;
}

void aarch64_emit(struct aarch64_emitter *emitter, uint32_t instr) {
  if (emitter->head >= emitter->len) {
    todo("emitter reached size limit");
  }

  emitter->block[emitter->head++] = instr;
}

void aarch64_emit_nop(struct aarch64_emitter *emitter) {
  aarch64_emit(emitter, NOP);
}

uint32_t *aarch64_emit_reserved(struct aarch64_emitter *emitter) {
  uint32_t *instr = &emitter->block[emitter->head];
  aarch64_emit_nop(emitter);
  return instr;
}

void aarch64_emit_add_32(struct aarch64_emitter *emitter, struct aarch64_reg lhs,
                         struct aarch64_reg rhs, struct aarch64_reg dest) {
  aarch64_emit(emitter, ADD_32_REG(0, 0, rhs.idx, lhs.idx, dest.idx));
}

void aarch64_emit_sub_32(struct aarch64_emitter *emitter, struct aarch64_reg lhs,
                         struct aarch64_reg rhs, struct aarch64_reg dest) {
  aarch64_emit(emitter, SUB_32_REG(0, 0, rhs.idx, lhs.idx, dest.idx));
}

void aarch64_emit_mul_32(struct aarch64_emitter *emitter, struct aarch64_reg lhs,
                         struct aarch64_reg rhs, struct aarch64_reg dest) {
  aarch64_emit(emitter, MADD_32(rhs.idx, ZERO_REG.idx, lhs.idx, dest.idx));
}

void aarch64_emit_madd_32(struct aarch64_emitter *emitter, struct aarch64_reg lhs,
                         struct aarch64_reg rhs, struct aarch64_reg add, struct aarch64_reg dest) {
  aarch64_emit(emitter, MADD_32(rhs.idx, add.idx, lhs.idx, dest.idx));
}

void aarch64_emit_msub_32(struct aarch64_emitter *emitter, struct aarch64_reg lhs,
                         struct aarch64_reg rhs, struct aarch64_reg sub, struct aarch64_reg dest) {
  aarch64_emit(emitter, MSUB_32(rhs.idx, sub.idx, lhs.idx, dest.idx));
}

void aarch64_emit_sdiv_32(struct aarch64_emitter *emitter, struct aarch64_reg lhs,
                          struct aarch64_reg rhs, struct aarch64_reg dest) {
  aarch64_emit(emitter, SDIV_32(rhs.idx, lhs.idx, dest.idx));
}

void aarch64_emit_udiv_32(struct aarch64_emitter *emitter, struct aarch64_reg lhs,
                          struct aarch64_reg rhs, struct aarch64_reg dest) {
  aarch64_emit(emitter, UDIV_32(rhs.idx, lhs.idx, dest.idx));
}

void aarch64_emit_mov_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg source, struct aarch64_reg dest) {
  aarch64_emit(emitter, MOV_32_REG(source.idx, dest.idx));
}

void aarch64_emit_mov_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg source, struct aarch64_reg dest) {
  aarch64_emit(emitter, MOV_64_REG(source.idx, dest.idx));
}

void aarch64_emit_load_cnst_64(struct aarch64_emitter *emitter, struct aarch64_reg dest,
                               uint64_t cnst) {
  switch (cnst) {
  case 0: {
    aarch64_emit(emitter, MOV_64_REG(ZERO_REG.idx, dest.idx));
    break;
  }
  case -1: {
    aarch64_emit(emitter, MOVN_64_REG(ZERO_REG.idx, dest.idx));
    break;
  }
  default: {
    if (FITS_IN_BITS(cnst, 16)) {
      aarch64_emit(emitter, MOVZ_64(/* no shift */ 0, (uint16_t)cnst, dest.idx));
      break;
    } else {
      todo("mov cnst > 2^16");
    }
  }
  }
}

void aarch64_emit_load_cnst_32(struct aarch64_emitter *emitter, struct aarch64_reg dest,
                               uint32_t cnst) {
  switch (cnst) {
  case 0: {
    aarch64_emit(emitter, MOV_32_REG(ZERO_REG.idx, dest.idx));
    break;
  }
  case -1: {
    aarch64_emit(emitter, MOVN_32_REG(ZERO_REG.idx, dest.idx));
    break;
  }
  default: {
    if (FITS_IN_BITS(cnst, 16)) {
      aarch64_emit(emitter, MOVZ_32(/* no shift */ 0, (uint16_t)cnst, dest.idx));
      break;
    } else {
      todo("mov cnst > 2^16");
    }
  }
  }
}

void aarch64_emit_load_offset_32(struct aarch64_emitter *emitter, struct aarch64_reg addr, struct aarch64_reg dest, unsigned short offset) {
  aarch64_emit(emitter, LDR_32_IMM_UNSIGNED(offset, addr.idx, dest.idx));
}

void aarch64_emit_store_offset_32(struct aarch64_emitter *emitter, struct aarch64_reg addr, struct aarch64_reg source, unsigned short offset) {
  aarch64_emit(emitter, STR_32_IMM_UNSIGNED(offset, addr.idx, source.idx));
}

void aarch64_emit_ret(struct aarch64_emitter *emitter) {
  aarch64_emit(emitter, RET(/* normal reg for return address */ 30));
}

void aarch64_emit_sub_32_imm(struct aarch64_emitter *emitter,
                         struct aarch64_reg source, size_t imm,
                         struct aarch64_reg dest) {
  aarch64_emit(emitter, SUB_32_IMM(0, imm, source.idx, dest.idx));
}

void aarch64_emit_add_32_imm(struct aarch64_emitter *emitter,
                         struct aarch64_reg source, size_t imm,
                         struct aarch64_reg dest) {
  aarch64_emit(emitter, ADD_32_IMM(0, imm, source.idx, dest.idx));  
}

void aarch64_emit_sub_64_imm(struct aarch64_emitter *emitter,
                         struct aarch64_reg source, size_t imm,
                         struct aarch64_reg dest) {
  aarch64_emit(emitter, SUB_64_IMM(0, imm, source.idx, dest.idx));
}

void aarch64_emit_add_64_imm(struct aarch64_emitter *emitter,
                         struct aarch64_reg source, size_t imm,
                         struct aarch64_reg dest) {
  aarch64_emit(emitter, ADD_64_IMM(0, imm, source.idx, dest.idx));
}

void aarch64_emit_cbz_32_imm(struct aarch64_emitter *emitter, struct aarch64_reg cmp, unsigned offset) {
  invariant_assert(FITS_IN_BITS(offset, 19), "offset too big for branch instruction!");
  aarch64_emit(emitter, CBZ_32_IMM(offset, cmp.idx));
}
void aarch64_emit_cnbz_32_imm(struct aarch64_emitter *emitter, struct aarch64_reg cmp, unsigned offset) {
  invariant_assert(FITS_IN_BITS(offset, 19), "offset too big for branch instruction!");
  aarch64_emit(emitter, CBNZ_32_IMM(offset, cmp.idx));
}
void aarch64_emit_cbz_64_imm(struct aarch64_emitter *emitter, struct aarch64_reg cmp, unsigned offset) {
  invariant_assert(FITS_IN_BITS(offset, 19), "offset too big for branch instruction!");
  aarch64_emit(emitter, CBZ_64_IMM(offset, cmp.idx));
}

void aarch64_emit_cnbz_64_imm(struct aarch64_emitter *emitter, struct aarch64_reg cmp, unsigned offset) {
  invariant_assert(FITS_IN_BITS(offset, 19), "offset too big for branch instruction!");
  aarch64_emit(emitter, CBNZ_64_IMM(offset, cmp.idx));
}

void aarch64_emit_b(struct aarch64_emitter *emitter, unsigned offset) {
  invariant_assert(FITS_IN_BITS(offset, 26), "offset too big for branch instruction!");
  aarch64_emit(emitter, B(offset));
}

void aarch64_emit_bl(struct aarch64_emitter *emitter, unsigned offset) {
  invariant_assert(FITS_IN_BITS(offset, 26), "offset too big for branch instruction!");
  aarch64_emit(emitter, BL(offset));
}
