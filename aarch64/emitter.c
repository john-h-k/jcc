#include "emitter.h"

#include "../alloc.h"
#include "../util.h"
#include "../log.h"
#include "isa.h"

#include <stdlib.h>

#define SIG_FITS_IN_BITS(value, bitc)                                          \
  ((abs((value)) & ~((1 << (bitc)) - 1)) == 0)
#define UNS_FITS_IN_BITS(value, bitc)                                          \
  (((value) & ~((1 << (bitc)) - 1)) == 0)

#define CLAMP_BITS(value, bitc) ((value) & ((1 << bitc) - 1))

const struct aarch64_reg RETURN_REG = {0};
const struct aarch64_reg ZERO_REG = {31};
const struct aarch64_reg STACK_PTR_REG = {31};

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

/* Emits a `nop` and returns the address such that it can be modified later */
uint32_t *aarch64_emit_reserved(struct aarch64_emitter *emitter) {
  uint32_t *instr = &emitter->block[emitter->head];
  aarch64_emit_nop(emitter);
  return instr;
}

/* Nop */
void aarch64_emit_nop(struct aarch64_emitter *emitter) {
  aarch64_emit(emitter, NOP);
}

/* Bitfield operations (Immediate) */

void aarch64_emit_sbfm_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest) {
  aarch64_emit(emitter, SBFM_32_IMM(immr, imms, source.idx, dest.idx));
}

void aarch64_emit_sbfm_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest) {
  aarch64_emit(emitter, SBFM_64_IMM(immr, imms, source.idx, dest.idx));
}

void aarch64_emit_bfm_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest) {
  aarch64_emit(emitter, BFM_32_IMM(immr, imms, source.idx, dest.idx));
}

void aarch64_emit_bfm_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest) {
  aarch64_emit(emitter, BFM_64_IMM(immr, imms, source.idx, dest.idx));
}

void aarch64_emit_ubfm_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest) {
  aarch64_emit(emitter, UBFM_32_IMM(immr, imms, source.idx, dest.idx));
}

void aarch64_emit_ubfm_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest) {
  aarch64_emit(emitter, UBFM_64_IMM(immr, imms, source.idx, dest.idx));
}

/* Logical (register) */

/* Logical (immediate) */

#define VALIDATE_BITWISE_IMMS() invariant_assert(UNS_FITS_IN_BITS(immr, 6) && UNS_FITS_IN_BITS(imms, 6), "immediate too big for bitwise imm instr");

void aarch64_emit_eor_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest) {
  VALIDATE_BITWISE_IMMS();
  aarch64_emit(emitter, EOR_32_IMM(immr, imms, source.idx, dest.idx));
}

void aarch64_emit_eor_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest) {
  VALIDATE_BITWISE_IMMS();
  aarch64_emit(emitter, EOR_64_IMM(immr, imms, source.idx, dest.idx));
}
void aarch64_emit_orr_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest) {
  VALIDATE_BITWISE_IMMS();
  aarch64_emit(emitter, ORR_32_IMM(immr, imms, source.idx, dest.idx));
}

void aarch64_emit_orr_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest) {
  VALIDATE_BITWISE_IMMS();
  aarch64_emit(emitter, ORR_64_IMM(immr, imms, source.idx, dest.idx));
}

void aarch64_emit_ands_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest) {
  VALIDATE_BITWISE_IMMS();
  aarch64_emit(emitter, ANDS_32_IMM(immr, imms, source.idx, dest.idx));
}

void aarch64_emit_ands_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest) {
  VALIDATE_BITWISE_IMMS();
  aarch64_emit(emitter, ANDS_64_IMM(immr, imms, source.idx, dest.idx));
}

void aarch64_emit_and_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest) {
  warn("%zu immr, %zu imms", immr, imms);
  VALIDATE_BITWISE_IMMS();
  aarch64_emit(emitter, AND_32_IMM(immr, imms, source.idx, dest.idx));
}

void aarch64_emit_and_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest) {
  VALIDATE_BITWISE_IMMS();
  aarch64_emit(emitter, AND_64_IMM(immr, imms, source.idx, dest.idx));
}

/* Add & subtract (register) */

void aarch64_emit_add_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest) {
  aarch64_emit(emitter, ADD_64_REG(0, 0, rhs.idx, lhs.idx, dest.idx));
}

void aarch64_emit_adds_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest) {
  aarch64_emit(emitter, ADDS_64_REG(0, 0, rhs.idx, lhs.idx, dest.idx));
}

void aarch64_emit_sub_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest) {
  aarch64_emit(emitter, SUB_64_REG(0, 0, rhs.idx, lhs.idx, dest.idx));
}

void aarch64_emit_subs_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest) {
  aarch64_emit(emitter, SUBS_64_REG(0, 0, rhs.idx, lhs.idx, dest.idx));
}

void aarch64_emit_add_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest) {
  aarch64_emit(emitter, ADD_32_REG(0, 0, rhs.idx, lhs.idx, dest.idx));
}

void aarch64_emit_adds_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest) {
  aarch64_emit(emitter, ADDS_32_REG(0, 0, rhs.idx, lhs.idx, dest.idx));
}

void aarch64_emit_sub_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest) {
  aarch64_emit(emitter, SUB_32_REG(0, 0, rhs.idx, lhs.idx, dest.idx));
}

void aarch64_emit_subs_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest) {
  aarch64_emit(emitter, SUBS_32_REG(0, 0, rhs.idx, lhs.idx, dest.idx));
}

/* Add & subtract (immediate) */

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

/* Multiply & multiply-add */

void aarch64_emit_mul_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest) {
  aarch64_emit(emitter, MADD_64(rhs.idx, ZERO_REG.idx, lhs.idx, dest.idx));
}

void aarch64_emit_madd_64(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg add, struct aarch64_reg dest) {
  aarch64_emit(emitter, MADD_64(rhs.idx, add.idx, lhs.idx, dest.idx));
}

void aarch64_emit_msub_64(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg sub, struct aarch64_reg dest) {
  aarch64_emit(emitter, MSUB_64(rhs.idx, sub.idx, lhs.idx, dest.idx));
}

void aarch64_emit_mul_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest) {
  aarch64_emit(emitter, MADD_32(rhs.idx, ZERO_REG.idx, lhs.idx, dest.idx));
}

void aarch64_emit_madd_32(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg add, struct aarch64_reg dest) {
  aarch64_emit(emitter, MADD_32(rhs.idx, add.idx, lhs.idx, dest.idx));
}

void aarch64_emit_msub_32(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg sub, struct aarch64_reg dest) {
  aarch64_emit(emitter, MSUB_32(rhs.idx, sub.idx, lhs.idx, dest.idx));
}

/* Division */

void aarch64_emit_sdiv_64(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg dest) {
  aarch64_emit(emitter, SDIV_64(rhs.idx, lhs.idx, dest.idx));
}

void aarch64_emit_udiv_64(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg dest) {
  aarch64_emit(emitter, UDIV_64(rhs.idx, lhs.idx, dest.idx));
}

void aarch64_emit_sdiv_32(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg dest) {
  aarch64_emit(emitter, SDIV_32(rhs.idx, lhs.idx, dest.idx));
}

void aarch64_emit_udiv_32(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg dest) {
  aarch64_emit(emitter, UDIV_32(rhs.idx, lhs.idx, dest.idx));
}

/* Constant loading */

void aarch64_emit_load_cnst_64(struct aarch64_emitter *emitter,
                               struct aarch64_reg dest, uint64_t cnst) {
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
    if (UNS_FITS_IN_BITS(cnst, 16)) {
      aarch64_emit(emitter,
                   MOVZ_64(/* no shift */ 0, (uint16_t)cnst, dest.idx));
      break;
    } else {
      todo("mov cnst > 2^16");
    }
  }
  }
}

void aarch64_emit_load_cnst_32(struct aarch64_emitter *emitter,
                               struct aarch64_reg dest, uint32_t cnst) {
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
    if (UNS_FITS_IN_BITS(cnst, 16)) {
      aarch64_emit(emitter,
                   MOVZ_32(/* no shift */ 0, (uint16_t)cnst, dest.idx));
      break;
    } else {
      todo("mov cnst > 2^16");
    }
  }
  }
}

/* Loads and stores */

void aarch64_emit_load_offset_64(struct aarch64_emitter *emitter,
                                 struct aarch64_reg addr,
                                 struct aarch64_reg dest,
                                 unsigned short offset) {
  aarch64_emit(emitter, LDR_64_IMM_UNSIGNED(offset, addr.idx, dest.idx));
}

void aarch64_emit_store_offset_64(struct aarch64_emitter *emitter,
                                  struct aarch64_reg addr,
                                  struct aarch64_reg source,
                                  unsigned short offset) {
  aarch64_emit(emitter, STR_64_IMM_UNSIGNED(offset, addr.idx, source.idx));
}

void aarch64_emit_load_offset_32(struct aarch64_emitter *emitter,
                                 struct aarch64_reg addr,
                                 struct aarch64_reg dest,
                                 unsigned short offset) {
  aarch64_emit(emitter, LDR_32_IMM_UNSIGNED(offset, addr.idx, dest.idx));
}

void aarch64_emit_store_offset_32(struct aarch64_emitter *emitter,
                                  struct aarch64_reg addr,
                                  struct aarch64_reg source,
                                  unsigned short offset) {
  aarch64_emit(emitter, STR_32_IMM_UNSIGNED(offset, addr.idx, source.idx));
}

/* Register moves */

void aarch64_emit_mov_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg source, struct aarch64_reg dest) {
  aarch64_emit(emitter, MOV_32_REG(source.idx, dest.idx));
}

void aarch64_emit_mov_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg source, struct aarch64_reg dest) {
  aarch64_emit(emitter, MOV_64_REG(source.idx, dest.idx));
}

/* Branches */

void aarch64_emit_b_cond(struct aarch64_emitter *emitter, signed offset, enum aarch64_cond cond) {
  invariant_assert(SIG_FITS_IN_BITS(offset, 19),
                   "offset too big for branch instruction!");
  aarch64_emit(emitter, B_COND(CLAMP_BITS(offset, 19), cond));
}

void aarch64_emit_bc_cond(struct aarch64_emitter *emitter, signed offset, enum aarch64_cond cond) {
  invariant_assert(SIG_FITS_IN_BITS(offset, 19),
                   "offset too big for branch instruction!");
  aarch64_emit(emitter, BC_COND(CLAMP_BITS(offset, 19), cond));
}

void aarch64_emit_b(struct aarch64_emitter *emitter, signed offset) {
  invariant_assert(SIG_FITS_IN_BITS(offset, 26),
                   "offset too big for branch instruction!");
  aarch64_emit(emitter, B(CLAMP_BITS(offset, 26)));
}

void aarch64_emit_bl(struct aarch64_emitter *emitter, signed offset) {
  invariant_assert(SIG_FITS_IN_BITS(offset, 26),
                   "offset too big for branch instruction!");
  aarch64_emit(emitter, BL(CLAMP_BITS(offset, 26)));
}

void aarch64_emit_cbz_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg cmp, signed offset) {
  invariant_assert(SIG_FITS_IN_BITS(offset, 19),
                   "offset too big for branch instruction!");
  aarch64_emit(emitter, CBZ_32_IMM(CLAMP_BITS(offset, 19), cmp.idx));
}

void aarch64_emit_cnbz_32_imm(struct aarch64_emitter *emitter,
                              struct aarch64_reg cmp, signed offset) {
  invariant_assert(SIG_FITS_IN_BITS(offset, 19),
                   "offset too big for branch instruction!");
  aarch64_emit(emitter, CBNZ_32_IMM(CLAMP_BITS(offset, 19), cmp.idx));
}

void aarch64_emit_cbz_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg cmp, signed offset) {
  invariant_assert(SIG_FITS_IN_BITS(offset, 19),
                   "offset too big for branch instruction!");
  aarch64_emit(emitter, CBZ_64_IMM(offset, cmp.idx));
}

void aarch64_emit_cnbz_64_imm(struct aarch64_emitter *emitter,
                              struct aarch64_reg cmp, signed offset) {
  invariant_assert(SIG_FITS_IN_BITS(offset, 19),
                   "offset too big for branch instruction!");
  aarch64_emit(emitter, CBNZ_64_IMM(CLAMP_BITS(offset, 19), cmp.idx));
}

void aarch64_emit_ret(struct aarch64_emitter *emitter) {
  aarch64_emit(emitter, RET(/* normal reg for return address */ 30));
}

