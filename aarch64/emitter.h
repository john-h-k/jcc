#ifndef AARCH64_EMITTER_H
#define AARCH64_EMITTER_H

#include <stdlib.h>

struct aarch64_reg {
  size_t idx;
};

#if defined(RETURN_REG) || defined(STACK_PTR_REG) || defined(ZERO_REG)
#error "RETURN_REG/STACK_PTR_REG/ZERO_REG already defined. Check your includes"
#endif

// `[w|x]zr` and `sp` are encoded as the same thing and the instruction decides
// which is relevant
#define RETURN_REG ((struct aarch64_reg) {0})
#define ZERO_REG ((struct aarch64_reg) {31})
#define STACK_PTR_REG ((struct aarch64_reg) {31})

enum aarch64_cond {
  // always true
  AARCH64_COND_AL = 0b1110,
  AARCH64_COND_AL_ALT = 0b1111,

  AARCH64_COND_EQ = 0b0000,
  AARCH64_COND_NE = 0b0001,

  /* signed conditions */
  AARCH64_COND_GE = 0b1010,
  AARCH64_COND_LT = 0b1011,
  AARCH64_COND_GT = 0b1100,
  AARCH64_COND_LE = 0b1101,

  // signed overflow & no overflow
  AARCH64_COND_VS = 0b0110,
  AARCH64_COND_VC = 0b0111,

  // carry set & clear
  AARCH64_COND_CS = 0b0010,
  AARCH64_COND_CC = 0b0011,

  /* unsigned conditions */
  AARCH64_COND_HI = 0b1000,
  AARCH64_COND_LS = 0b1001,
  AARCH64_COND_HS = AARCH64_COND_CS,
  AARCH64_COND_LO = AARCH64_COND_CC,

  // minus & positive or zero
  AARCH64_COND_MI = 0b0100,
  AARCH64_COND_PL = 0b0101,
};

struct aarch64_emitter;

void create_aarch64_emitter(struct aarch64_emitter **emitter);
void free_aarch64_emitter(struct aarch64_emitter **emitter);

size_t aarch64_emitted_count(struct aarch64_emitter *emitter);
size_t aarch64_emit_bytesize(struct aarch64_emitter *emitter);
void aarch64_emit_copy_to(struct aarch64_emitter *emitter, void *dest);

/* Emits a `nop` and returns the address such that it can be modified later */
uint32_t *aarch64_emit_reserved(struct aarch64_emitter *emitter);

/* Nop */
void aarch64_emit_nop(struct aarch64_emitter *emitter);

/* Bitfield operations (Immediate) */

void aarch64_emit_sbfm_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest);

void aarch64_emit_sbfm_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest);

void aarch64_emit_bfm_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest);

void aarch64_emit_bfm_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest);

void aarch64_emit_ubfm_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest);

void aarch64_emit_ubfm_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest);

/* Logical (register) */

void aarch64_emit_and_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t shift, size_t imm6);

void aarch64_emit_and_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t shift, size_t imm6);

void aarch64_emit_orr_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t shift, size_t imm6);

void aarch64_emit_orn_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t shift, size_t imm6);

void aarch64_emit_orr_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t shift, size_t imm6);

void aarch64_emit_orn_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t shift, size_t imm6);

/* Logical (immediate) */

void aarch64_emit_eor_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest);

void aarch64_emit_eor_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest);

void aarch64_emit_orr_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest);

void aarch64_emit_orr_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest);

void aarch64_emit_ands_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest);

void aarch64_emit_ands_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest);

void aarch64_emit_and_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest);

void aarch64_emit_and_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr, size_t imms,
                             struct aarch64_reg dest);

/* Add & subtract (register) */

void aarch64_emit_sub_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest);

void aarch64_emit_subs_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest);

void aarch64_emit_add_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest);

void aarch64_emit_adds_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest);

void aarch64_emit_sub_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest);

void aarch64_emit_subs_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest);

void aarch64_emit_add_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest);

void aarch64_emit_adds_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest);

/* Add & subtract (immediate) */

void aarch64_emit_sub_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t imm,
                             struct aarch64_reg dest);

void aarch64_emit_add_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t imm,
                             struct aarch64_reg dest);

void aarch64_emit_sub_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t imm,
                             struct aarch64_reg dest);

void aarch64_emit_add_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t imm,
                             struct aarch64_reg dest);

/* Multiply & multiply-add */

void aarch64_emit_mul_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest);

void aarch64_emit_madd_64(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg add, struct aarch64_reg dest);

void aarch64_emit_msub_64(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg sub, struct aarch64_reg dest);

void aarch64_emit_mul_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest);

void aarch64_emit_madd_32(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg add, struct aarch64_reg dest);

void aarch64_emit_msub_32(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg sub, struct aarch64_reg dest);

/* Division */

void aarch64_emit_sdiv_64(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg dest);

void aarch64_emit_udiv_64(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg dest);

void aarch64_emit_sdiv_32(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg dest);

void aarch64_emit_udiv_32(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg dest);

/* Constant loading */

void aarch64_emit_load_cnst_32(struct aarch64_emitter *emitter,
                               struct aarch64_reg dest, uint32_t cnst);

void aarch64_emit_load_cnst_64(struct aarch64_emitter *emitter,
                               struct aarch64_reg deset, uint64_t cnst);

/* Loads and stores */

void aarch64_emit_load_offset_64(struct aarch64_emitter *emitter,
                                 struct aarch64_reg addr,
                                 struct aarch64_reg dest,
                                 unsigned short offset);
void aarch64_emit_store_offset_64(struct aarch64_emitter *emitter,
                                  struct aarch64_reg addr,
                                  struct aarch64_reg source,
                                  unsigned short offset);

void aarch64_emit_load_offset_32(struct aarch64_emitter *emitter,
                                 struct aarch64_reg addr,
                                 struct aarch64_reg dest,
                                 unsigned short offset);
void aarch64_emit_store_offset_32(struct aarch64_emitter *emitter,
                                  struct aarch64_reg addr,
                                  struct aarch64_reg source,
                                  unsigned short offset);

/* Register moves */

void aarch64_emit_mov_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg source, struct aarch64_reg dest);
void aarch64_emit_mov_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg source, struct aarch64_reg dest);

/* Conditional selects */

void aarch64_emit_csel_32(struct aarch64_emitter *emitter, enum aarch64_cond cond, struct aarch64_reg true_source, struct aarch64_reg false_source, struct aarch64_reg dest);
void aarch64_emit_csinc_32(struct aarch64_emitter *emitter, enum aarch64_cond cond, struct aarch64_reg true_source, struct aarch64_reg false_source, struct aarch64_reg dest);
void aarch64_emit_csinv_32(struct aarch64_emitter *emitter, enum aarch64_cond cond, struct aarch64_reg true_source, struct aarch64_reg false_source, struct aarch64_reg dest);
void aarch64_emit_csneg_32(struct aarch64_emitter *emitter, enum aarch64_cond cond, struct aarch64_reg true_source, struct aarch64_reg false_source, struct aarch64_reg dest);

void aarch64_emit_csel_64(struct aarch64_emitter *emitter, enum aarch64_cond cond, struct aarch64_reg true_source, struct aarch64_reg false_source, struct aarch64_reg dest);
void aarch64_emit_csinc_64(struct aarch64_emitter *emitter, enum aarch64_cond cond, struct aarch64_reg true_source, struct aarch64_reg false_source, struct aarch64_reg dest);
void aarch64_emit_csinv_64(struct aarch64_emitter *emitter, enum aarch64_cond cond, struct aarch64_reg true_source, struct aarch64_reg false_source, struct aarch64_reg dest);
void aarch64_emit_csneg_64(struct aarch64_emitter *emitter, enum aarch64_cond cond, struct aarch64_reg true_source, struct aarch64_reg false_source, struct aarch64_reg dest);

/* Branches */

void aarch64_emit_b(struct aarch64_emitter *emitter, signed offset);
void aarch64_emit_bl(struct aarch64_emitter *emitter, signed offset);

void aarch64_emit_b_cond(struct aarch64_emitter *emitter, signed offset, enum aarch64_cond cond);
void aarch64_emit_bc_cond(struct aarch64_emitter *emitter, signed offset, enum aarch64_cond cond);

void aarch64_emit_cbz_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg cmp, signed offset);
void aarch64_emit_cbnz_32_imm(struct aarch64_emitter *emitter,
                              struct aarch64_reg cmp, signed offset);
void aarch64_emit_cbz_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg cmp, signed offset);
void aarch64_emit_cnbz_64_imm(struct aarch64_emitter *emitter,
                              struct aarch64_reg cmp, signed offset);

void aarch64_emit_ret(struct aarch64_emitter *emitter);

#endif
