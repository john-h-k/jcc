#ifndef AARCH64_EMITTER_H
#define AARCH64_EMITTER_H

#include <stddef.h>
#include <stdlib.h>

#include "codegen.h"

struct aarch64_emitter;

void create_aarch64_emitter(struct aarch64_emitter **emitter);
void free_aarch64_emitter(struct aarch64_emitter **emitter);

size_t aarch64_emitted_count(struct aarch64_emitter *emitter);
size_t aarch64_emit_bytesize(struct aarch64_emitter *emitter);
void aarch64_emit_copy_to(struct aarch64_emitter *emitter, void *dest);

/* Nop */
void aarch64_emit_nop(struct aarch64_emitter *emitter);

/* Bitfield operations (Immediate) */

void aarch64_emit_sbfm_32_imm(struct aarch64_emitter *emitter,
                              struct aarch64_reg source, size_t immr,
                              size_t imms, struct aarch64_reg dest);

void aarch64_emit_sbfm_64_imm(struct aarch64_emitter *emitter,
                              struct aarch64_reg source, size_t immr,
                              size_t imms, struct aarch64_reg dest);

void aarch64_emit_bfm_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr,
                             size_t imms, struct aarch64_reg dest);

void aarch64_emit_bfm_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr,
                             size_t imms, struct aarch64_reg dest);

void aarch64_emit_ubfm_32_imm(struct aarch64_emitter *emitter,
                              struct aarch64_reg source, size_t immr,
                              size_t imms, struct aarch64_reg dest);

void aarch64_emit_ubfm_64_imm(struct aarch64_emitter *emitter,
                              struct aarch64_reg source, size_t immr,
                              size_t imms, struct aarch64_reg dest);

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

void aarch64_emit_orr_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t shift, size_t imm6);

void aarch64_emit_orn_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t shift, size_t imm6);

void aarch64_emit_orn_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t shift, size_t imm6);

void aarch64_emit_eor_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t shift, size_t imm6);

void aarch64_emit_eor_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t shift, size_t imm6);

void aarch64_emit_eon_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t shift, size_t imm6);

void aarch64_emit_eon_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t shift, size_t imm6);

/* Logical (immediate) */

void aarch64_emit_eor_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr,
                             size_t imms, struct aarch64_reg dest);

void aarch64_emit_eor_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr,
                             size_t imms, struct aarch64_reg dest);

void aarch64_emit_orr_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr,
                             size_t imms, struct aarch64_reg dest);

void aarch64_emit_orr_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr,
                             size_t imms, struct aarch64_reg dest);

void aarch64_emit_ands_32_imm(struct aarch64_emitter *emitter,
                              struct aarch64_reg source, size_t immr,
                              size_t imms, struct aarch64_reg dest);

void aarch64_emit_ands_64_imm(struct aarch64_emitter *emitter,
                              struct aarch64_reg source, size_t immr,
                              size_t imms, struct aarch64_reg dest);

void aarch64_emit_and_32_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr,
                             size_t imms, struct aarch64_reg dest);

void aarch64_emit_and_64_imm(struct aarch64_emitter *emitter,
                             struct aarch64_reg source, size_t immr,
                             size_t imms, struct aarch64_reg dest);

/* Add & subtract (register) */

void aarch64_emit_sub_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t imm6, enum aarch64_shift shift);

void aarch64_emit_subs_64(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t imm6, enum aarch64_shift shift);

void aarch64_emit_add_64(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t imm6, enum aarch64_shift shift);

void aarch64_emit_adds_64(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t imm6, enum aarch64_shift shift);

void aarch64_emit_sub_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t imm6, enum aarch64_shift shift);

void aarch64_emit_subs_32(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t imm6, enum aarch64_shift shift);

void aarch64_emit_add_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t imm6, enum aarch64_shift shift);

void aarch64_emit_adds_32(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest, size_t imm6, enum aarch64_shift shift);

/* Addressing (immediate) */

void aarch64_emit_adr(struct aarch64_emitter *emitter, int imm,
                      struct aarch64_reg dest);

void aarch64_emit_adrp(struct aarch64_emitter *emitter, int imm,
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

/* Shifts */

void aarch64_emit_lslv_64(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg dest);

void aarch64_emit_lsrv_64(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg dest);

void aarch64_emit_asrv_64(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg dest);

void aarch64_emit_rorv_64(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg dest);

void aarch64_emit_lslv_32(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg dest);

void aarch64_emit_lsrv_32(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg dest);

void aarch64_emit_asrv_32(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg dest);

void aarch64_emit_rorv_32(struct aarch64_emitter *emitter,
                          struct aarch64_reg lhs, struct aarch64_reg rhs,
                          struct aarch64_reg dest);

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
                               struct aarch64_reg deset, int64_t cnst);

/* Loads and stores */

void aarch64_emit_store_pair_post_index_32(struct aarch64_emitter *emitter,
                                           struct aarch64_reg addr,
                                           struct aarch64_reg source0,
                                           struct aarch64_reg source1,
                                           unsigned short offset);

void aarch64_emit_load_pair_post_index_32(struct aarch64_emitter *emitter,
                                          struct aarch64_reg addr,
                                          struct aarch64_reg dest0,
                                          struct aarch64_reg dest1,
                                          unsigned short offset);

void aarch64_emit_store_pair_post_index_64(struct aarch64_emitter *emitter,
                                           struct aarch64_reg addr,
                                           struct aarch64_reg source0,
                                           struct aarch64_reg source1,
                                           unsigned short offset);

void aarch64_emit_load_pair_post_index_64(struct aarch64_emitter *emitter,
                                          struct aarch64_reg addr,
                                          struct aarch64_reg dest0,
                                          struct aarch64_reg dest1,
                                          unsigned short offset);

void aarch64_emit_store_pair_pre_index_32(struct aarch64_emitter *emitter,
                                          struct aarch64_reg addr,
                                          struct aarch64_reg source0,
                                          struct aarch64_reg source1,
                                          unsigned short offset);

void aarch64_emit_load_pair_pre_index_32(struct aarch64_emitter *emitter,
                                         struct aarch64_reg addr,
                                         struct aarch64_reg dest0,
                                         struct aarch64_reg dest1,
                                         unsigned short offset);

void aarch64_emit_store_pair_pre_index_64(struct aarch64_emitter *emitter,
                                          struct aarch64_reg addr,
                                          struct aarch64_reg source0,
                                          struct aarch64_reg source1,
                                          unsigned short offset);

void aarch64_emit_load_pair_pre_index_64(struct aarch64_emitter *emitter,
                                         struct aarch64_reg addr,
                                         struct aarch64_reg dest0,
                                         struct aarch64_reg dest1,
                                         unsigned short offset);

void aarch64_emit_store_pair_offset_64(struct aarch64_emitter *emitter,
                                       struct aarch64_reg addr,
                                       struct aarch64_reg source0,
                                       struct aarch64_reg source1,
                                       unsigned short offset);

void aarch64_emit_load_pair_offset_64(struct aarch64_emitter *emitter,
                                      struct aarch64_reg addr,
                                      struct aarch64_reg dest0,
                                      struct aarch64_reg dest1,
                                      unsigned short offset);

void aarch64_emit_store_pair_offset_32(struct aarch64_emitter *emitter,
                                       struct aarch64_reg addr,
                                       struct aarch64_reg source0,
                                       struct aarch64_reg source1,
                                       unsigned short offset);

void aarch64_emit_load_pair_offset_32(struct aarch64_emitter *emitter,
                                      struct aarch64_reg addr,
                                      struct aarch64_reg dest0,
                                      struct aarch64_reg dest1,
                                      unsigned short offset);

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

void aarch64_emit_movn_32(struct aarch64_emitter *emitter,
                          struct aarch64_reg source, struct aarch64_reg dest);
void aarch64_emit_movn_64(struct aarch64_emitter *emitter,
                          struct aarch64_reg source, struct aarch64_reg dest);

/* Conditional selects */

void aarch64_emit_csel_32(struct aarch64_emitter *emitter,
                          enum aarch64_cond cond,
                          struct aarch64_reg true_source,
                          struct aarch64_reg false_source,
                          struct aarch64_reg dest);
void aarch64_emit_csinc_32(struct aarch64_emitter *emitter,
                           enum aarch64_cond cond,
                           struct aarch64_reg true_source,
                           struct aarch64_reg false_source,
                           struct aarch64_reg dest);
void aarch64_emit_csinv_32(struct aarch64_emitter *emitter,
                           enum aarch64_cond cond,
                           struct aarch64_reg true_source,
                           struct aarch64_reg false_source,
                           struct aarch64_reg dest);
void aarch64_emit_csneg_32(struct aarch64_emitter *emitter,
                           enum aarch64_cond cond,
                           struct aarch64_reg true_source,
                           struct aarch64_reg false_source,
                           struct aarch64_reg dest);

void aarch64_emit_csel_64(struct aarch64_emitter *emitter,
                          enum aarch64_cond cond,
                          struct aarch64_reg true_source,
                          struct aarch64_reg false_source,
                          struct aarch64_reg dest);
void aarch64_emit_csinc_64(struct aarch64_emitter *emitter,
                           enum aarch64_cond cond,
                           struct aarch64_reg true_source,
                           struct aarch64_reg false_source,
                           struct aarch64_reg dest);
void aarch64_emit_csinv_64(struct aarch64_emitter *emitter,
                           enum aarch64_cond cond,
                           struct aarch64_reg true_source,
                           struct aarch64_reg false_source,
                           struct aarch64_reg dest);
void aarch64_emit_csneg_64(struct aarch64_emitter *emitter,
                           enum aarch64_cond cond,
                           struct aarch64_reg true_source,
                           struct aarch64_reg false_source,
                           struct aarch64_reg dest);

/* Branches */

void aarch64_emit_b(struct aarch64_emitter *emitter, signed offset);
void aarch64_emit_bl(struct aarch64_emitter *emitter, signed offset);

void aarch64_emit_b_cond(struct aarch64_emitter *emitter, signed offset,
                         enum aarch64_cond cond);
void aarch64_emit_bc_cond(struct aarch64_emitter *emitter, signed offset,
                          enum aarch64_cond cond);

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
