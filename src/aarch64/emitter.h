#ifndef AARCH64_EMITTER_H
#define AARCH64_EMITTER_H

#include "codegen.h"
#include "isa.h"

#include <stddef.h>
#include <stdlib.h>

struct aarch64_emitter;

void create_aarch64_emitter(struct aarch64_emitter **emitter);
void free_aarch64_emitter(struct aarch64_emitter **emitter);

size_t aarch64_emitted_count(struct aarch64_emitter *emitter);
size_t aarch64_emit_bytesize(struct aarch64_emitter *emitter);
void aarch64_emit_copy_to(struct aarch64_emitter *emitter, void *dest);

/* Nop */

void aarch64_emit_nop(struct aarch64_emitter *emitter);

/* Single reg FP data processing */

void aarch64_emit_scvtf(struct aarch64_emitter *emitter,
                        const struct aarch64_reg_1_source ucvtf);
void aarch64_emit_ucvtf(struct aarch64_emitter *emitter,
                        const struct aarch64_reg_1_source scvtf);
void aarch64_emit_fcvt(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_1_source fcvt);
void aarch64_emit_fmov(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_1_source fmov);
void aarch64_emit_fneg(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_1_source fneg);
void aarch64_emit_fsqrt(struct aarch64_emitter *emitter,
                        const struct aarch64_reg_1_source fsqrt);
void aarch64_emit_fabs(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_1_source fabs);

/* Two reg FP data processing */

void aarch64_emit_fcmp(struct aarch64_emitter *emitter,
                       const struct aarch64_fcmp fcmp);
void aarch64_emit_fcmp_zero(struct aarch64_emitter *emitter,
                            const struct aarch64_fcmp_zero fcmp_zero);

void aarch64_emit_fadd(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source fadd);
void aarch64_emit_fsub(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source fsub);
void aarch64_emit_fmul(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source fmul);
void aarch64_emit_fdiv(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source fdiv);

void aarch64_emit_fmaxnm(struct aarch64_emitter *emitter,
                         const struct aarch64_reg_2_source fmaxnm);
void aarch64_emit_fminnm(struct aarch64_emitter *emitter,
                         const struct aarch64_reg_2_source fminnm);

/* Register moves */

void aarch64_emit_movz(struct aarch64_emitter *emitter,
                       const struct aarch64_mov_imm movz);
void aarch64_emit_movk(struct aarch64_emitter *emitter,
                       const struct aarch64_mov_imm movk);
void aarch64_emit_movn(struct aarch64_emitter *emitter,
                       const struct aarch64_mov_imm movn);

/* Bitfield operations (Immediate) */

void aarch64_emit_sbfm(struct aarch64_emitter *emitter,
                       const struct aarch64_bitfield bf);
void aarch64_emit_bfm(struct aarch64_emitter *emitter,
                      const struct aarch64_bitfield bf);
void aarch64_emit_ubfm(struct aarch64_emitter *emitter,
                       const struct aarch64_bitfield bf);

/* Logical (register) */

void aarch64_emit_and(struct aarch64_emitter *emitter,
                      const struct aarch64_logical_reg log);
void aarch64_emit_ands(struct aarch64_emitter *emitter,
                       const struct aarch64_logical_reg log);
void aarch64_emit_orr(struct aarch64_emitter *emitter,
                      const struct aarch64_logical_reg log);
void aarch64_emit_orn(struct aarch64_emitter *emitter,
                      const struct aarch64_logical_reg log);
void aarch64_emit_eor(struct aarch64_emitter *emitter,
                      const struct aarch64_logical_reg log);
void aarch64_emit_eon(struct aarch64_emitter *emitter,
                      const struct aarch64_logical_reg log);

/* Logical (immediate) */

void aarch64_emit_and_imm(struct aarch64_emitter *emitter,
                          const struct aarch64_logical_imm log);
void aarch64_emit_ands_imm(struct aarch64_emitter *emitter,
                           const struct aarch64_logical_imm log);
void aarch64_emit_orr_imm(struct aarch64_emitter *emitter,
                          const struct aarch64_logical_imm log);
void aarch64_emit_eor_imm(struct aarch64_emitter *emitter,
                          const struct aarch64_logical_imm log);

/* Add & subtract (register) */

void aarch64_emit_sub(struct aarch64_emitter *emitter,
                      const struct aarch64_addsub_reg sub);
void aarch64_emit_subs(struct aarch64_emitter *emitter,
                       const struct aarch64_addsub_reg subs);
void aarch64_emit_add(struct aarch64_emitter *emitter,
                      const struct aarch64_addsub_reg add);
void aarch64_emit_adds(struct aarch64_emitter *emitter,
                       const struct aarch64_addsub_reg adds);

/* Add & subtract (extended register) */

void aarch64_emit_sub_ext(struct aarch64_emitter *emitter,
                          const struct aarch64_addsub_ext sub_ext);
void aarch64_emit_subs_ext(struct aarch64_emitter *emitter,
                           const struct aarch64_addsub_ext subs_ext);
void aarch64_emit_add_ext(struct aarch64_emitter *emitter,
                          const struct aarch64_addsub_ext add_ext);
void aarch64_emit_adds_ext(struct aarch64_emitter *emitter,
                           const struct aarch64_addsub_ext adds_ext);

/* Addressing (immediate) */

void aarch64_emit_adr(struct aarch64_emitter *emitter,
                      const struct aarch64_addr_imm addr);
void aarch64_emit_adrp(struct aarch64_emitter *emitter,
                       const struct aarch64_addr_imm addr);

/* Add & subtract (immediate) */

void aarch64_emit_sub_imm(struct aarch64_emitter *emitter,
                          const struct aarch64_addsub_imm sub);
void aarch64_emit_subs_imm(struct aarch64_emitter *emitter,
                           const struct aarch64_addsub_imm sub);
void aarch64_emit_add_imm(struct aarch64_emitter *emitter,
                          const struct aarch64_addsub_imm add);
void aarch64_emit_adds_imm(struct aarch64_emitter *emitter,
                           const struct aarch64_addsub_imm add);

/* Multiply & multiply-add */

void aarch64_emit_madd(struct aarch64_emitter *emitter,
                       const struct aarch64_fma fma);
void aarch64_emit_msub(struct aarch64_emitter *emitter,
                       const struct aarch64_fma fma);

/* Shifts */

void aarch64_emit_lslv(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source shift);
void aarch64_emit_lsrv(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source shift);
void aarch64_emit_asrv(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source shift);
void aarch64_emit_rorv(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source shift);

/* Division */

void aarch64_emit_sdiv(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source div);
void aarch64_emit_udiv(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source div);

/* Loads and stores */

void aarch64_emit_load_pair_imm(struct aarch64_emitter *emitter,
                                const struct aarch64_load_pair_imm ldp);
void aarch64_emit_store_pair_imm(struct aarch64_emitter *emitter,
                                 const struct aarch64_store_pair_imm stp);

void aarch64_emit_load_byte_imm(struct aarch64_emitter *emitter,
                                const struct aarch64_load_imm ldrb);
void aarch64_emit_store_byte_imm(struct aarch64_emitter *emitter,
                                 const struct aarch64_store_imm strb);
void aarch64_emit_load_half_imm(struct aarch64_emitter *emitter,
                                const struct aarch64_load_imm ldrh);
void aarch64_emit_store_half_imm(struct aarch64_emitter *emitter,
                                 const struct aarch64_store_imm strh);
void aarch64_emit_load_imm(struct aarch64_emitter *emitter,
                           const struct aarch64_load_imm ldr);
void aarch64_emit_store_imm(struct aarch64_emitter *emitter,
                            const struct aarch64_store_imm str);

void aarch64_emit_load_byte(struct aarch64_emitter *emitter,
                            const struct aarch64_load ldrb);
void aarch64_emit_store_byte(struct aarch64_emitter *emitter,
                             const struct aarch64_store strb);
void aarch64_emit_load_half(struct aarch64_emitter *emitter,
                            const struct aarch64_load ldrh);
void aarch64_emit_store_half(struct aarch64_emitter *emitter,
                             const struct aarch64_store strh);
void aarch64_emit_load(struct aarch64_emitter *emitter,
                       const struct aarch64_load ldr);
void aarch64_emit_store(struct aarch64_emitter *emitter,
                        const struct aarch64_store str);

/* Conditional selects */

void aarch64_emit_csel(struct aarch64_emitter *emitter,
                       const struct aarch64_conditional_select select);
void aarch64_emit_csinc(struct aarch64_emitter *emitter,
                        const struct aarch64_conditional_select select);
void aarch64_emit_csinv(struct aarch64_emitter *emitter,
                        const struct aarch64_conditional_select select);
void aarch64_emit_csneg(struct aarch64_emitter *emitter,
                        const struct aarch64_conditional_select select);

/* Branches */

void aarch64_emit_b(struct aarch64_emitter *emitter,
                    const struct aarch64_branch b);
void aarch64_emit_br(struct aarch64_emitter *emitter,
                     const struct aarch64_branch_reg br);
void aarch64_emit_bl(struct aarch64_emitter *emitter,
                     const struct aarch64_branch bl);
void aarch64_emit_blr(struct aarch64_emitter *emitter,
                      const struct aarch64_branch_reg blr);

void aarch64_emit_b_cond(struct aarch64_emitter *emitter,
                         const struct aarch64_conditional_branch br);
void aarch64_emit_bc_cond(struct aarch64_emitter *emitter,
                          const struct aarch64_conditional_branch br);

void aarch64_emit_cbz(struct aarch64_emitter *emitter,
                      const struct aarch64_compare_and_branch cmp);
void aarch64_emit_cbnz(struct aarch64_emitter *emitter,
                       const struct aarch64_compare_and_branch cmp);

void aarch64_emit_ret(struct aarch64_emitter *emitter,
                      const struct aarch64_branch_reg ret);

#endif
