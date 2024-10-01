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

/* Register moves */

void aarch64_emit_mov_imm(struct aarch64_emitter *emitter, const struct aarch64_mov_imm mov);

/* Bitfield operations (Immediate) */

void aarch64_emit_sbfm_imm(struct aarch64_emitter *emitter, const struct aarch64_bitfield_imm bf);
void aarch64_emit_bfm_imm(struct aarch64_emitter *emitter, const struct aarch64_bitfield_imm bf);
void aarch64_emit_ubfm_imm(struct aarch64_emitter *emitter, const struct aarch64_bitfield_imm bf);

/* Logical (register) */

void aarch64_emit_mvn(struct aarch64_emitter *emitter, const struct aarch64_reg_1_source_with_shift mvn);

void aarch64_emit_and(struct aarch64_emitter *emitter, const struct aarch64_logical_reg log);
void aarch64_emit_orr(struct aarch64_emitter *emitter, const struct aarch64_logical_reg log);
void aarch64_emit_orn(struct aarch64_emitter *emitter, const struct aarch64_logical_reg log);
void aarch64_emit_eor(struct aarch64_emitter *emitter, const struct aarch64_logical_reg log);
void aarch64_emit_eon(struct aarch64_emitter *emitter, const struct aarch64_logical_reg log);
void aarch64_emit_ands(struct aarch64_emitter *emitter, const struct aarch64_logical_reg log);

/* Logical (immediate) */

void aarch64_emit_and_imm(struct aarch64_emitter *emitter, const struct aarch64_logical_imm log);
void aarch64_emit_orr_imm(struct aarch64_emitter *emitter, const struct aarch64_logical_imm log);
void aarch64_emit_eor_imm(struct aarch64_emitter *emitter, const struct aarch64_logical_imm log);
void aarch64_emit_ands_imm(struct aarch64_emitter *emitter, const struct aarch64_logical_imm log);


/* Add & subtract (register) */

void aarch64_emit_sub(struct aarch64_emitter *emitter, const struct aarch64_addsub_reg sub);
void aarch64_emit_subs(struct aarch64_emitter *emitter, const struct aarch64_addsub_reg subs);
void aarch64_emit_add(struct aarch64_emitter *emitter, const struct aarch64_addsub_reg add);
void aarch64_emit_adds(struct aarch64_emitter *emitter, const struct aarch64_addsub_reg adds);

/* Addressing (immediate) */

void aarch64_emit_adr(struct aarch64_emitter *emitter, const struct aarch64_addr_imm addr);
void aarch64_emit_adrp(struct aarch64_emitter *emitter, const struct aarch64_addr_imm addr);

/* Add & subtract (immediate) */

void aarch64_emit_sub_imm(struct aarch64_emitter *emitter, const struct aarch64_addsub_imm sub);
void aarch64_emit_subs_imm(struct aarch64_emitter *emitter, const struct aarch64_addsub_imm sub);
void aarch64_emit_add_imm(struct aarch64_emitter *emitter, const struct aarch64_addsub_imm add);
void aarch64_emit_adds_imm(struct aarch64_emitter *emitter, const struct aarch64_addsub_imm add);

/* Multiply & multiply-add */

void aarch64_emit_madd(struct aarch64_emitter *emitter, const struct aarch64_fma fma);
void aarch64_emit_msub(struct aarch64_emitter *emitter, const struct aarch64_fma fma);

/* Shifts */

void aarch64_emit_lslv(struct aarch64_emitter *emitter, const struct aarch64_reg_2_source shift);
void aarch64_emit_lsrv(struct aarch64_emitter *emitter, const struct aarch64_reg_2_source shift);
void aarch64_emit_asrv(struct aarch64_emitter *emitter, const struct aarch64_reg_2_source shift);
void aarch64_emit_rorv(struct aarch64_emitter *emitter, const struct aarch64_reg_2_source shift);

/* Division */

void aarch64_emit_sdiv(struct aarch64_emitter *emitter, const struct aarch64_reg_2_source div);
void aarch64_emit_udiv(struct aarch64_emitter *emitter, const struct aarch64_reg_2_source div);

/* Loads and stores */

void aarch64_emit_load_pair_imm(struct aarch64_emitter *emitter, const struct aarch64_load_pair_imm ldp);
void aarch64_emit_store_pair_imm(struct aarch64_emitter *emitter, const struct aarch64_store_pair_imm stp);

void aarch64_emit_load_imm(struct aarch64_emitter *emitter, const struct aarch64_load_imm ldr);
void aarch64_emit_store_imm(struct aarch64_emitter *emitter, const struct aarch64_store_imm str);

/* Conditional selects */

void aarch64_emit_csel(struct aarch64_emitter *emitter, const struct aarch64_conditional_select select);
void aarch64_emit_csinc(struct aarch64_emitter *emitter, const struct aarch64_conditional_select select);
void aarch64_emit_csinv(struct aarch64_emitter *emitter, const struct aarch64_conditional_select select);
void aarch64_emit_csneg(struct aarch64_emitter *emitter, const struct aarch64_conditional_select select);

/* Branches */

void aarch64_emit_b(struct aarch64_emitter *emitter, const struct aarch64_branch br);
void aarch64_emit_bl(struct aarch64_emitter *emitter, const struct aarch64_branch br);

void aarch64_emit_b_cond(struct aarch64_emitter *emitter, const struct aarch64_conditional_branch br);
void aarch64_emit_bc_cond(struct aarch64_emitter *emitter, const struct aarch64_conditional_branch br);

void aarch64_emit_cbz(struct aarch64_emitter *emitter, const struct aarch64_compare_and_branch cmp);
void aarch64_emit_cbnz(struct aarch64_emitter *emitter, const struct aarch64_compare_and_branch cmp);

void aarch64_emit_ret(struct aarch64_emitter *emitter, const struct aarch64_ret ret);

#endif
