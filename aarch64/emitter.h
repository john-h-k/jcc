#ifndef AARCH64_EMITTER_H
#define AARCH64_EMITTER_H

#include <stdlib.h>

struct aarch64_reg {
  size_t idx;
};

// `[w|x]zr` and `sp` are encoded as the same thing and the instruction decides
// which is relevant
const struct aarch64_reg RETURN_REG;
const struct aarch64_reg ZERO_REG;
const struct aarch64_reg STACK_PTR_REG;

struct aarch64_emitter;

void create_aarch64_emitter(struct aarch64_emitter **emitter);
void free_aarch64_emitter(struct aarch64_emitter **emitter);

size_t aarch64_emit_bytesize(struct aarch64_emitter *emitter);
void aarch64_emit_copy_to(struct aarch64_emitter *emitter, void *dest);

/* Emits a `nop` and returns the address such that it can be modified later */

uint32_t *aarch64_emit_reserved(struct aarch64_emitter *emitter);

/* Nop */

void aarch64_emit_nop(struct aarch64_emitter *emitter);

/* Add & subtract (register) */

void aarch64_emit_sub_32(struct aarch64_emitter *emitter,
                         struct aarch64_reg lhs, struct aarch64_reg rhs,
                         struct aarch64_reg dest);

void aarch64_emit_add_32(struct aarch64_emitter *emitter,
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

/* Branches */

size_t aarch64_emitted_count(struct aarch64_emitter *emitter);

void aarch64_emit_b(struct aarch64_emitter *emitter, unsigned offset);
void aarch64_emit_bl(struct aarch64_emitter *emitter, unsigned offset);

void aarch64_emit_cbz_32_imm(struct aarch64_emitter *emitter, struct aarch64_reg cmp, unsigned offset);
void aarch64_emit_cnbz_32_imm(struct aarch64_emitter *emitter, struct aarch64_reg cmp, unsigned offset);
void aarch64_emit_cbz_64_imm(struct aarch64_emitter *emitter, struct aarch64_reg cmp, unsigned offset);
void aarch64_emit_cnbz_64_imm(struct aarch64_emitter *emitter, struct aarch64_reg cmp, unsigned offset);

void aarch64_emit_ret(struct aarch64_emitter *emitter);

#endif
