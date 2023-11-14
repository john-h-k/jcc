#ifndef AARCH64_EMITTER_H
#define AARCH64_EMITTER_H

#include <stdlib.h>

struct aarch64_emitter;

void create_aarch64_emitter(struct aarch64_emitter **emitter);
void free_aarch64_emitter(struct aarch64_emitter **emitter);

size_t aarch64_emit_bytesize(struct aarch64_emitter *emitter);
void aarch64_emit_copy_to(struct aarch64_emitter *emitter, void *dest);

void aarch64_emit_sub_32(struct aarch64_emitter *emitter, size_t reg_lhs,
                         size_t reg_rhs, size_t reg_to);
void aarch64_emit_add_32(struct aarch64_emitter *emitter, size_t reg_lhs,
                         size_t reg_rhs, size_t reg_to);
void aarch64_emit_mul_32(struct aarch64_emitter *emitter, size_t reg_lhs,
                         size_t reg_rhs, size_t reg_to);
void aarch64_emit_sdiv_32(struct aarch64_emitter *emitter, size_t reg_lhs,
                          size_t reg_rhs, size_t reg_to);
void aarch64_emit_udiv_32(struct aarch64_emitter *emitter, size_t reg_lhs,
                          size_t reg_rhs, size_t reg_to);

void aarch64_emit_load_cnst_32(struct aarch64_emitter *emitter, size_t reg_idx,
                               uint32_t cnst);
void aarch64_emit_load_cnst_64(struct aarch64_emitter *emitter, size_t reg_idx,
                               uint64_t cnst);
void aarch64_emit_mov_32(struct aarch64_emitter *emitter, size_t reg_from,
                         size_t reg_to);
void aarch64_emit_mov_64(struct aarch64_emitter *emitter, size_t reg_from,
                         size_t reg_to);
void aarch64_emit_ret(struct aarch64_emitter *emitter);

#endif
