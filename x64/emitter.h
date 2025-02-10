#ifndef X64_EMITTER_H
#define X64_EMITTER_H

#include "codegen.h"

struct x64_emitter;

void create_x64_emitter(struct x64_emitter **emitter);
void free_x64_emitter(struct x64_emitter **emitter);

size_t x64_emitted_count(struct x64_emitter *emitter);
size_t x64_emit_bytesize(struct x64_emitter *emitter);
void x64_emit_copy_to(struct x64_emitter *emitter, void *dest);

/* Nop */

void x64_emit_nop(struct x64_emitter *emitter);

/* Move immediate */

void x64_emit_mov_imm(struct x64_emitter *emitter, struct x64_mov_imm mov_imm);
void x64_emit_mov_reg(struct x64_emitter *emitter, struct x64_mov_reg mov_reg);
void x64_emit_add(struct x64_emitter *emitter, struct x64_alu_reg alu_reg);
void x64_emit_ret(struct x64_emitter *emitter);

#endif
