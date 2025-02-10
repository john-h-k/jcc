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
void x64_emit_movsx(struct x64_emitter *emitter, struct x64_mov_reg movsx);

void x64_emit_add(struct x64_emitter *emitter, struct x64_alu_reg add);
void x64_emit_sub(struct x64_emitter *emitter, struct x64_alu_reg sub);
void x64_emit_eor(struct x64_emitter *emitter, struct x64_alu_reg eor);
void x64_emit_and(struct x64_emitter *emitter, struct x64_alu_reg and);
void x64_emit_or(struct x64_emitter *emitter, struct x64_alu_reg or);

void x64_emit_add_imm(struct x64_emitter *emitter, struct x64_alu_imm add_imm);
void x64_emit_sub_imm(struct x64_emitter *emitter, struct x64_alu_imm sub_imm);
void x64_emit_or_imm(struct x64_emitter *emitter, struct x64_alu_imm or_imm);
void x64_emit_eor_imm(struct x64_emitter *emitter, struct x64_alu_imm eor_imm);
void x64_emit_and_imm(struct x64_emitter *emitter, struct x64_alu_imm and_imm);

void x64_emit_not(struct x64_emitter *emitter, struct x64_1_reg not);
void x64_emit_neg(struct x64_emitter *emitter, struct x64_1_reg neg);

void x64_emit_shl(struct x64_emitter *emitter, struct x64_shift shl);
void x64_emit_shr(struct x64_emitter *emitter, struct x64_shift shr);
void x64_emit_sar(struct x64_emitter *emitter, struct x64_shift sar);

void x64_emit_mov_load_imm(struct x64_emitter *emitter, struct x64_mov_load_imm mov_load_imm);
void x64_emit_mov_store_imm(struct x64_emitter *emitter, struct x64_mov_store_imm mov_store_imm);

void x64_emit_push(struct x64_emitter *emitter, struct x64_push push);
void x64_emit_pop(struct x64_emitter *emitter, struct x64_pop pop);

void x64_emit_ret(struct x64_emitter *emitter);

#endif
