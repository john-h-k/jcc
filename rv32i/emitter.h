#ifndef RV32I_EMITTER_H
#define RV32I_EMITTER_H

#include "codegen.h"

#include <stdlib.h>
struct rv32i_emitter;

void create_rv32i_emitter(struct rv32i_emitter **emitter);
void free_rv32i_emitter(struct rv32i_emitter **emitter);

size_t rv32i_emitted_count(struct rv32i_emitter *emitter);
size_t rv32i_emit_bytesize(struct rv32i_emitter *emitter);
void rv32i_emit_copy_to(struct rv32i_emitter *emitter, void *dest);

/* Nop */

void rv32i_emit_lui(struct rv32i_emitter *emitter, const struct rv32i_lui lui);
void rv32i_emit_jalr(struct rv32i_emitter *emitter, const struct rv32i_jalr jalr);
void rv32i_emit_addi(struct rv32i_emitter *emitter, const struct rv32i_op_imm addi);

void rv32i_emit_add(struct rv32i_emitter *emitter, const struct rv32i_op add);
void rv32i_emit_sub(struct rv32i_emitter *emitter, const struct rv32i_op sub);
void rv32i_emit_mul(struct rv32i_emitter *emitter, const struct rv32i_op mul);
void rv32i_emit_div(struct rv32i_emitter *emitter, const struct rv32i_op div);
void rv32i_emit_rem(struct rv32i_emitter *emitter, const struct rv32i_op rem);
void rv32i_emit_divu(struct rv32i_emitter *emitter, const struct rv32i_op divu);
void rv32i_emit_remu(struct rv32i_emitter *emitter, const struct rv32i_op remu);

void rv32i_emit_sb(struct rv32i_emitter *emitter, const struct rv32i_store sb);
void rv32i_emit_sh(struct rv32i_emitter *emitter, const struct rv32i_store sh);
void rv32i_emit_sw(struct rv32i_emitter *emitter, const struct rv32i_store sw);

void rv32i_emit_lb(struct rv32i_emitter *emitter, const struct rv32i_load lb);
void rv32i_emit_lbu(struct rv32i_emitter *emitter, const struct rv32i_load lbu);
void rv32i_emit_lh(struct rv32i_emitter *emitter, const struct rv32i_load lh);
void rv32i_emit_lhu(struct rv32i_emitter *emitter, const struct rv32i_load lhu);
void rv32i_emit_lw(struct rv32i_emitter *emitter, const struct rv32i_load lw);

#endif
