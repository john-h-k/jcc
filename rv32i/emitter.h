#ifndef rv32i_EMITTER_H
#define rv32i_EMITTER_H

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
void rv32i_emit_addi(struct rv32i_emitter *emitter, const struct rv32i_addsub_imm addi);
void rv32i_emit_add(struct rv32i_emitter *emitter, const struct rv32i_addsub_reg add);

#endif
