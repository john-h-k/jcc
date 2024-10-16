#ifndef EEP_EMITTER_H
#define EEP_EMITTER_H

#include <stdlib.h>
#include "codegen.h"

#if defined(RETURN_REG) || defined(STACK_PTR_REG)
#error "RETURN_REG/STACK_PTR_REG already defined. Check your includes"
#endif

#define RETURN_REG ((struct eep_reg){7})
#define STACK_PTR_REG ((struct eep_reg){6})

struct eep_emitter;

void create_eep_emitter(struct eep_emitter **emitter);
void free_eep_emitter(struct eep_emitter **emitter);

size_t eep_emitted_count(struct eep_emitter *emitter);
size_t eep_emit_bytesize(struct eep_emitter *emitter);
void eep_emit_copy_to(struct eep_emitter *emitter, void *dest);

/* Nop */
void eep_emit_nop(struct eep_emitter *emitter);

void eep_emit_mov(struct eep_emitter *emitter, const struct eep_mov mov);
void eep_emit_mov_imm(struct eep_emitter *emitter, const struct eep_mov_imm mov_imm);

void eep_emit_add(struct eep_emitter *emitter, const struct eep_alu alu);
void eep_emit_sub(struct eep_emitter *emitter, const struct eep_alu alu);
void eep_emit_adc(struct eep_emitter *emitter, const struct eep_alu alu);
void eep_emit_sbc(struct eep_emitter *emitter, const struct eep_alu alu);
void eep_emit_and(struct eep_emitter *emitter, const struct eep_alu alu);
void eep_emit_cmp(struct eep_emitter *emitter, const struct eep_alu alu);

void eep_emit_add_imm(struct eep_emitter *emitter, const struct eep_alu_imm imm);
void eep_emit_sub_imm(struct eep_emitter *emitter, const struct eep_alu_imm imm);
void eep_emit_adc_imm(struct eep_emitter *emitter, const struct eep_alu_imm imm);
void eep_emit_sbc_imm(struct eep_emitter *emitter, const struct eep_alu_imm imm);
void eep_emit_and_imm(struct eep_emitter *emitter, const struct eep_alu_imm imm);
void eep_emit_cmp_imm(struct eep_emitter *emitter, const struct eep_alu_imm imm);

void eep_emit_lsl(struct eep_emitter *emitter, const struct eep_alu_shift shift);
void eep_emit_lsr(struct eep_emitter *emitter, const struct eep_alu_shift shift);
void eep_emit_asr(struct eep_emitter *emitter, const struct eep_alu_shift shift);
void eep_emit_xsr(struct eep_emitter *emitter, const struct eep_alu_shift shift);

void eep_emit_load_direct(struct eep_emitter *emitter, const struct eep_ldr_direct ldr);
void eep_emit_store_direct(struct eep_emitter *emitter, const struct eep_str_direct str);

void eep_emit_load_offset(struct eep_emitter *emitter, const struct eep_ldr_offset ldr);
void eep_emit_store_offset(struct eep_emitter *emitter, const struct eep_str_offset str);

void eep_emit_jump(struct eep_emitter *emitter, const struct eep_jmp jmp);
void eep_emit_ext(struct eep_emitter *emitter, const struct eep_ext ext);

#endif
