#ifndef EEP_EMITTER_H
#define EEP_EMITTER_H

#include <stdlib.h>

struct eep_reg {
  size_t idx;
};

#if defined(RETURN_REG) || defined(STACK_PTR_REG)
#error "RETURN_REG/STACK_PTR_REG already defined. Check your includes"
#endif

#define RETURN_REG ((struct eep_reg) {7})
#define STACK_PTR_REG ((struct eep_reg) {6})

enum eep_cond {
  EEP_COND_JMP = 0b0000,
  EEP_COND_NOOP = 0b0001,

  EEP_COND_JEQ = 0b0010,
  EEP_COND_JNE = 0b0011,

  EEP_COND_JCS = 0b0100,
  EEP_COND_JCC = 0b0101,

  EEP_COND_JMI = 0b0110,
  EEP_COND_JPL = 0b0111,

  EEP_COND_JGE = 0b1000,
  EEP_COND_JLT = 0b1001,

  EEP_COND_JGT = 0b1010,
  EEP_COND_JLE = 0b1011,

  EEP_COND_JHI = 0b1100,
  EEP_COND_JLS = 0b1101,

  EEP_COND_JSR = 0b1110,
  EEP_COND_RET = 0b1111,
};

struct eep_emitter;

void create_eep_emitter(struct eep_emitter **emitter);
void free_eep_emitter(struct eep_emitter **emitter);

size_t eep_emitted_count(struct eep_emitter *emitter);
size_t eep_emit_bytesize(struct eep_emitter *emitter);
void eep_emit_copy_to(struct eep_emitter *emitter, void *dest);

/* Emits a `nop` and returns the address such that it can be modified later */
uint16_t *eep_emit_reserved(struct eep_emitter *emitter);

/* Nop */
void eep_emit_nop(struct eep_emitter *emitter);

void eep_emit_mov(struct eep_emitter *emitter, struct eep_reg source, struct eep_reg dest);
void eep_emit_mov_imm(struct eep_emitter *emitter, int imm, struct eep_reg dest);

void eep_emit_add(struct eep_emitter *emitter, struct eep_reg lhs, struct eep_reg rhs, struct eep_reg dest);
void eep_emit_sub(struct eep_emitter *emitter, struct eep_reg lhs, struct eep_reg rhs, struct eep_reg dest);
void eep_emit_adc(struct eep_emitter *emitter, struct eep_reg lhs, struct eep_reg rhs, struct eep_reg dest);
void eep_emit_sbc(struct eep_emitter *emitter, struct eep_reg lhs, struct eep_reg rhs, struct eep_reg dest);
void eep_emit_and(struct eep_emitter *emitter, struct eep_reg lhs, struct eep_reg rhs, struct eep_reg dest);

void eep_emit_cmp(struct eep_emitter *emitter, struct eep_reg lhs, struct eep_reg rhs);

void eep_emit_add_imm(struct eep_emitter *emitter, struct eep_reg dest, int imm);
void eep_emit_sub_imm(struct eep_emitter *emitter, struct eep_reg dest, int imm);
void eep_emit_adc_imm(struct eep_emitter *emitter, struct eep_reg dest, int imm);
void eep_emit_sbc_imm(struct eep_emitter *emitter, struct eep_reg dest, int imm);
void eep_emit_and_imm(struct eep_emitter *emitter, struct eep_reg dest, int imm);

void eep_emit_cmp_imm(struct eep_emitter *emitter, struct eep_reg lhs, int imm);

void eep_emit_lsl(struct eep_emitter *emitter, struct eep_reg source, struct eep_reg dest, int scnt);
void eep_emit_lsr(struct eep_emitter *emitter, struct eep_reg source, struct eep_reg dest, int scnt);
void eep_emit_asr(struct eep_emitter *emitter, struct eep_reg source, struct eep_reg dest, int scnt);
void eep_emit_xsr(struct eep_emitter *emitter, struct eep_reg source, struct eep_reg dest, int scnt);

void eep_emit_load_direct(struct eep_emitter *emitter, int source, struct eep_reg dest);
void eep_emit_store_direct(struct eep_emitter *emitter, struct eep_reg source, int dest);

void eep_emit_load_offset(struct eep_emitter *emitter, struct eep_reg source, int source_offset, struct eep_reg dest);
void eep_emit_store_offset(struct eep_emitter *emitter, struct eep_reg source, struct eep_reg dest, int dest_offset);

void eep_emit_jump(struct eep_emitter *emitter, enum eep_cond cond, signed char offset);
void eep_emit_ext(struct eep_emitter *emitter, int imm);



#endif
