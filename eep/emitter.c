#include "emitter.h"

#include "../alloc.h"
#include "../bit_twiddle.h"
#include "../log.h"
#include "../util.h"
#include "isa.h"

#include <stdlib.h>

#define NOP JUMP(EEP_COND_NOOP, 0)

// FIXME: this is all copied from the aarch64 emitter - we should create a
// general fixed-width emitter to use for both

struct eep_emitter {
  uint16_t *block;
  size_t len;
  size_t head;
};

#define BLOCK_SIZE 4096

void create_eep_emitter(struct eep_emitter **emitter) {
  *emitter = nonnull_malloc(sizeof(**emitter));

  (*emitter)->block = nonnull_malloc(BLOCK_SIZE);
  (*emitter)->len = BLOCK_SIZE;
  (*emitter)->head = 0;
}

size_t eep_emit_bytesize(struct eep_emitter *emitter) {
  return emitter->head * sizeof(*emitter->block);
}

size_t eep_emitted_count(struct eep_emitter *emitter) { return emitter->head; }

void eep_emit_copy_to(struct eep_emitter *emitter, void *dest) {
  memcpy(dest, emitter->block, emitter->head * sizeof(*emitter->block));
}

void free_eep_emitter(struct eep_emitter **emitter) {
  free((*emitter)->block);

  free(*emitter);
  *emitter = NULL;
}

void eep_emit(struct eep_emitter *emitter, uint16_t instr) {
  if (emitter->head >= emitter->len) {
    todo("emitter reached size limit");
  }

  emitter->block[emitter->head++] = instr;
}

/* Nop */
void eep_emit_nop(struct eep_emitter *emitter) { eep_emit(emitter, NOP); }

/* Emits a `nop` and returns the address such that it can be modified later */
uint16_t *eep_emit_reserved(struct eep_emitter *emitter) {
  uint16_t *instr = &emitter->block[emitter->head];
  eep_emit_nop(emitter);
  return instr;
}

void eep_emit_mov(struct eep_emitter *emitter, struct eep_reg source,
                  struct eep_reg dest) {
  eep_emit(emitter, MOV(dest.idx, source.idx));
}

void eep_emit_mov_imm(struct eep_emitter *emitter, int imm,
                      struct eep_reg dest) {
  eep_emit(emitter, MOV_IMM(dest.idx, imm));
}

void eep_emit_add(struct eep_emitter *emitter, struct eep_reg lhs,
                  struct eep_reg rhs, struct eep_reg dest) {
  eep_emit(emitter, ADD(dest.idx, lhs.idx, rhs.idx));
}
void eep_emit_sub(struct eep_emitter *emitter, struct eep_reg lhs,
                  struct eep_reg rhs, struct eep_reg dest) {
  eep_emit(emitter, SUB(dest.idx, lhs.idx, rhs.idx));
}
void eep_emit_adc(struct eep_emitter *emitter, struct eep_reg lhs,
                  struct eep_reg rhs, struct eep_reg dest) {
  eep_emit(emitter, ADC(dest.idx, lhs.idx, rhs.idx));
}
void eep_emit_sbc(struct eep_emitter *emitter, struct eep_reg lhs,
                  struct eep_reg rhs, struct eep_reg dest) {
  eep_emit(emitter, SBC(dest.idx, lhs.idx, rhs.idx));
}
void eep_emit_and(struct eep_emitter *emitter, struct eep_reg lhs,
                  struct eep_reg rhs, struct eep_reg dest) {
  eep_emit(emitter, AND(dest.idx, lhs.idx, rhs.idx));
}

void eep_emit_cmp(struct eep_emitter *emitter, struct eep_reg lhs,
                  struct eep_reg rhs) {
  eep_emit(emitter, CMP(lhs.idx, rhs.idx));
}

void eep_emit_add_imm(struct eep_emitter *emitter, struct eep_reg dest,
                      int imm) {
  eep_emit(emitter, ADD_IMM(dest.idx, imm));
}
void eep_emit_sub_imm(struct eep_emitter *emitter, struct eep_reg dest,
                      int imm) {
  eep_emit(emitter, SUB_IMM(dest.idx, imm));
}
void eep_emit_adc_imm(struct eep_emitter *emitter, struct eep_reg dest,
                      int imm) {
  eep_emit(emitter, ADC_IMM(dest.idx, imm));
}
void eep_emit_sbc_imm(struct eep_emitter *emitter, struct eep_reg dest,
                      int imm) {
  eep_emit(emitter, SBC_IMM(dest.idx, imm));
}
void eep_emit_and_imm(struct eep_emitter *emitter, struct eep_reg dest,
                      int imm) {
  eep_emit(emitter, AND_IMM(dest.idx, imm));
}

void eep_emit_cmp_imm(struct eep_emitter *emitter, struct eep_reg lhs,
                      int imm) {
  eep_emit(emitter, CMP_IMM(lhs.idx, imm));
}

void eep_emit_lsl(struct eep_emitter *emitter, struct eep_reg source,
                  struct eep_reg dest, int scnt) {
  eep_emit(emitter, LSL(dest.idx, source.idx, scnt));
}
void eep_emit_lsr(struct eep_emitter *emitter, struct eep_reg source,
                  struct eep_reg dest, int scnt) {
  eep_emit(emitter, LSR(dest.idx, source.idx, scnt));
}
void eep_emit_asr(struct eep_emitter *emitter, struct eep_reg source,
                  struct eep_reg dest, int scnt) {
  eep_emit(emitter, ASR(dest.idx, source.idx, scnt));
}
void eep_emit_xsr(struct eep_emitter *emitter, struct eep_reg source,
                  struct eep_reg dest, int scnt) {
  eep_emit(emitter, XSR(dest.idx, source.idx, scnt));
}

void eep_emit_load_direct(struct eep_emitter *emitter, int source,
                          struct eep_reg dest) {
  eep_emit(emitter, LDR_DIRECT(dest.idx, source));
}

void eep_emit_store_direct(struct eep_emitter *emitter, struct eep_reg source,
                           int dest) {
  eep_emit(emitter, STR_DIRECT(dest, source.idx));
}

void eep_emit_load_offset(struct eep_emitter *emitter, struct eep_reg source,
                          int source_offset, struct eep_reg dest) {
  eep_emit(emitter, LDR_OFFSET(dest.idx, source.idx, source_offset));
}

void eep_emit_store_offset(struct eep_emitter *emitter, struct eep_reg source,
                           struct eep_reg dest, int dest_offset) {
  eep_emit(emitter, STR_OFFSET(source.idx, dest.idx, dest_offset));
}

void eep_emit_jump(struct eep_emitter *emitter, enum eep_cond cond,
                   signed char offset) {
  int jumpopc = (int)cond;
  eep_emit(emitter, JUMP(jumpopc, offset));
}

void eep_emit_ext(struct eep_emitter *emitter, int imm) {
  eep_emit(emitter, EXT(imm));
}
