#if 0

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

  (*emitter)->len = BLOCK_SIZE;
  (*emitter)->block = nonnull_malloc((*emitter)->len * BLOCK_SIZE);
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

void eep_emit_instr(struct eep_emitter *emitter, uint16_t instr) {
  if (emitter->head >= emitter->len) {
    todo("emitter reached size limit");
  }

  emitter->block[emitter->head++] = instr;
}

/* Nop */
void eep_emit_nop(struct eep_emitter *emitter) { eep_emit_instr(emitter, NOP); }

void eep_emit_mov(struct eep_emitter *emitter, const struct eep_mov mov) {
  eep_emit_instr(emitter, MOV(mov.dest.idx, mov.source.idx));
}
void eep_emit_mov_imm(struct eep_emitter *emitter,
                      const struct eep_mov_imm mov_imm) {
  eep_emit_instr(emitter, MOV_IMM(mov_imm.dest.idx, mov_imm.imm));
}

void eep_emit_add(struct eep_emitter *emitter, const struct eep_alu alu) {
  eep_emit_instr(emitter, ADD(alu.dest.idx, alu.lhs.idx, alu.rhs.idx));
}
void eep_emit_sub(struct eep_emitter *emitter, const struct eep_alu alu) {
  eep_emit_instr(emitter, SUB(alu.dest.idx, alu.lhs.idx, alu.rhs.idx));
}
void eep_emit_adc(struct eep_emitter *emitter, const struct eep_alu alu) {
  eep_emit_instr(emitter, ADC(alu.dest.idx, alu.lhs.idx, alu.rhs.idx));
}
void eep_emit_sbc(struct eep_emitter *emitter, const struct eep_alu alu) {
  eep_emit_instr(emitter, SBC(alu.dest.idx, alu.lhs.idx, alu.rhs.idx));
}
void eep_emit_and(struct eep_emitter *emitter, const struct eep_alu alu) {
  eep_emit_instr(emitter, AND(alu.dest.idx, alu.lhs.idx, alu.rhs.idx));
}
void eep_emit_cmp(struct eep_emitter *emitter, const struct eep_alu alu) {
  eep_emit_instr(emitter, CMP(alu.lhs.idx, alu.rhs.idx));
}

void eep_emit_add_imm(struct eep_emitter *emitter,
                      const struct eep_alu_imm imm) {
  eep_emit_instr(emitter, ADD_IMM(imm.dest.idx, imm.imm));
}
void eep_emit_sub_imm(struct eep_emitter *emitter,
                      const struct eep_alu_imm imm) {
  eep_emit_instr(emitter, SUB_IMM(imm.dest.idx, imm.imm));
}
void eep_emit_adc_imm(struct eep_emitter *emitter,
                      const struct eep_alu_imm imm) {
  eep_emit_instr(emitter, ADC_IMM(imm.dest.idx, imm.imm));
}
void eep_emit_sbc_imm(struct eep_emitter *emitter,
                      const struct eep_alu_imm imm) {
  eep_emit_instr(emitter, SBC_IMM(imm.dest.idx, imm.imm));
}
void eep_emit_and_imm(struct eep_emitter *emitter,
                      const struct eep_alu_imm imm) {
  eep_emit_instr(emitter, AND_IMM(imm.dest.idx, imm.imm));
}
void eep_emit_cmp_imm(struct eep_emitter *emitter,
                      const struct eep_alu_imm imm) {
  eep_emit_instr(emitter, CMP_IMM(imm.dest.idx, imm.imm));
}

void eep_emit_lsl(struct eep_emitter *emitter,
                  const struct eep_alu_shift shift) {
  eep_emit_instr(emitter, LSL(shift.dest.idx, shift.source.idx, shift.imm));
}
void eep_emit_lsr(struct eep_emitter *emitter,
                  const struct eep_alu_shift shift) {
  eep_emit_instr(emitter, LSR(shift.dest.idx, shift.source.idx, shift.imm));
}
void eep_emit_asr(struct eep_emitter *emitter,
                  const struct eep_alu_shift shift) {
  eep_emit_instr(emitter, ASR(shift.dest.idx, shift.source.idx, shift.imm));
}
void eep_emit_xsr(struct eep_emitter *emitter,
                  const struct eep_alu_shift shift) {
  eep_emit_instr(emitter, XSR(shift.dest.idx, shift.source.idx, shift.imm));
}

void eep_emit_load_direct(struct eep_emitter *emitter,
                          const struct eep_ldr_direct ldr) {
  eep_emit_instr(emitter, LDR_DIRECT(ldr.dest.idx, ldr.imm));
}
void eep_emit_store_direct(struct eep_emitter *emitter,
                           const struct eep_str_direct str) {
  eep_emit_instr(emitter, STR_DIRECT(str.source.idx, str.imm));
}

void eep_emit_load_offset(struct eep_emitter *emitter,
                          const struct eep_ldr_offset ldr) {
  eep_emit_instr(emitter, LDR_OFFSET(ldr.dest.idx, ldr.addr.idx, ldr.imm));
}
void eep_emit_store_offset(struct eep_emitter *emitter,
                           const struct eep_str_offset str) {
  eep_emit_instr(emitter, STR_OFFSET(str.source.idx, str.addr.idx, str.imm));
}

void eep_emit_jump(struct eep_emitter *emitter, const struct eep_jmp jmp) {
  eep_emit_instr(emitter, JUMP(jmp.cond, jmp.imm));
}
void eep_emit_ext(struct eep_emitter *emitter, const struct eep_ext ext) {
  eep_emit_instr(emitter, EXT(ext.imm));
}

#endif
