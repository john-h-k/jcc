#include "emitter.h"

#include "../alloc.h"
#include "../bit_twiddle.h"
#include "../log.h"
#include "../util.h"
#include "codegen.h"
#include "isa.h"

#include <stdlib.h>

struct x64_emitter {
  unsigned char *block;
  size_t count;
  size_t len;
  size_t head;
};

#define BLOCK_SIZE 4096

// static void bad_instr(void) {
//   BUG("register types or arguments did not make sense");
// }

void create_x64_emitter(struct x64_emitter **emitter) {
  *emitter = nonnull_malloc(sizeof(**emitter));

  (*emitter)->len = BLOCK_SIZE;
  (*emitter)->block =
      nonnull_malloc((*emitter)->len * sizeof((*emitter)->block));
  (*emitter)->head = 0;
  (*emitter)->count = 0;
}

size_t x64_emit_bytesize(struct x64_emitter *emitter) {
  return emitter->head * sizeof(*emitter->block);
}

size_t x64_emitted_count(struct x64_emitter *emitter) { return emitter->count; }

void x64_emit_copy_to(struct x64_emitter *emitter, void *dest) {
  memcpy(dest, emitter->block, emitter->head * sizeof(*emitter->block));
}

void free_x64_emitter(struct x64_emitter **emitter) {
  free((*emitter)->block);

  free(*emitter);
  *emitter = NULL;
}

static void x64_emit_instr(struct x64_emitter *emitter,
                           struct x64_raw_instr instr) {
  if (emitter->head + instr.len > emitter->len) {
    size_t new_len = emitter->len + BLOCK_SIZE;
    emitter->block =
        nonnull_realloc(emitter->block, new_len * sizeof(emitter->block));
    emitter->len = new_len;
  }

  memcpy(&emitter->block[emitter->head], instr.buff, instr.len);
  emitter->head += instr.len;
  emitter->count += 1;
}

/* Nop */

void x64_emit_nop(struct x64_emitter *emitter) { x64_emit_instr(emitter, NOP); }

void x64_emit_add(struct x64_emitter *emitter, struct x64_alu_reg add) {
  x64_emit_instr(emitter, ADD_REG(add.dest, add.rhs));
}

void x64_emit_sub(struct x64_emitter *emitter, struct x64_alu_reg sub) {
  x64_emit_instr(emitter, SUB_REG(sub.dest, sub.rhs));
}

void x64_emit_div(struct x64_emitter *emitter, struct x64_div div) {
  x64_emit_instr(emitter, DIV_REG(div.rhs));
}

void x64_emit_idiv(struct x64_emitter *emitter, struct x64_div idiv) {
  x64_emit_instr(emitter, IDIV_REG(idiv.rhs));
}

void x64_emit_and(struct x64_emitter *emitter, struct x64_alu_reg and) {
  x64_emit_instr(emitter, AND_REG(and.dest, and.rhs));
}

void x64_emit_eor(struct x64_emitter *emitter, struct x64_alu_reg eor) {
  x64_emit_instr(emitter, EOR_REG(eor.dest, eor.rhs));
}

void x64_emit_or(struct x64_emitter *emitter, struct x64_alu_reg or) {
  x64_emit_instr(emitter, OR_REG(or.dest, or.rhs));
}

void x64_emit_not(struct x64_emitter *emitter, struct x64_1_reg not ) {
  x64_emit_instr(emitter, NOT_REG(not .dest));
}

void x64_emit_neg(struct x64_emitter *emitter, struct x64_1_reg neg) {
  x64_emit_instr(emitter, NEG_REG(neg.dest));
}

void x64_emit_shl(struct x64_emitter *emitter, struct x64_shift shl) {
  x64_emit_instr(emitter, SHL(shl.dest));
}

void x64_emit_shr(struct x64_emitter *emitter, struct x64_shift shr) {
  x64_emit_instr(emitter, SHR(shr.dest));
}

void x64_emit_sar(struct x64_emitter *emitter, struct x64_shift sar) {
  x64_emit_instr(emitter, SAR(sar.dest));
}

void x64_emit_add_imm(struct x64_emitter *emitter, struct x64_alu_imm add_imm) {
  x64_emit_instr(emitter, ADD_IMM(add_imm.dest, add_imm.imm));
}

void x64_emit_movsx(struct x64_emitter *emitter, struct x64_mov_reg movsx) {
  struct x64_reg dest = movsx.dest;

  DEBUG_ASSERT(dest.ty == X64_REG_TY_R || dest.ty == X64_REG_TY_RD || dest.ty == X64_REG_TY_E, "movsx dest must be 32 or 64 bit");

  switch (movsx.source.ty) {
  case X64_REG_TY_W:
    x64_emit_instr(emitter, dest.ty == X64_REG_TY_R ? MOVSX16_64(movsx.dest, movsx.source) : MOVSX16_32(movsx.dest, movsx.source));
    break;
  case X64_REG_TY_L:
    x64_emit_instr(emitter, dest.ty == X64_REG_TY_R ? MOVSX8_64(movsx.dest, movsx.source) : MOVSX8_32(movsx.dest, movsx.source));
    break;
  case X64_REG_TY_RD:
  case X64_REG_TY_E:
    x64_emit_instr(emitter, MOVSX32_64(movsx.dest, movsx.source));
    break;
  case X64_REG_TY_R:
    BUG("source of movsx should be 8/16/32 bit register");
  }
}

void x64_emit_sub_imm(struct x64_emitter *emitter, struct x64_alu_imm sub_imm) {
  x64_emit_instr(emitter, SUB_IMM(sub_imm.dest, sub_imm.imm));
}

void x64_emit_or_imm(struct x64_emitter *emitter, struct x64_alu_imm or_imm) {
  x64_emit_instr(emitter, OR_IMM(or_imm.dest, or_imm.imm));
}

void x64_emit_eor_imm(struct x64_emitter *emitter, struct x64_alu_imm eor_imm) {
  x64_emit_instr(emitter, EOR_IMM(eor_imm.dest, eor_imm.imm));
}

void x64_emit_and_imm(struct x64_emitter *emitter, struct x64_alu_imm and_imm) {
  x64_emit_instr(emitter, AND_IMM(and_imm.dest, and_imm.imm));
}

void x64_emit_mov_imm(struct x64_emitter *emitter, struct x64_mov_imm mov_imm) {
  x64_emit_instr(emitter, MOV_IMM(mov_imm.dest, mov_imm.imm));
}

void x64_emit_mov_reg(struct x64_emitter *emitter, struct x64_mov_reg mov_reg) {
  x64_emit_instr(emitter, MOV_REG(mov_reg.dest, mov_reg.source));
}

void x64_emit_mov_load_imm(struct x64_emitter *emitter,
                           struct x64_mov_load_imm mov_load_imm) {
  x64_emit_instr(emitter, MOV_LOAD_IMM(mov_load_imm.dest, mov_load_imm.addr,
                                       mov_load_imm.imm));
}

void x64_emit_mov_store_imm(struct x64_emitter *emitter,
                            struct x64_mov_store_imm mov_store_imm) {
  x64_emit_instr(emitter, MOV_STORE_IMM(mov_store_imm.source,
                                        mov_store_imm.addr, mov_store_imm.imm));
}

void x64_emit_push(struct x64_emitter *emitter, struct x64_push push) {
  x64_emit_instr(emitter, PUSH_REG64(push.source));
}

void x64_emit_pop(struct x64_emitter *emitter, struct x64_pop pop) {
  x64_emit_instr(emitter, POP_REG64(pop.dest));
}

void x64_emit_ret(struct x64_emitter *emitter) { x64_emit_instr(emitter, RET); }
