#include "emitter.h"

#include "../alloc.h"
#include "../bit_twiddle.h"
#include "../log.h"
#include "../util.h"
#include "codegen.h"
#include "isa.h"

#include <stdlib.h>

struct rv32i_emitter {
  uint32_t *block;
  size_t len;
  size_t head;
};

#define BLOCK_SIZE 4096

// static void bad_instr(void) {
//   bug("register types or arguments did not make sense");
// }

void create_rv32i_emitter(struct rv32i_emitter **emitter) {
  *emitter = nonnull_malloc(sizeof(**emitter));

  (*emitter)->len = BLOCK_SIZE;
  (*emitter)->block =
      nonnull_malloc((*emitter)->len * sizeof((*emitter)->block));
  (*emitter)->head = 0;
}

size_t rv32i_emit_bytesize(struct rv32i_emitter *emitter) {
  return emitter->head * sizeof(*emitter->block);
}

size_t rv32i_emitted_count(struct rv32i_emitter *emitter) {
  return emitter->head;
}

void rv32i_emit_copy_to(struct rv32i_emitter *emitter, void *dest) {
  memcpy(dest, emitter->block, emitter->head * sizeof(*emitter->block));
}

void free_rv32i_emitter(struct rv32i_emitter **emitter) {
  free((*emitter)->block);

  free(*emitter);
  *emitter = NULL;
}

static void rv32i_emit_instr(struct rv32i_emitter *emitter,
                               uint32_t instr) {
  if (emitter->head >= emitter->len) {
    size_t new_len = emitter->len + BLOCK_SIZE;
    emitter->block =
        nonnull_realloc(emitter->block, new_len * sizeof(emitter->block));
    emitter->len = new_len;
  }

  emitter->block[emitter->head++] = instr;
}

void rv32i_emit_lui(struct rv32i_emitter *emitter, const struct rv32i_lui lui) {
  rv32i_emit_instr(emitter, LUI(lui.imm, lui.dest.idx));
}

void rv32i_emit_jalr(struct rv32i_emitter *emitter, const struct rv32i_jalr jalr) {
  rv32i_emit_instr(emitter, JALR(jalr.imm, jalr.target.idx, jalr.ret_addr.idx));
}

void rv32i_emit_addi(struct rv32i_emitter *emitter, const struct rv32i_op_imm addi) {
  rv32i_emit_instr(emitter, ADDI(addi.imm, addi.source.idx, addi.dest.idx));
}

void rv32i_emit_add(struct rv32i_emitter *emitter, const struct rv32i_op add) {
  rv32i_emit_instr(emitter, ADD(add.rhs.idx, add.lhs.idx, add.dest.idx));
}

void rv32i_emit_sub(struct rv32i_emitter *emitter, const struct rv32i_op sub) {
  rv32i_emit_instr(emitter, SUB(sub.rhs.idx, sub.lhs.idx, sub.dest.idx));
}
void rv32i_emit_mul(struct rv32i_emitter *emitter, const struct rv32i_op mul) {
  rv32i_emit_instr(emitter, MUL(mul.rhs.idx, mul.lhs.idx, mul.dest.idx));
}
void rv32i_emit_div(struct rv32i_emitter *emitter, const struct rv32i_op div) {
  rv32i_emit_instr(emitter, DIV(div.rhs.idx, div.lhs.idx, div.dest.idx));
}
void rv32i_emit_rem(struct rv32i_emitter *emitter, const struct rv32i_op rem) {
  rv32i_emit_instr(emitter, REM(rem.rhs.idx, rem.lhs.idx, rem.dest.idx));
}
void rv32i_emit_divu(struct rv32i_emitter *emitter, const struct rv32i_op divu) {
  rv32i_emit_instr(emitter, DIVU(divu.rhs.idx, divu.lhs.idx, divu.dest.idx));
}
void rv32i_emit_remu(struct rv32i_emitter *emitter, const struct rv32i_op remu) {
  rv32i_emit_instr(emitter, REMU(remu.rhs.idx, remu.lhs.idx, remu.dest.idx));
}


void rv32i_emit_sb(struct rv32i_emitter *emitter, const struct rv32i_store sb) {
  rv32i_emit_instr(emitter, SB(sb.imm, sb.source.idx, sb.addr.idx));
}

void rv32i_emit_sh(struct rv32i_emitter *emitter, const struct rv32i_store sh) {
  rv32i_emit_instr(emitter, SH(sh.imm, sh.source.idx, sh.addr.idx));
}

void rv32i_emit_sw(struct rv32i_emitter *emitter, const struct rv32i_store sw) {
  rv32i_emit_instr(emitter, SW(sw.imm, sw.source.idx, sw.addr.idx));
}

void rv32i_emit_lb(struct rv32i_emitter *emitter, const struct rv32i_load lb) {
  rv32i_emit_instr(emitter, LB(lb.imm, lb.addr.idx, lb.dest.idx));
}

void rv32i_emit_lbu(struct rv32i_emitter *emitter, const struct rv32i_load lbu) {
  rv32i_emit_instr(emitter, LBU(lbu.imm, lbu.addr.idx, lbu.dest.idx));
}

void rv32i_emit_lh(struct rv32i_emitter *emitter, const struct rv32i_load lh) {
  rv32i_emit_instr(emitter, LH(lh.imm, lh.addr.idx, lh.dest.idx));
}

void rv32i_emit_lhu(struct rv32i_emitter *emitter, const struct rv32i_load lhu) {
  rv32i_emit_instr(emitter, LHU(lhu.imm, lhu.addr.idx, lhu.dest.idx));
}

void rv32i_emit_lw(struct rv32i_emitter *emitter, const struct rv32i_load lw) {
  rv32i_emit_instr(emitter, LW(lw.imm, lw.addr.idx, lw.dest.idx));
}

