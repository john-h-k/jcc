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

static void rv32i_emit_instr(struct rv32i_emitter *emitter, uint32_t instr) {
  if (emitter->head >= emitter->len) {
    size_t new_len = emitter->len + BLOCK_SIZE;
    emitter->block =
        nonnull_realloc(emitter->block, new_len * sizeof(emitter->block));
    emitter->len = new_len;
  }

  emitter->block[emitter->head++] = instr;
}

void rv32i_emit_ori(struct rv32i_emitter *emitter, const struct rv32i_op_imm ori) {
  rv32i_emit_instr(emitter, ORI(ori.imm, ori.source.idx, ori.dest.idx));
}

void rv32i_emit_andi(struct rv32i_emitter *emitter, const struct rv32i_op_imm andi) {
  rv32i_emit_instr(emitter, ANDI(andi.imm, andi.source.idx, andi.dest.idx));
}

void rv32i_emit_slli(struct rv32i_emitter *emitter, const struct rv32i_op_imm slli) {
  rv32i_emit_instr(emitter, SLLI(slli.imm, slli.source.idx, slli.dest.idx));
}

void rv32i_emit_srli(struct rv32i_emitter *emitter, const struct rv32i_op_imm srli) {
  rv32i_emit_instr(emitter, SRLI(srli.imm, srli.source.idx, srli.dest.idx));
}

void rv32i_emit_srai(struct rv32i_emitter *emitter, const struct rv32i_op_imm srai) {
  rv32i_emit_instr(emitter, SRAI(srai.imm, srai.source.idx, srai.dest.idx));
}

void rv32i_emit_lui(struct rv32i_emitter *emitter, const struct rv32i_u lui) {
  rv32i_emit_instr(emitter, LUI(lui.imm, lui.dest.idx));
}

void rv32i_emit_auipc(struct rv32i_emitter *emitter, const struct rv32i_u auipc) {
  rv32i_emit_instr(emitter, AUIPC(auipc.imm, auipc.dest.idx));
}

void rv32i_emit_jalr(struct rv32i_emitter *emitter,
                     const struct rv32i_jalr jalr) {
  rv32i_emit_instr(emitter, JALR(jalr.imm, jalr.target.idx, jalr.ret_addr.idx));
}

#define OFFSET(instr)                                                          \
  signed long long cur_pos = rv32i_emitted_count(emitter);                     \
  signed long long target_pos = instr.target->first_instr->id;                 \
                                                                               \
  offset = (target_pos - cur_pos) * 4;

void rv32i_emit_jal(struct rv32i_emitter *emitter, const struct rv32i_jal jal) {
  simm_t offset = 0;
  if (jal.target) {
    OFFSET(jal);
  }

  rv32i_emit_instr(emitter, JAL(offset, jal.ret_addr.idx));
}

void rv32i_emit_beq(struct rv32i_emitter *emitter,
                    const struct rv32i_conditional_branch beq) {
  simm_t offset;
  OFFSET(beq);

  rv32i_emit_instr(emitter, BEQ(offset, beq.rhs.idx, beq.lhs.idx));
}

void rv32i_emit_bne(struct rv32i_emitter *emitter,
                    const struct rv32i_conditional_branch bne) {
  simm_t offset;
  OFFSET(bne);

  rv32i_emit_instr(emitter, BNE(offset, bne.rhs.idx, bne.lhs.idx));
}

void rv32i_emit_blt(struct rv32i_emitter *emitter,
                    const struct rv32i_conditional_branch blt) {
  simm_t offset;
  OFFSET(blt);

  rv32i_emit_instr(emitter, BLT(offset, blt.rhs.idx, blt.lhs.idx));
}

void rv32i_emit_bge(struct rv32i_emitter *emitter,
                    const struct rv32i_conditional_branch bge) {
  simm_t offset;
  OFFSET(bge);

  rv32i_emit_instr(emitter, BGE(offset, bge.rhs.idx, bge.lhs.idx));
}

void rv32i_emit_bltu(struct rv32i_emitter *emitter,
                     const struct rv32i_conditional_branch bltu) {
  simm_t offset;
  OFFSET(bltu);

  rv32i_emit_instr(emitter, BLTU(offset, bltu.rhs.idx, bltu.lhs.idx));
}

void rv32i_emit_bgeu(struct rv32i_emitter *emitter,
                     const struct rv32i_conditional_branch bgeu) {
  simm_t offset;
  OFFSET(bgeu);

  rv32i_emit_instr(emitter, BGEU(offset, bgeu.rhs.idx, bgeu.lhs.idx));
}

void rv32i_emit_fmv(struct rv32i_emitter *emitter,
                    const struct rv32i_op_mov fmv) {
  switch (fmv.source.ty) {
  case RV32I_REG_TY_NONE:
    BUG("NONE type in fmv emit");
  case RV32I_REG_TY_GP:
    rv32i_emit_instr(emitter, FMV_XW(fmv.source.idx, fmv.dest.idx));
    break;
  case RV32I_REG_TY_FP:
    rv32i_emit_instr(emitter, FMV_WX(fmv.source.idx, fmv.dest.idx));
    break;
  }
}

void rv32i_emit_fadd(struct rv32i_emitter *emitter,
                     const struct rv32i_op_fp fadd) {
  rv32i_emit_instr(emitter, FADD_S(fadd.rhs.idx, fadd.lhs.idx, fadd.dest.idx));
}

void rv32i_emit_fsub(struct rv32i_emitter *emitter,
                     const struct rv32i_op_fp fsub) {
  rv32i_emit_instr(emitter, FSUB_S(fsub.rhs.idx, fsub.lhs.idx, fsub.dest.idx));
}

void rv32i_emit_fmul(struct rv32i_emitter *emitter,
                     const struct rv32i_op_fp fmul) {
  rv32i_emit_instr(emitter, FMUL_S(fmul.rhs.idx, fmul.lhs.idx, fmul.dest.idx));
}

void rv32i_emit_fdiv(struct rv32i_emitter *emitter,
                     const struct rv32i_op_fp fdiv) {
  rv32i_emit_instr(emitter, FDIV_S(fdiv.rhs.idx, fdiv.lhs.idx, fdiv.dest.idx));
}

void rv32i_emit_fsgnj(struct rv32i_emitter *emitter,
                      const struct rv32i_op_fp fsgnj) {
  rv32i_emit_instr(emitter,
                   FSGNJ_S(fsgnj.rhs.idx, fsgnj.lhs.idx, fsgnj.dest.idx));
}

void rv32i_emit_fsgnjn(struct rv32i_emitter *emitter,
                       const struct rv32i_op_fp fsgnjn) {
  rv32i_emit_instr(emitter,
                   FSGNJN_S(fsgnjn.rhs.idx, fsgnjn.lhs.idx, fsgnjn.dest.idx));
}

void rv32i_emit_fsgnjx(struct rv32i_emitter *emitter,
                       const struct rv32i_op_fp fsgnjx) {
  rv32i_emit_instr(emitter,
                   FSGNJX_S(fsgnjx.rhs.idx, fsgnjx.lhs.idx, fsgnjx.dest.idx));
}

void rv32i_emit_fmax(struct rv32i_emitter *emitter,
                     const struct rv32i_op_fp fmax) {
  rv32i_emit_instr(emitter, FMAX_S(fmax.rhs.idx, fmax.lhs.idx, fmax.dest.idx));
}

void rv32i_emit_fmin(struct rv32i_emitter *emitter,
                     const struct rv32i_op_fp fmin) {
  rv32i_emit_instr(emitter, FMIN_S(fmin.rhs.idx, fmin.lhs.idx, fmin.dest.idx));
}

void rv32i_emit_fsqrt(struct rv32i_emitter *emitter,
                      const struct rv32i_op_unary_fp fsqrt) {
  rv32i_emit_instr(emitter, FSQRT_S(fsqrt.source.idx, fsqrt.dest.idx));
}

void rv32i_emit_addi(struct rv32i_emitter *emitter,
                     const struct rv32i_op_imm addi) {
  rv32i_emit_instr(emitter, ADDI(addi.imm, addi.source.idx, addi.dest.idx));
}

void rv32i_emit_xori(struct rv32i_emitter *emitter,
                     const struct rv32i_op_imm xori) {
  rv32i_emit_instr(emitter, XORI(xori.imm, xori.source.idx, xori.dest.idx));
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
void rv32i_emit_divu(struct rv32i_emitter *emitter,
                     const struct rv32i_op divu) {
  rv32i_emit_instr(emitter, DIVU(divu.rhs.idx, divu.lhs.idx, divu.dest.idx));
}
void rv32i_emit_remu(struct rv32i_emitter *emitter,
                     const struct rv32i_op remu) {
  rv32i_emit_instr(emitter, REMU(remu.rhs.idx, remu.lhs.idx, remu.dest.idx));
}

void rv32i_emit_or(struct rv32i_emitter *emitter, const struct rv32i_op or) {
  rv32i_emit_instr(emitter, OR(or.rhs.idx, or.lhs.idx, or.dest.idx));
}
void rv32i_emit_xor(struct rv32i_emitter *emitter, const struct rv32i_op xor) {
  rv32i_emit_instr(emitter, XOR(xor.rhs.idx, xor.lhs.idx, xor.dest.idx));
}
void rv32i_emit_and(struct rv32i_emitter *emitter, const struct rv32i_op and) {
  rv32i_emit_instr(emitter, AND(and.rhs.idx, and.lhs.idx, and.dest.idx));
}

void rv32i_emit_sll(struct rv32i_emitter *emitter, const struct rv32i_op sll) {
  rv32i_emit_instr(emitter, SLL(sll.rhs.idx, sll.lhs.idx, sll.dest.idx));
}
void rv32i_emit_srl(struct rv32i_emitter *emitter, const struct rv32i_op srl) {
  rv32i_emit_instr(emitter, SRL(srl.rhs.idx, srl.lhs.idx, srl.dest.idx));
}
void rv32i_emit_sra(struct rv32i_emitter *emitter, const struct rv32i_op sra) {
  rv32i_emit_instr(emitter, SRA(sra.rhs.idx, sra.lhs.idx, sra.dest.idx));
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

void rv32i_emit_fsw(struct rv32i_emitter *emitter,
                    const struct rv32i_store fsw) {
  rv32i_emit_instr(emitter, FSW(fsw.imm, fsw.source.idx, fsw.addr.idx));
}

void rv32i_emit_flw(struct rv32i_emitter *emitter,
                    const struct rv32i_load flw) {
  rv32i_emit_instr(emitter, FLW(flw.imm, flw.dest.idx, flw.addr.idx));
}

void rv32i_emit_lb(struct rv32i_emitter *emitter, const struct rv32i_load lb) {
  rv32i_emit_instr(emitter, LB(lb.imm, lb.addr.idx, lb.dest.idx));
}

void rv32i_emit_lbu(struct rv32i_emitter *emitter,
                    const struct rv32i_load lbu) {
  rv32i_emit_instr(emitter, LBU(lbu.imm, lbu.addr.idx, lbu.dest.idx));
}

void rv32i_emit_lh(struct rv32i_emitter *emitter, const struct rv32i_load lh) {
  rv32i_emit_instr(emitter, LH(lh.imm, lh.addr.idx, lh.dest.idx));
}

void rv32i_emit_lhu(struct rv32i_emitter *emitter,
                    const struct rv32i_load lhu) {
  rv32i_emit_instr(emitter, LHU(lhu.imm, lhu.addr.idx, lhu.dest.idx));
}

void rv32i_emit_lw(struct rv32i_emitter *emitter, const struct rv32i_load lw) {
  rv32i_emit_instr(emitter, LW(lw.imm, lw.addr.idx, lw.dest.idx));
}
