#include "emitter.h"

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

void rv32i_emit_ori(struct rv32i_emitter *emitter,
                    const struct rv32i_op_imm ori) {
  rv32i_emit_instr(emitter, ORI(ori.imm, ori.source.idx, ori.dest.idx));
}

void rv32i_emit_andi(struct rv32i_emitter *emitter,
                     const struct rv32i_op_imm andi) {
  rv32i_emit_instr(emitter, ANDI(andi.imm, andi.source.idx, andi.dest.idx));
}

void rv32i_emit_slli(struct rv32i_emitter *emitter,
                     const struct rv32i_op_imm slli) {
  rv32i_emit_instr(emitter, SLLI(slli.imm, slli.source.idx, slli.dest.idx));
}

void rv32i_emit_srli(struct rv32i_emitter *emitter,
                     const struct rv32i_op_imm srli) {
  rv32i_emit_instr(emitter, SRLI(srli.imm, srli.source.idx, srli.dest.idx));
}

void rv32i_emit_srai(struct rv32i_emitter *emitter,
                     const struct rv32i_op_imm srai) {
  rv32i_emit_instr(emitter, SRAI(srai.imm, srai.source.idx, srai.dest.idx));
}

void rv32i_emit_lui(struct rv32i_emitter *emitter, const struct rv32i_u lui) {
  rv32i_emit_instr(emitter, LUI(lui.imm, lui.dest.idx));
}

void rv32i_emit_auipc(struct rv32i_emitter *emitter,
                      const struct rv32i_u auipc) {
  rv32i_emit_instr(emitter, AUIPC(auipc.imm, auipc.dest.idx));
}

void rv32i_emit_jalr(struct rv32i_emitter *emitter,
                     const struct rv32i_jalr jalr) {
  rv32i_emit_instr(emitter, JALR(jalr.imm, jalr.target.idx, jalr.ret_addr.idx));
}

#define OFFSET(instr)                                                          \
  switch (instr.target.ty) { \
  case RV32I_TARGET_TY_OFFSET: \
    offset = instr.target.offset; \
    break; \
  case RV32I_TARGET_TY_BASICBLOCK: {\
    signed long long cur_pos = rv32i_emitted_count(emitter);                     \
    signed long long target_pos = instr.target.basicblock->first_instr->id;                 \
                                                                                 \
    offset = (target_pos - cur_pos) * 4; \
    break; \
  } \
  case RV32I_TARGET_TY_SYMBOL: \
    /* will be relocated */ \
    offset = 0; \
    break; \
  } \


void rv32i_emit_jal(struct rv32i_emitter *emitter, const struct rv32i_jal jal) {
  simm_t offset = 0;
  OFFSET(jal);

  // // FIXME: won't work 
  // offset -= 4;

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

void rv32i_emit_slt(struct rv32i_emitter *emitter, const struct rv32i_op slt) {
  rv32i_emit_instr(emitter, SLT(slt.rhs.idx, slt.lhs.idx, slt.dest.idx));
}

void rv32i_emit_sltu(struct rv32i_emitter *emitter,
                     const struct rv32i_op sltu) {
  rv32i_emit_instr(emitter, SLTU(sltu.rhs.idx, sltu.lhs.idx, sltu.dest.idx));
}

void rv32i_emit_slti(struct rv32i_emitter *emitter,
                     const struct rv32i_op_imm slti) {
  rv32i_emit_instr(emitter, SLTI(slti.imm, slti.source.idx, slti.dest.idx));
}

void rv32i_emit_sltiu(struct rv32i_emitter *emitter,
                      const struct rv32i_op_imm sltiu) {
  rv32i_emit_instr(emitter, SLTIU(sltiu.imm, sltiu.source.idx, sltiu.dest.idx));
}

void rv32i_emit_feq_s(struct rv32i_emitter *emitter,
                      const struct rv32i_op_fp feq) {
  rv32i_emit_instr(emitter, FEQ_S(feq.rhs.idx, feq.lhs.idx, feq.dest.idx));
}
void rv32i_emit_flt_s(struct rv32i_emitter *emitter,
                      const struct rv32i_op_fp flt) {
  rv32i_emit_instr(emitter, FLT_S(flt.rhs.idx, flt.lhs.idx, flt.dest.idx));
}
void rv32i_emit_fle_s(struct rv32i_emitter *emitter,
                      const struct rv32i_op_fp fle) {
  rv32i_emit_instr(emitter, FLE_S(fle.rhs.idx, fle.lhs.idx, fle.dest.idx));
}

void rv32i_emit_feq_d(struct rv32i_emitter *emitter,
                      const struct rv32i_op_fp feq) {
  rv32i_emit_instr(emitter, FEQ_D(feq.rhs.idx, feq.lhs.idx, feq.dest.idx));
}
void rv32i_emit_flt_d(struct rv32i_emitter *emitter,
                      const struct rv32i_op_fp flt) {
  rv32i_emit_instr(emitter, FLT_D(flt.rhs.idx, flt.lhs.idx, flt.dest.idx));
}
void rv32i_emit_fle_d(struct rv32i_emitter *emitter,
                      const struct rv32i_op_fp fle) {
  rv32i_emit_instr(emitter, FLE_D(fle.rhs.idx, fle.lhs.idx, fle.dest.idx));
}

void rv32i_emit_fcvt_s(struct rv32i_emitter *emitter,
                       const struct rv32i_op_mov fcvt) {

  switch (fcvt.source.ty) {
  case RV32I_REG_TY_NONE:
    BUG("NONE type in fcvtu emit");
  case RV32I_REG_TY_W:
    rv32i_emit_instr(emitter, FCVT_SW(fcvt.source.idx, fcvt.dest.idx));
    break;
  case RV32I_REG_TY_F:
    switch (fcvt.dest.ty) {
    case RV32I_REG_TY_NONE:
      BUG("NONE type in fcvtu emit");
    case RV32I_REG_TY_W:
      rv32i_emit_instr(emitter, FCVT_WS(fcvt.source.idx, fcvt.dest.idx));
      break;
    case RV32I_REG_TY_F:
      rv32i_emit_instr(emitter, FCVT_D_S(fcvt.source.idx, fcvt.dest.idx));
      break;
    }
    break;
  }
}

void rv32i_emit_fcvtu_s(struct rv32i_emitter *emitter,
                        const struct rv32i_op_mov fcvtu) {
  switch (fcvtu.source.ty) {
  case RV32I_REG_TY_NONE:
    BUG("NONE type in fcvtu emit");
  case RV32I_REG_TY_W:
    rv32i_emit_instr(emitter, FCVTU_SW(fcvtu.source.idx, fcvtu.dest.idx));
    break;
  case RV32I_REG_TY_F:
    rv32i_emit_instr(emitter, FCVTU_WS(fcvtu.source.idx, fcvtu.dest.idx));
    break;
  }
}

void rv32i_emit_fcvt_d(struct rv32i_emitter *emitter,
                       const struct rv32i_op_mov fcvt) {

  switch (fcvt.source.ty) {
  case RV32I_REG_TY_NONE:
    BUG("NONE type in fcvtu emit");
  case RV32I_REG_TY_W:
    rv32i_emit_instr(emitter, FCVT_DW(fcvt.source.idx, fcvt.dest.idx));
    break;
  case RV32I_REG_TY_F:
    switch (fcvt.dest.ty) {
    case RV32I_REG_TY_NONE:
      BUG("NONE type in fcvtu emit");
    case RV32I_REG_TY_W:
      rv32i_emit_instr(emitter, FCVT_WD(fcvt.source.idx, fcvt.dest.idx));
      break;
    case RV32I_REG_TY_F:
      rv32i_emit_instr(emitter, FCVT_S_D(fcvt.source.idx, fcvt.dest.idx));
      break;
    }
    break;
  }
}

void rv32i_emit_fcvtu_d(struct rv32i_emitter *emitter,
                        const struct rv32i_op_mov fcvtu) {
  switch (fcvtu.source.ty) {
  case RV32I_REG_TY_NONE:
    BUG("NONE type in fcvtu emit");
  case RV32I_REG_TY_W:
    rv32i_emit_instr(emitter, FCVTU_DW(fcvtu.source.idx, fcvtu.dest.idx));
    break;
  case RV32I_REG_TY_F:
    rv32i_emit_instr(emitter, FCVTU_WD(fcvtu.source.idx, fcvtu.dest.idx));
    break;
  }
}

void rv32i_emit_fmv_s(struct rv32i_emitter *emitter,
                      const struct rv32i_op_mov fmv) {
  switch (fmv.source.ty) {
  case RV32I_REG_TY_NONE:
    BUG("NONE type in fmv emit");
  case RV32I_REG_TY_W:
    rv32i_emit_instr(emitter, FMV_SW(fmv.source.idx, fmv.dest.idx));
    break;
  case RV32I_REG_TY_F:
    rv32i_emit_instr(emitter, FMV_WS(fmv.source.idx, fmv.dest.idx));
    break;
  }
}

void rv32i_emit_fmv_d(struct rv32i_emitter *emitter,
                      const struct rv32i_op_mov fmv) {
  BUG("not supported on rv32i");
}

void rv32i_emit_fadd_s(struct rv32i_emitter *emitter,
                       const struct rv32i_op_fp fadd) {
  rv32i_emit_instr(emitter, FADD_S(fadd.rhs.idx, fadd.lhs.idx, fadd.dest.idx));
}

void rv32i_emit_fadd_d(struct rv32i_emitter *emitter,
                       const struct rv32i_op_fp fadd) {
  rv32i_emit_instr(emitter, FADD_D(fadd.rhs.idx, fadd.lhs.idx, fadd.dest.idx));
}

void rv32i_emit_fsub_s(struct rv32i_emitter *emitter,
                       const struct rv32i_op_fp fsub) {
  rv32i_emit_instr(emitter, FSUB_S(fsub.rhs.idx, fsub.lhs.idx, fsub.dest.idx));
}

void rv32i_emit_fsub_d(struct rv32i_emitter *emitter,
                       const struct rv32i_op_fp fsub) {
  rv32i_emit_instr(emitter, FSUB_D(fsub.rhs.idx, fsub.lhs.idx, fsub.dest.idx));
}

void rv32i_emit_fmul_s(struct rv32i_emitter *emitter,
                       const struct rv32i_op_fp fmul) {
  rv32i_emit_instr(emitter, FMUL_S(fmul.rhs.idx, fmul.lhs.idx, fmul.dest.idx));
}

void rv32i_emit_fmul_d(struct rv32i_emitter *emitter,
                       const struct rv32i_op_fp fmul) {
  rv32i_emit_instr(emitter, FMUL_D(fmul.rhs.idx, fmul.lhs.idx, fmul.dest.idx));
}

void rv32i_emit_fdiv_s(struct rv32i_emitter *emitter,
                       const struct rv32i_op_fp fdiv) {
  rv32i_emit_instr(emitter, FDIV_S(fdiv.rhs.idx, fdiv.lhs.idx, fdiv.dest.idx));
}

void rv32i_emit_fdiv_d(struct rv32i_emitter *emitter,
                       const struct rv32i_op_fp fdiv) {
  rv32i_emit_instr(emitter, FDIV_D(fdiv.rhs.idx, fdiv.lhs.idx, fdiv.dest.idx));
}

void rv32i_emit_fsgnj_s(struct rv32i_emitter *emitter,
                        const struct rv32i_op_fp fsgnj) {
  rv32i_emit_instr(emitter,
                   FSGNJ_S(fsgnj.rhs.idx, fsgnj.lhs.idx, fsgnj.dest.idx));
}

void rv32i_emit_fsgnj_d(struct rv32i_emitter *emitter,
                        const struct rv32i_op_fp fsgnj) {
  rv32i_emit_instr(emitter,
                   FSGNJ_D(fsgnj.rhs.idx, fsgnj.lhs.idx, fsgnj.dest.idx));
}

void rv32i_emit_fsgnjn_s(struct rv32i_emitter *emitter,
                         const struct rv32i_op_fp fsgnjn) {
  rv32i_emit_instr(emitter,
                   FSGNJN_S(fsgnjn.rhs.idx, fsgnjn.lhs.idx, fsgnjn.dest.idx));
}

void rv32i_emit_fsgnjn_d(struct rv32i_emitter *emitter,
                         const struct rv32i_op_fp fsgnjn) {
  rv32i_emit_instr(emitter,
                   FSGNJN_D(fsgnjn.rhs.idx, fsgnjn.lhs.idx, fsgnjn.dest.idx));
}

void rv32i_emit_fsgnjx_s(struct rv32i_emitter *emitter,
                         const struct rv32i_op_fp fsgnjx) {
  rv32i_emit_instr(emitter,
                   FSGNJX_S(fsgnjx.rhs.idx, fsgnjx.lhs.idx, fsgnjx.dest.idx));
}

void rv32i_emit_fsgnjx_d(struct rv32i_emitter *emitter,
                         const struct rv32i_op_fp fsgnjx) {
  rv32i_emit_instr(emitter,
                   FSGNJX_D(fsgnjx.rhs.idx, fsgnjx.lhs.idx, fsgnjx.dest.idx));
}

void rv32i_emit_fmax_s(struct rv32i_emitter *emitter,
                       const struct rv32i_op_fp fmax) {
  rv32i_emit_instr(emitter, FMAX_S(fmax.rhs.idx, fmax.lhs.idx, fmax.dest.idx));
}

void rv32i_emit_fmax_d(struct rv32i_emitter *emitter,
                       const struct rv32i_op_fp fmax) {
  rv32i_emit_instr(emitter, FMAX_D(fmax.rhs.idx, fmax.lhs.idx, fmax.dest.idx));
}

void rv32i_emit_fmin_s(struct rv32i_emitter *emitter,
                       const struct rv32i_op_fp fmin) {
  rv32i_emit_instr(emitter, FMIN_S(fmin.rhs.idx, fmin.lhs.idx, fmin.dest.idx));
}

void rv32i_emit_fmin_d(struct rv32i_emitter *emitter,
                       const struct rv32i_op_fp fmin) {
  rv32i_emit_instr(emitter, FMIN_D(fmin.rhs.idx, fmin.lhs.idx, fmin.dest.idx));
}

void rv32i_emit_fsqrt_s(struct rv32i_emitter *emitter,
                        const struct rv32i_op_unary_fp fsqrt) {
  rv32i_emit_instr(emitter, FSQRT_S(fsqrt.source.idx, fsqrt.dest.idx));
}

void rv32i_emit_fsqrt_d(struct rv32i_emitter *emitter,
                        const struct rv32i_op_unary_fp fsqrt) {
  rv32i_emit_instr(emitter, FSQRT_D(fsqrt.source.idx, fsqrt.dest.idx));
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
void rv32i_emit_mulh(struct rv32i_emitter *emitter,
                     const struct rv32i_op mulh) {
  rv32i_emit_instr(emitter, MULH(mulh.rhs.idx, mulh.lhs.idx, mulh.dest.idx));
}
void rv32i_emit_mulhu(struct rv32i_emitter *emitter,
                      const struct rv32i_op mulhu) {
  rv32i_emit_instr(emitter,
                   MULHU(mulhu.rhs.idx, mulhu.lhs.idx, mulhu.dest.idx));
}
void rv32i_emit_mulhsu(struct rv32i_emitter *emitter,
                       const struct rv32i_op mulhsu) {
  rv32i_emit_instr(emitter,
                   MULHSU(mulhsu.rhs.idx, mulhsu.lhs.idx, mulhsu.dest.idx));
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

void rv32i_emit_fsd(struct rv32i_emitter *emitter,
                    const struct rv32i_store fsd) {
  rv32i_emit_instr(emitter, FSD(fsd.imm, fsd.source.idx, fsd.addr.idx));
}

void rv32i_emit_flw(struct rv32i_emitter *emitter,
                    const struct rv32i_load flw) {
  rv32i_emit_instr(emitter, FLW(flw.imm, flw.addr.idx, flw.dest.idx));
}

void rv32i_emit_fld(struct rv32i_emitter *emitter,
                    const struct rv32i_load fld) {
  rv32i_emit_instr(emitter, FLD(fld.imm, fld.addr.idx, fld.dest.idx));
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

void rv32i_emit_ecall(struct rv32i_emitter *emitter) {
  rv32i_emit_instr(emitter, ECALL);
}

void rv32i_emit_ebreak(struct rv32i_emitter *emitter) {
  rv32i_emit_instr(emitter, EBREAK);
}
