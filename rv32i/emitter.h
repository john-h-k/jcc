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

void rv32i_emit_lui(struct rv32i_emitter *emitter, const struct rv32i_u lui);
void rv32i_emit_auipc(struct rv32i_emitter *emitter, const struct rv32i_u auipc);

void rv32i_emit_addi(struct rv32i_emitter *emitter,
                     const struct rv32i_op_imm addi);
void rv32i_emit_xori(struct rv32i_emitter *emitter,
                     const struct rv32i_op_imm xori);
void rv32i_emit_jalr(struct rv32i_emitter *emitter,
                     const struct rv32i_jalr jalr);
void rv32i_emit_jal(struct rv32i_emitter *emitter, const struct rv32i_jal jal);

void rv32i_emit_beq(struct rv32i_emitter *emitter,
                    const struct rv32i_conditional_branch beq);
void rv32i_emit_bne(struct rv32i_emitter *emitter,
                    const struct rv32i_conditional_branch bne);
void rv32i_emit_blt(struct rv32i_emitter *emitter,
                    const struct rv32i_conditional_branch blt);
void rv32i_emit_bge(struct rv32i_emitter *emitter,
                    const struct rv32i_conditional_branch bge);
void rv32i_emit_bltu(struct rv32i_emitter *emitter,
                     const struct rv32i_conditional_branch bltu);
void rv32i_emit_bgeu(struct rv32i_emitter *emitter,
                     const struct rv32i_conditional_branch bgeu);

void rv32i_emit_ori(struct rv32i_emitter *emitter, const struct rv32i_op_imm ori);

void rv32i_emit_andi(struct rv32i_emitter *emitter, const struct rv32i_op_imm andi);

void rv32i_emit_slli(struct rv32i_emitter *emitter, const struct rv32i_op_imm slli);

void rv32i_emit_srli(struct rv32i_emitter *emitter, const struct rv32i_op_imm srli);

void rv32i_emit_srai(struct rv32i_emitter *emitter, const struct rv32i_op_imm srai);

void rv32i_emit_fmv(struct rv32i_emitter *emitter,
                    const struct rv32i_op_mov fmv);
void rv32i_emit_fadd(struct rv32i_emitter *emitter,
                     const struct rv32i_op_fp fadd);
void rv32i_emit_fsub(struct rv32i_emitter *emitter,
                     const struct rv32i_op_fp fsub);
void rv32i_emit_fmul(struct rv32i_emitter *emitter,
                     const struct rv32i_op_fp fmul);
void rv32i_emit_fdiv(struct rv32i_emitter *emitter,
                     const struct rv32i_op_fp fdiv);

void rv32i_emit_fsgnj(struct rv32i_emitter *emitter,
                      const struct rv32i_op_fp fsgnj);
void rv32i_emit_fsgnjn(struct rv32i_emitter *emitter,
                       const struct rv32i_op_fp fsgnjn);
void rv32i_emit_fsgnjx(struct rv32i_emitter *emitter,
                       const struct rv32i_op_fp fsgnjx);

void rv32i_emit_fmax(struct rv32i_emitter *emitter,
                     const struct rv32i_op_fp fmax);
void rv32i_emit_fmin(struct rv32i_emitter *emitter,
                     const struct rv32i_op_fp fmin);

void rv32i_emit_fsqrt(struct rv32i_emitter *emitter,
                      const struct rv32i_op_unary_fp fsqrt);

void rv32i_emit_add(struct rv32i_emitter *emitter, const struct rv32i_op add);
void rv32i_emit_sub(struct rv32i_emitter *emitter, const struct rv32i_op sub);
void rv32i_emit_mul(struct rv32i_emitter *emitter, const struct rv32i_op mul);
void rv32i_emit_div(struct rv32i_emitter *emitter, const struct rv32i_op div);
void rv32i_emit_rem(struct rv32i_emitter *emitter, const struct rv32i_op rem);
void rv32i_emit_divu(struct rv32i_emitter *emitter, const struct rv32i_op divu);
void rv32i_emit_remu(struct rv32i_emitter *emitter, const struct rv32i_op remu);

void rv32i_emit_or(struct rv32i_emitter *emitter, const struct rv32i_op or);
void rv32i_emit_xor(struct rv32i_emitter *emitter, const struct rv32i_op xor);
void rv32i_emit_and(struct rv32i_emitter *emitter, const struct rv32i_op and);

void rv32i_emit_sll(struct rv32i_emitter *emitter, const struct rv32i_op sll);
void rv32i_emit_srl(struct rv32i_emitter *emitter, const struct rv32i_op srl);
void rv32i_emit_sra(struct rv32i_emitter *emitter, const struct rv32i_op sra);

void rv32i_emit_sb(struct rv32i_emitter *emitter, const struct rv32i_store sb);
void rv32i_emit_sh(struct rv32i_emitter *emitter, const struct rv32i_store sh);
void rv32i_emit_sw(struct rv32i_emitter *emitter, const struct rv32i_store sw);
void rv32i_emit_fsw(struct rv32i_emitter *emitter,
                    const struct rv32i_store fsw);

void rv32i_emit_lb(struct rv32i_emitter *emitter, const struct rv32i_load lb);
void rv32i_emit_lbu(struct rv32i_emitter *emitter, const struct rv32i_load lbu);
void rv32i_emit_lh(struct rv32i_emitter *emitter, const struct rv32i_load lh);
void rv32i_emit_lhu(struct rv32i_emitter *emitter, const struct rv32i_load lhu);
void rv32i_emit_lw(struct rv32i_emitter *emitter, const struct rv32i_load lw);
void rv32i_emit_flw(struct rv32i_emitter *emitter, const struct rv32i_load flw);

#endif
