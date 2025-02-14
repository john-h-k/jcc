#ifndef X64_EMITTER_H
#define X64_EMITTER_H

#include "codegen.h"

struct x64_emitter;
struct x64_target_reloc {
  size_t idx;
};

void create_x64_emitter(struct x64_emitter **emitter);
void free_x64_emitter(struct x64_emitter **emitter);

size_t x64_emitted_count(struct x64_emitter *emitter);
size_t x64_emit_bytesize(struct x64_emitter *emitter);
void x64_emit_copy_to(struct x64_emitter *emitter, void *dest);

void x64_get_bytes(struct x64_emitter *emitter, size_t start, size_t count, char *buff);

/* Nop */

void x64_emit_nop(struct x64_emitter *emitter);

/* SSE */

void x64_emit_mov_store_ss_imm(struct x64_emitter *emitter, struct x64_mov_store_imm mov_store_ss);
void x64_emit_mov_store_sd_imm(struct x64_emitter *emitter, struct x64_mov_store_imm mov_store_sd);
void x64_emit_mov_load_ss_imm(struct x64_emitter *emitter, struct x64_mov_load_imm mov_load_ss);
void x64_emit_mov_load_sd_imm(struct x64_emitter *emitter, struct x64_mov_load_imm mov_load_sd);

void x64_emit_movaps(struct x64_emitter *emitter, struct x64_2_reg_unary movaps);
void x64_emit_movapd(struct x64_emitter *emitter, struct x64_2_reg_unary movapd);

void x64_emit_cvtsi2ss(struct x64_emitter *emitter, struct x64_2_reg_unary cvtsi2ss);
void x64_emit_cvtsi2sd(struct x64_emitter *emitter, struct x64_2_reg_unary cvtsi2sd);

void x64_emit_cvttss2si(struct x64_emitter *emitter, struct x64_2_reg_unary cvttss2si);
void x64_emit_cvttsd2si(struct x64_emitter *emitter, struct x64_2_reg_unary cvttsd2si);

void x64_emit_cvtss2si(struct x64_emitter *emitter, struct x64_2_reg_unary cvtss2si);
void x64_emit_cvtsd2si(struct x64_emitter *emitter, struct x64_2_reg_unary cvtsd2si);

void x64_emit_cvtss2sd(struct x64_emitter *emitter, struct x64_2_reg_unary cvtss2sd);
void x64_emit_cvtsd2ss(struct x64_emitter *emitter, struct x64_2_reg_unary cvtsd2ss);

void x64_emit_addss(struct x64_emitter *emitter, struct x64_alu_reg addss);
void x64_emit_addsd(struct x64_emitter *emitter, struct x64_alu_reg addsd);
void x64_emit_subss(struct x64_emitter *emitter, struct x64_alu_reg subss);
void x64_emit_subsd(struct x64_emitter *emitter, struct x64_alu_reg subsd);
void x64_emit_mulss(struct x64_emitter *emitter, struct x64_alu_reg mulss);
void x64_emit_mulsd(struct x64_emitter *emitter, struct x64_alu_reg mulsd);
void x64_emit_divss(struct x64_emitter *emitter, struct x64_alu_reg divss);
void x64_emit_divsd(struct x64_emitter *emitter, struct x64_alu_reg divsd);

void x64_emit_ucomiss(struct x64_emitter *emitter, struct x64_cmp ucomiss);
void x64_emit_ucomisd(struct x64_emitter *emitter, struct x64_cmp ucomisd);

void x64_emit_sqrtss(struct x64_emitter *emitter, struct x64_2_reg_unary sqrtss);
void x64_emit_sqrtsd(struct x64_emitter *emitter, struct x64_2_reg_unary sqrtsd);

void x64_emit_andps(struct x64_emitter *emitter, struct x64_alu_reg andps);
void x64_emit_andpd(struct x64_emitter *emitter, struct x64_alu_reg andpd);
void x64_emit_xorps(struct x64_emitter *emitter, struct x64_alu_reg xorps);
void x64_emit_xorpd(struct x64_emitter *emitter, struct x64_alu_reg xorpd);
void x64_emit_orps(struct x64_emitter *emitter, struct x64_alu_reg orps);
void x64_emit_orpd(struct x64_emitter *emitter, struct x64_alu_reg orpd);

/* Move immediate */

void x64_emit_mov_imm(struct x64_emitter *emitter, struct x64_mov_imm mov_imm);
void x64_emit_mov_reg(struct x64_emitter *emitter, struct x64_mov_reg mov_reg);
void x64_emit_movq(struct x64_emitter *emitter, struct x64_mov_reg movq);
void x64_emit_movd(struct x64_emitter *emitter, struct x64_mov_reg movd);
void x64_emit_movsx(struct x64_emitter *emitter, struct x64_mov_reg movsx);

void x64_emit_add(struct x64_emitter *emitter, struct x64_alu_reg add);
void x64_emit_sub(struct x64_emitter *emitter, struct x64_alu_reg sub);
void x64_emit_xor(struct x64_emitter *emitter, struct x64_alu_reg xor);
void x64_emit_and(struct x64_emitter *emitter, struct x64_alu_reg and);
void x64_emit_or(struct x64_emitter *emitter, struct x64_alu_reg or);

void x64_emit_div(struct x64_emitter *emitter, struct x64_div div);
void x64_emit_idiv(struct x64_emitter *emitter, struct x64_div idiv);
void x64_emit_imul(struct x64_emitter *emitter, struct x64_mul imul);

void x64_emit_add_imm(struct x64_emitter *emitter, struct x64_alu_imm add_imm);
void x64_emit_sub_imm(struct x64_emitter *emitter, struct x64_alu_imm sub_imm);
void x64_emit_or_imm(struct x64_emitter *emitter, struct x64_alu_imm or_imm);
void x64_emit_xor_imm(struct x64_emitter *emitter, struct x64_alu_imm xor_imm);
void x64_emit_and_imm(struct x64_emitter *emitter, struct x64_alu_imm and_imm);

void x64_emit_not(struct x64_emitter *emitter, struct x64_1_reg not);
void x64_emit_neg(struct x64_emitter *emitter, struct x64_1_reg neg);

void x64_emit_shl(struct x64_emitter *emitter, struct x64_shift shl);
void x64_emit_shr(struct x64_emitter *emitter, struct x64_shift shr);
void x64_emit_sar(struct x64_emitter *emitter, struct x64_shift sar);

void x64_emit_mov_load_imm(struct x64_emitter *emitter, struct x64_mov_load_imm mov_load_imm);

void x64_emit_movzx_load_half_imm(struct x64_emitter *emitter, struct x64_mov_load_imm movzx_load_half_imm);
void x64_emit_movzx_load_byte_imm(struct x64_emitter *emitter, struct x64_mov_load_imm movzx_load_byte_imm);

void x64_emit_mov_store_imm(struct x64_emitter *emitter, struct x64_mov_store_imm mov_store_imm);
void x64_emit_mov_store_half_imm(struct x64_emitter *emitter, struct x64_mov_store_imm mov_store_half_imm);
void x64_emit_mov_store_byte_imm(struct x64_emitter *emitter, struct x64_mov_store_imm mov_store_byte_imm);

void x64_emit_lea_pcrel(struct x64_emitter *emitter, struct x64_lea_pcrel lea_pcrel);
void x64_emit_lea(struct x64_emitter *emitter, struct x64_lea lea);

void x64_emit_push(struct x64_emitter *emitter, struct x64_push push);
void x64_emit_pop(struct x64_emitter *emitter, struct x64_pop pop);

void x64_emit_call(struct x64_emitter *emitter, struct x64_branch call);

void x64_emit_jmp_reg(struct x64_emitter *emitter, struct x64_branch_reg jmp_reg);
void x64_emit_call_reg(struct x64_emitter *emitter, struct x64_branch_reg call_reg);

struct x64_target_reloc x64_emit_jmp(struct x64_emitter *emitter, struct x64_branch jmp);
struct x64_target_reloc x64_emit_jcc(struct x64_emitter *emitter, struct x64_conditional_branch jcc);

void x64_emit_setcc(struct x64_emitter *emitter, struct x64_conditional_select setcc);

void x64_emit_cmp(struct x64_emitter *emitter, struct x64_cmp cmp);
void x64_emit_test(struct x64_emitter *emitter, struct x64_cmp test);

void x64_emit_cmp_imm(struct x64_emitter *emitter, struct x64_cmp_imm cmp_imm);
void x64_emit_test_imm(struct x64_emitter *emitter, struct x64_cmp_imm test_imm);

void x64_emit_ret(struct x64_emitter *emitter);

void x64_reloc(struct x64_emitter *emitter, struct x64_target_reloc reloc, size_t offset);

#endif
