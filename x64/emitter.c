#include "emitter.h"

#include "../util.h"
#include "../vector.h"
#include "codegen.h"
#include "isa.h"

#include <stdlib.h>

struct x64_emitter {
  unsigned char *block;
  size_t count;
  size_t len;
  size_t head;

  struct vector *target_relocs;
};

enum x64_target_reloc_ty {
  X64_TARGET_RELOC_TY_JMP,
  X64_TARGET_RELOC_TY_JCC,
};

struct x64_target_reloc_data {
  enum x64_target_reloc_ty ty;

  size_t offset;
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
  (*emitter)->target_relocs = vector_create(sizeof(struct x64_target_reloc_data));
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
  vector_free(&(*emitter)->target_relocs);

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

/* SSE */

void x64_emit_movaps(struct x64_emitter *emitter, struct x64_2_reg_unary movaps) {
  x64_emit_instr(emitter, MOVAPS(movaps.dest, movaps.source));
}

void x64_emit_movapd(struct x64_emitter *emitter, struct x64_2_reg_unary movapd) {
  x64_emit_instr(emitter, MOVAPD(movapd.dest, movapd.source));
}

void x64_emit_sqrtss(struct x64_emitter *emitter, struct x64_2_reg_unary sqrtss) {
  x64_emit_instr(emitter, SQRTSS(sqrtss.dest, sqrtss.source));
}

void x64_emit_sqrtsd(struct x64_emitter *emitter, struct x64_2_reg_unary sqrtsd) {
  x64_emit_instr(emitter, SQRTSD(sqrtsd.dest, sqrtsd.source));
}

void x64_emit_andps(struct x64_emitter *emitter, struct x64_alu_reg andps) {
  x64_emit_instr(emitter, ANDPS(andps.dest, andps.rhs));
}
void x64_emit_andpd(struct x64_emitter *emitter, struct x64_alu_reg andpd) {
  x64_emit_instr(emitter, ANDPD(andpd.dest, andpd.rhs));
}
void x64_emit_xorps(struct x64_emitter *emitter, struct x64_alu_reg xorps) {
  x64_emit_instr(emitter, XORPS(xorps.dest, xorps.rhs));
}
void x64_emit_xorpd(struct x64_emitter *emitter, struct x64_alu_reg xorpd) {
  x64_emit_instr(emitter, XORPD(xorpd.dest, xorpd.rhs));
}
void x64_emit_orps(struct x64_emitter *emitter, struct x64_alu_reg orps) {
  x64_emit_instr(emitter, ORPS(orps.dest, orps.rhs));
}
void x64_emit_orpd(struct x64_emitter *emitter, struct x64_alu_reg orpd) {
  x64_emit_instr(emitter, ORPD(orpd.dest, orpd.rhs));
}

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

void x64_emit_imul(struct x64_emitter *emitter, struct x64_mul imul) {
  x64_emit_instr(emitter, IMUL_REG(imul.rhs));
}

void x64_emit_and(struct x64_emitter *emitter, struct x64_alu_reg and) {
  x64_emit_instr(emitter, AND_REG(and.dest, and.rhs));
}

void x64_emit_xor(struct x64_emitter *emitter, struct x64_alu_reg xor) {
  x64_emit_instr(emitter, XOR_REG(xor.dest, xor.rhs));
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

  DEBUG_ASSERT(dest.ty == X64_REG_TY_R ||
                   dest.ty == X64_REG_TY_E,
               "movsx dest must be 32 or 64 bit");

  switch (movsx.source.ty) {
  case X64_REG_TY_NONE:
    BUG("doesn't make sense");
  case X64_REG_TY_W:
    x64_emit_instr(emitter, dest.ty == X64_REG_TY_R
                                ? MOVSX16_64(movsx.dest, movsx.source)
                                : MOVSX16_32(movsx.dest, movsx.source));
    break;
  case X64_REG_TY_L:
    x64_emit_instr(emitter, dest.ty == X64_REG_TY_R
                                ? MOVSX8_64(movsx.dest, movsx.source)
                                : MOVSX8_32(movsx.dest, movsx.source));
    break;
  case X64_REG_TY_E:
    x64_emit_instr(emitter, MOVSX32_64(movsx.dest, movsx.source));
    break;
  case X64_REG_TY_R:
  case X64_REG_TY_XMM:
    BUG("source of movsx should be 8/16/32 bit register");
  }
}

void x64_emit_sub_imm(struct x64_emitter *emitter, struct x64_alu_imm sub_imm) {
  x64_emit_instr(emitter, SUB_IMM(sub_imm.dest, sub_imm.imm));
}

void x64_emit_or_imm(struct x64_emitter *emitter, struct x64_alu_imm or_imm) {
  x64_emit_instr(emitter, OR_IMM(or_imm.dest, or_imm.imm));
}

void x64_emit_xor_imm(struct x64_emitter *emitter, struct x64_alu_imm xor_imm) {
  x64_emit_instr(emitter, XOR_IMM(xor_imm.dest, xor_imm.imm));
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

void x64_emit_movq(struct x64_emitter *emitter, struct x64_mov_reg movq) {
  x64_emit_instr(emitter, SSE_MOVQ(movq.dest, movq.source));
}

void x64_emit_movd(struct x64_emitter *emitter, struct x64_mov_reg movd) {
  x64_emit_instr(emitter, SSE_MOVD(movd.dest, movd.source));
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

void x64_emit_movzx_load_half_imm(struct x64_emitter *emitter, struct x64_mov_load_imm movzx_load_half_imm) {
  x64_emit_instr(emitter, MOVZX_LOAD_HALF_IMM(movzx_load_half_imm.dest,
                                        movzx_load_half_imm.addr, movzx_load_half_imm.imm));
}

void x64_emit_movzx_load_byte_imm(struct x64_emitter *emitter, struct x64_mov_load_imm movzx_load_byte_imm) {
  x64_emit_instr(emitter, MOVZX_LOAD_BYTE_IMM(movzx_load_byte_imm.dest,
                                        movzx_load_byte_imm.addr, movzx_load_byte_imm.imm));
}

void x64_emit_mov_store_half_imm(struct x64_emitter *emitter, struct x64_mov_store_imm mov_store_half_imm) {
  x64_emit_instr(emitter, MOV_STORE_HALF_IMM(mov_store_half_imm.source,
                                        mov_store_half_imm.addr, mov_store_half_imm.imm));
}

void x64_emit_mov_store_byte_imm(struct x64_emitter *emitter, struct x64_mov_store_imm mov_store_byte_imm) {
  x64_emit_instr(emitter, MOV_STORE_BYTE_IMM(mov_store_byte_imm.source,
                                        mov_store_byte_imm.addr, mov_store_byte_imm.imm));
}

void x64_emit_lea_pcrel(struct x64_emitter *emitter, struct x64_lea_pcrel lea_pcrel) {
  x64_emit_instr(emitter, LEA_PCREL(lea_pcrel.dest, lea_pcrel.offset));
}

void x64_emit_lea(struct x64_emitter *emitter, struct x64_lea lea) {
  if (lea.scale) {
    x64_emit_instr(emitter, LEA_REG64(lea.dest, lea.index, lea.base, (size_t)tzcnt(lea.scale), lea.offset));
  } else {
    x64_emit_instr(emitter, LEA_NOIDX_REG64(lea.dest, lea.base, lea.offset));
  }
}

void x64_emit_push(struct x64_emitter *emitter, struct x64_push push) {
  x64_emit_instr(emitter, PUSH_REG64(push.source));
}

void x64_emit_pop(struct x64_emitter *emitter, struct x64_pop pop) {
  x64_emit_instr(emitter, POP_REG64(pop.dest));
}

void x64_emit_call(struct x64_emitter *emitter, struct x64_branch call) {
  x64_emit_instr(emitter, CALL_REL32(0));
}

void x64_emit_jmp_reg(struct x64_emitter *emitter, struct x64_branch_reg jmp_reg) {
  TODO("x64 emit jmp reg");
}

void x64_emit_call_reg(struct x64_emitter *emitter, struct x64_branch_reg call_reg) {
  TODO("x64 emit call reg");
}

void x64_emit_setcc(struct x64_emitter *emitter, struct x64_conditional_select setcc) {
  x64_emit_instr(emitter, SET_COND(setcc.cond, setcc.dest));
}

struct x64_target_reloc x64_emit_jmp(struct x64_emitter *emitter,
                                      UNUSED struct x64_branch jmp) {
  size_t cur_pos = x64_emit_bytesize(emitter);
  x64_emit_instr(emitter, JMP_REL());

  struct x64_target_reloc_data reloc = {.ty = X64_TARGET_RELOC_TY_JMP,
                                   .offset = cur_pos};

  size_t idx = vector_length(emitter->target_relocs);
  vector_push_back(emitter->target_relocs, &reloc);
  return (struct x64_target_reloc){ idx };
}

struct x64_target_reloc x64_emit_jcc(struct x64_emitter *emitter,
                                      struct x64_conditional_branch jcc) {
  size_t cur_pos = x64_emit_bytesize(emitter);
  x64_emit_instr(emitter, JMP_COND_REL(jcc.cond));

  struct x64_target_reloc_data reloc = {.ty = X64_TARGET_RELOC_TY_JCC,
                                   .offset = cur_pos};

  size_t idx = vector_length(emitter->target_relocs);
  vector_push_back(emitter->target_relocs, &reloc);
  return (struct x64_target_reloc){ idx };
}

void x64_reloc(struct x64_emitter *emitter,
               struct x64_target_reloc reloc, size_t offset) {
  struct x64_target_reloc_data *data = vector_get(emitter->target_relocs, reloc.idx);
  size_t base = data->offset;
  size_t reloc_offset = data->offset;

  switch (data->ty) {
  case X64_TARGET_RELOC_TY_JMP:
    base += 1;
    reloc_offset += 5;
    break;
  case X64_TARGET_RELOC_TY_JCC:
    base += 2;
    reloc_offset += 6;
    break;
  }

  ssize_t disp = offset - reloc_offset;

  memcpy(&emitter->block[base], &disp, 4);
}

void x64_emit_cmp(struct x64_emitter *emitter, struct x64_cmp cmp) {
  x64_emit_instr(emitter, CMP_REG(cmp.lhs, cmp.rhs));
}

void x64_emit_test(struct x64_emitter *emitter, struct x64_cmp test) {
  x64_emit_instr(emitter, TEST_REG(test.lhs, test.rhs));
}

void x64_emit_cmp_imm(struct x64_emitter *emitter, struct x64_cmp_imm cmp_imm) {
  TODO("cmp imm");
}

void x64_emit_test_imm(struct x64_emitter *emitter,
                       struct x64_cmp_imm test_imm) {
  TODO("test imm");
}

void x64_emit_ret(struct x64_emitter *emitter) { x64_emit_instr(emitter, RET); }
