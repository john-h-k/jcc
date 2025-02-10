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

void x64_emit_add(struct x64_emitter *emitter, struct x64_alu_reg alu_reg) {
  x64_emit_instr(emitter, ADD_REG(alu_reg.dest, alu_reg.rhs));
}

void x64_emit_sub(struct x64_emitter *emitter, struct x64_alu_reg alu_reg) {
  x64_emit_instr(emitter, SUB_REG(alu_reg.dest, alu_reg.rhs));
}

void x64_emit_mov_imm(struct x64_emitter *emitter, struct x64_mov_imm mov_imm) {
  x64_emit_instr(emitter, MOV_IMM(mov_imm.dest, mov_imm.imm));
}

void x64_emit_mov_reg(struct x64_emitter *emitter, struct x64_mov_reg mov_reg) {
  x64_emit_instr(emitter, MOV_REG(mov_reg.dest, mov_reg.source));
}

void x64_emit_ret(struct x64_emitter *emitter) { x64_emit_instr(emitter, RET); }
