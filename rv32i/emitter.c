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

void rv32i_emit_addi(struct rv32i_emitter *emitter, const struct rv32i_addsub_imm addi) {
  rv32i_emit_instr(emitter, ADDI(addi.imm, addi.source.idx, addi.dest.idx));
}

void rv32i_emit_add(struct rv32i_emitter *emitter, const struct rv32i_addsub_reg add) {
  rv32i_emit_instr(emitter, ADD(add.lhs.idx, add.rhs.idx, add.dest.idx));
}
