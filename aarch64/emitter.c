#include "emitter.h"

#include "../alloc.h"
#include "../bit_twiddle.h"
#include "../log.h"
#include "../util.h"
#include "codegen.h"
#include "isa.h"

#include <stdlib.h>

struct aarch64_emitter {
  uint32_t *block;
  size_t len;
  size_t head;
};

#define BLOCK_SIZE 4096

#define IS64_REG(r) (r).ty == AARCH64_REG_TY_X
#define IS64(d) (d).dest.ty == AARCH64_REG_TY_X
#define IS32(d) (d).dest.ty == AARCH64_REG_TY_W

#define ISDBL(d) (d).dest.ty == AARCH64_REG_TY_D
#define ISDBL_REG(d) (d).ty == AARCH64_REG_TY_D
#define ISFLT(d) (d).dest.ty == AARCH64_REG_TY_S
#define ISFLT_REG(d) (d).ty == AARCH64_REG_TY_S

void bad_instr() {
  bug("register types or arguments did not make sense");
}

void create_aarch64_emitter(struct aarch64_emitter **emitter) {
  *emitter = nonnull_malloc(sizeof(**emitter));

  (*emitter)->len = BLOCK_SIZE;
  (*emitter)->block =
      nonnull_malloc((*emitter)->len * sizeof((*emitter)->block));
  (*emitter)->head = 0;
}

size_t aarch64_emit_bytesize(struct aarch64_emitter *emitter) {
  return emitter->head * sizeof(*emitter->block);
}

size_t aarch64_emitted_count(struct aarch64_emitter *emitter) {
  return emitter->head;
}

void aarch64_emit_copy_to(struct aarch64_emitter *emitter, void *dest) {
  memcpy(dest, emitter->block, emitter->head * sizeof(*emitter->block));
}

void free_aarch64_emitter(struct aarch64_emitter **emitter) {
  free((*emitter)->block);

  free(*emitter);
  *emitter = NULL;
}

void aarch64_emit(struct aarch64_emitter *emitter, uint32_t instr) {
  if (emitter->head >= emitter->len) {
    size_t new_len = emitter->len + BLOCK_SIZE;
    emitter->block =
        nonnull_realloc(emitter->block, new_len * sizeof(emitter->block));
    emitter->len = new_len;
  }

  emitter->block[emitter->head++] = instr;
}

/* Nop */

void aarch64_emit_nop(struct aarch64_emitter *emitter) {
  aarch64_emit(emitter, NOP);
}

/* Single reg FP data processing */

void aarch64_emit_scvtf(struct aarch64_emitter *emitter, const struct aarch64_reg_1_source scvtf) {
  struct aarch64_reg dest = scvtf.dest;
  struct aarch64_reg source = scvtf.source;

  if (source.ty == AARCH64_REG_TY_S) {
    if (dest.ty == AARCH64_REG_TY_W) {
      aarch64_emit(emitter, FCVTZS_S_TO_32(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_X) {
      aarch64_emit(emitter, FCVTZS_S_TO_64(source.idx, dest.idx));
      return;
    }
  } else if (source.ty == AARCH64_REG_TY_D) {
    if (dest.ty == AARCH64_REG_TY_W) {
      aarch64_emit(emitter, FCVTZS_D_TO_32(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_X) {
      aarch64_emit(emitter, FCVTZS_D_TO_64(source.idx, dest.idx));
      return;
    }
  } else if (source.ty == AARCH64_REG_TY_W) {
    if (dest.ty == AARCH64_REG_TY_S) {
      aarch64_emit(emitter, SCVTF_32_TO_S(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_D) {
      aarch64_emit(emitter, SCVTF_32_TO_D(source.idx, dest.idx));
      return;
    }
  } else if (source.ty == AARCH64_REG_TY_X) {
    if (dest.ty == AARCH64_REG_TY_S) {
      aarch64_emit(emitter, SCVTF_64_TO_S(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_D) {
      aarch64_emit(emitter, SCVTF_64_TO_D(source.idx, dest.idx));
      return;
    }
  }

  bad_instr();
}

void aarch64_emit_ucvtf(struct aarch64_emitter *emitter, const struct aarch64_reg_1_source ucvtf) {
  struct aarch64_reg dest = ucvtf.dest;
  struct aarch64_reg source = ucvtf.source;

  if (source.ty == AARCH64_REG_TY_S) {
    if (dest.ty == AARCH64_REG_TY_W) {
      aarch64_emit(emitter, FCVTZU_S_TO_32(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_X) {
      aarch64_emit(emitter, FCVTZU_S_TO_64(source.idx, dest.idx));
      return;
    }
  } else if (source.ty == AARCH64_REG_TY_D) {
    if (dest.ty == AARCH64_REG_TY_W) {
      aarch64_emit(emitter, FCVTZU_D_TO_32(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_X) {
      aarch64_emit(emitter, FCVTZU_D_TO_64(source.idx, dest.idx));
      return;
    }
  } else if (source.ty == AARCH64_REG_TY_W) {
    if (dest.ty == AARCH64_REG_TY_S) {
      aarch64_emit(emitter, UCVTF_32_TO_S(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_D) {
      aarch64_emit(emitter, UCVTF_32_TO_D(source.idx, dest.idx));
      return;
    }
  } else if (source.ty == AARCH64_REG_TY_X) {
    if (dest.ty == AARCH64_REG_TY_S) {
      aarch64_emit(emitter, UCVTF_64_TO_S(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_D) {
      aarch64_emit(emitter, UCVTF_64_TO_D(source.idx, dest.idx));
      return;
    }
  }

  bad_instr();
}

void aarch64_emit_fcvt(struct aarch64_emitter *emitter, const struct aarch64_reg_1_source fcvt) {
  struct aarch64_reg dest = fcvt.dest;
  struct aarch64_reg source = fcvt.source;

  if (source.ty == AARCH64_REG_TY_D && dest.ty == AARCH64_REG_TY_S) {
    aarch64_emit(emitter, FCVT_D_TO_S(source.idx, dest.idx));
  } else if (source.ty == AARCH64_REG_TY_S && dest.ty == AARCH64_REG_TY_D) {
    aarch64_emit(emitter, FCVT_S_TO_D(source.idx, dest.idx));
  } else {
    bad_instr();
  }
}

void aarch64_emit_fmov(struct aarch64_emitter *emitter, const struct aarch64_reg_1_source fmov) {
  struct aarch64_reg dest = fmov.dest;
  struct aarch64_reg source = fmov.source;

  if (dest.ty == AARCH64_REG_TY_S) {
    if (source.ty == AARCH64_REG_TY_W) {
      aarch64_emit(emitter, FMOV_32_TO_S(source.idx, dest.idx));
      return;
    } else if (source.ty == AARCH64_REG_TY_X) {
      aarch64_emit(emitter, FMOV_64_TO_S(source.idx, dest.idx));
      return;
    } else if (source.ty == AARCH64_REG_TY_S) {
      aarch64_emit(emitter, FMOV_S(source.idx, dest.idx));
      return;
    }
  } else if (dest.ty == AARCH64_REG_TY_D) {
    if (source.ty == AARCH64_REG_TY_W) {
      aarch64_emit(emitter, FMOV_32_TO_D(source.idx, dest.idx));
      return;
    } else if (source.ty == AARCH64_REG_TY_X) {
      aarch64_emit(emitter, FMOV_64_TO_D(source.idx, dest.idx));
      return;
    } else if (source.ty == AARCH64_REG_TY_D) {
      aarch64_emit(emitter, FMOV_D(source.idx, dest.idx));
      return;
    }
  }

  bad_instr();
}

/* Two reg FP data processing */

void aarch64_emit_fadd(struct aarch64_emitter *emitter, const struct aarch64_reg_2_source fadd) {
  if (ISDBL(fadd)) {
    aarch64_emit(emitter, FADD_D(fadd.rhs.idx, fadd.lhs.idx, fadd.dest.idx));
  } else if (ISFLT(fadd)) {
    aarch64_emit(emitter, FADD_S(fadd.rhs.idx, fadd.lhs.idx, fadd.dest.idx));
  } else {
    bad_instr();
  }
}

void aarch64_emit_fsub(struct aarch64_emitter *emitter, const struct aarch64_reg_2_source fsub) {
  if (ISDBL(fsub)) {
    aarch64_emit(emitter, FSUB_D(fsub.rhs.idx, fsub.lhs.idx, fsub.dest.idx));
  } else if (ISFLT(fsub)) {
    aarch64_emit(emitter, FSUB_S(fsub.rhs.idx, fsub.lhs.idx, fsub.dest.idx));
  } else {
    bad_instr();
  }
}

void aarch64_emit_fmul(struct aarch64_emitter *emitter, const struct aarch64_reg_2_source fmul) {
  if (ISDBL(fmul)) {
    aarch64_emit(emitter, FMUL_D(fmul.rhs.idx, fmul.lhs.idx, fmul.dest.idx));
  } else if (ISFLT(fmul)) {
    aarch64_emit(emitter, FMUL_S(fmul.rhs.idx, fmul.lhs.idx, fmul.dest.idx));
  } else {
    bad_instr();
  }
}

void aarch64_emit_fdiv(struct aarch64_emitter *emitter, const struct aarch64_reg_2_source fdiv) {
  if (ISDBL(fdiv)) {
    aarch64_emit(emitter, FDIV_D(fdiv.rhs.idx, fdiv.lhs.idx, fdiv.dest.idx));
  } else if (ISFLT(fdiv)) {
    aarch64_emit(emitter, FDIV_S(fdiv.rhs.idx, fdiv.lhs.idx, fdiv.dest.idx));
  } else {
    bad_instr();
  }
}

/* Register moves */

void aarch64_emit_movz(struct aarch64_emitter *emitter,
                          const struct aarch64_mov_imm movz) {
  if (!UNS_FITS_IN_BITS(movz.imm, 16)) {
    bug("int too big");
  }

  if (IS64(movz)) {
    aarch64_emit(emitter, MOVZ_64(movz.shift, (uint16_t)movz.imm, movz.dest.idx));
  } else {
    aarch64_emit(emitter, MOVZ_32(movz.shift, (uint16_t)movz.imm, movz.dest.idx));
  }
}

void aarch64_emit_movk(struct aarch64_emitter *emitter,
                          const struct aarch64_mov_imm movk) {
  if (!UNS_FITS_IN_BITS(movk.imm, 16)) {
    bug("int too big");
  }

  if (IS64(movk)) {
    aarch64_emit(emitter, MOVK_64(movk.shift, (uint16_t)movk.imm, movk.dest.idx));
  } else {
    aarch64_emit(emitter, MOVK_32(movk.shift, (uint16_t)movk.imm, movk.dest.idx));
  }
}

void aarch64_emit_movn(struct aarch64_emitter *emitter,
                          const struct aarch64_mov_imm movn) {
  if (!UNS_FITS_IN_BITS(movn.imm, 16)) {
    bug("int too big");
  }

  if (IS64(movn)) {
    aarch64_emit(emitter, MOVN_64(movn.shift, (uint16_t)movn.imm, movn.dest.idx));
  } else {
    aarch64_emit(emitter, MOVN_32(movn.shift, (uint16_t)movn.imm, movn.dest.idx));
  }
}

/* Bitfield operations (Immediate) */

void aarch64_emit_sbfm(struct aarch64_emitter *emitter,
                       const struct aarch64_bitfield bf) {
  if (IS64(bf)) {
    aarch64_emit(emitter,
                 SBFM_64_IMM(bf.immr, bf.imms, bf.source.idx, bf.dest.idx));
  } else {
    aarch64_emit(emitter,
                 SBFM_32_IMM(bf.immr, bf.imms, bf.source.idx, bf.dest.idx));
  }
}

void aarch64_emit_bfm(struct aarch64_emitter *emitter,
                      const struct aarch64_bitfield bf) {
  if (IS64(bf)) {
    aarch64_emit(emitter,
                 BFM_64_IMM(bf.immr, bf.imms, bf.source.idx, bf.dest.idx));
  } else {
    aarch64_emit(emitter,
                 BFM_32_IMM(bf.immr, bf.imms, bf.source.idx, bf.dest.idx));
  }
}

void aarch64_emit_ubfm(struct aarch64_emitter *emitter,
                       const struct aarch64_bitfield bf) {
  if (IS64(bf)) {
    aarch64_emit(emitter,
                 UBFM_64_IMM(bf.immr, bf.imms, bf.source.idx, bf.dest.idx));
  } else {
    aarch64_emit(emitter,
                 UBFM_32_IMM(bf.immr, bf.imms, bf.source.idx, bf.dest.idx));
  }
}

/* Logical (register) */

void aarch64_emit_mvn(struct aarch64_emitter *emitter,
                      const struct aarch64_reg_1_source_with_shift mvn) {
  UNUSED_ARG(emitter);
  UNUSED_ARG(mvn);

  todo("mvn");
  // if (IS64(mvn)) {
  //       aarch64_emit(emitter, MVN(log.shift, log.rhs.idx, log.imm6,
  //                                    log.lhs.idx, log.dest.idx));
  // } else {
  //   aarch64_emit(emitter, AND_32_REG(log.shift, log.rhs.idx, log.imm6,
  //                                    log.lhs.idx, log.dest.idx));
  // }
}

void aarch64_emit_and(struct aarch64_emitter *emitter,
                      const struct aarch64_logical_reg log) {
  if (IS64(log)) {
    aarch64_emit(emitter, AND_64_REG(log.shift, log.rhs.idx, log.imm6,
                                     log.lhs.idx, log.dest.idx));
  } else {
    aarch64_emit(emitter, AND_32_REG(log.shift, log.rhs.idx, log.imm6,
                                     log.lhs.idx, log.dest.idx));
  }
}

void aarch64_emit_ands(struct aarch64_emitter *emitter,
                      const struct aarch64_logical_reg log) {
  if (IS64(log)) {
    aarch64_emit(emitter, ANDS_64_REG(log.shift, log.rhs.idx, log.imm6,
                                     log.lhs.idx, log.dest.idx));
  } else {
    aarch64_emit(emitter, ANDS_32_REG(log.shift, log.rhs.idx, log.imm6,
                                     log.lhs.idx, log.dest.idx));
  }
}

void aarch64_emit_eor(struct aarch64_emitter *emitter,
                      const struct aarch64_logical_reg log) {
  if (IS64(log)) {
    aarch64_emit(emitter, EOR_64_REG(log.shift, log.rhs.idx, log.imm6,
                                     log.lhs.idx, log.dest.idx));
  } else {
    aarch64_emit(emitter, EOR_32_REG(log.shift, log.rhs.idx, log.imm6,
                                     log.lhs.idx, log.dest.idx));
  }
}

void aarch64_emit_eon(struct aarch64_emitter *emitter,
                      const struct aarch64_logical_reg log) {
  if (IS64(log)) {
    aarch64_emit(emitter, EON_64_REG(log.shift, log.rhs.idx, log.imm6,
                                     log.lhs.idx, log.dest.idx));
  } else {
    aarch64_emit(emitter, EON_32_REG(log.shift, log.rhs.idx, log.imm6,
                                     log.lhs.idx, log.dest.idx));
  }
}

void aarch64_emit_orr(struct aarch64_emitter *emitter,
                      const struct aarch64_logical_reg log) {
  if (IS64(log)) {
    aarch64_emit(emitter, ORR_64_REG(log.shift, log.rhs.idx, log.imm6,
                                     log.lhs.idx, log.dest.idx));
  } else {
    aarch64_emit(emitter, ORR_32_REG(log.shift, log.rhs.idx, log.imm6,
                                     log.lhs.idx, log.dest.idx));
  }
}

void aarch64_emit_orn(struct aarch64_emitter *emitter,
                      const struct aarch64_logical_reg log) {
  if (IS64(log)) {
    aarch64_emit(emitter, ORN_64_REG(log.shift, log.rhs.idx, log.imm6,
                                     log.lhs.idx, log.dest.idx));
  } else {
    aarch64_emit(emitter, ORN_32_REG(log.shift, log.rhs.idx, log.imm6,
                                     log.lhs.idx, log.dest.idx));
  }
}


/* Logical (immediate) */

#define VALIDATE_BITWISE_IMMS()                                                \
  invariant_assert(UNS_FITS_IN_BITS(log.immr, 6) &&                            \
                       UNS_FITS_IN_BITS(log.imms, 6),                          \
                   "immediate too big for bitwise imm instr");

void aarch64_emit_eor_imm(struct aarch64_emitter *emitter,
                          const struct aarch64_logical_imm log) {
  VALIDATE_BITWISE_IMMS();

  if (IS64(log)) {
    aarch64_emit(emitter,
                 EOR_64_IMM(log.immr, log.imms, log.source.idx, log.dest.idx));
  } else {
    aarch64_emit(emitter,
                 EOR_32_IMM(log.immr, log.imms, log.source.idx, log.dest.idx));
  }
}

void aarch64_emit_eon_imm(struct aarch64_emitter *emitter,
                          const struct aarch64_logical_imm log) {
  VALIDATE_BITWISE_IMMS();

  if (IS64(log)) {
    aarch64_emit(emitter,
                 EON_64_IMM(log.immr, log.imms, log.source.idx, log.dest.idx));
  } else {
    aarch64_emit(emitter,
                 EON_32_IMM(log.immr, log.imms, log.source.idx, log.dest.idx));
  }
}


void aarch64_emit_orr_imm(struct aarch64_emitter *emitter,
                          const struct aarch64_logical_imm log) {
  VALIDATE_BITWISE_IMMS();

  if (IS64(log)) {
    aarch64_emit(emitter,
                 ORR_64_IMM(log.immr, log.imms, log.source.idx, log.dest.idx));
  } else {
    aarch64_emit(emitter,
                 ORR_32_IMM(log.immr, log.imms, log.source.idx, log.dest.idx));
  }
}

void aarch64_emit_orn_imm(struct aarch64_emitter *emitter,
                          const struct aarch64_logical_imm log) {
  VALIDATE_BITWISE_IMMS();

  if (IS64(log)) {
    aarch64_emit(emitter,
                 ORN_64_IMM(log.immr, log.imms, log.source.idx, log.dest.idx));
  } else {
    aarch64_emit(emitter,
                 ORN_32_IMM(log.immr, log.imms, log.source.idx, log.dest.idx));
  }
}

void aarch64_emit_ands_imm(struct aarch64_emitter *emitter,
                           const struct aarch64_logical_imm log) {
  VALIDATE_BITWISE_IMMS();

  if (IS64(log)) {
    aarch64_emit(emitter,
                 ANDS_64_IMM(log.immr, log.imms, log.source.idx, log.dest.idx));
  } else {
    aarch64_emit(emitter,
                 ANDS_32_IMM(log.immr, log.imms, log.source.idx, log.dest.idx));
  }
}

void aarch64_emit_and_imm(struct aarch64_emitter *emitter,
                          const struct aarch64_logical_imm log) {
  VALIDATE_BITWISE_IMMS();

  if (IS64(log)) {
    aarch64_emit(emitter,
                 AND_64_IMM(log.immr, log.imms, log.source.idx, log.dest.idx));
  } else {
    aarch64_emit(emitter,
                 AND_32_IMM(log.immr, log.imms, log.source.idx, log.dest.idx));
  }
}

/* Add & subtract (register) */

void aarch64_emit_sub(struct aarch64_emitter *emitter,
                      const struct aarch64_addsub_reg sub) {
  if (IS64(sub)) {
    aarch64_emit(emitter, SUB_64_REG(sub.shift, sub.imm6, sub.rhs.idx,
                                     sub.lhs.idx, sub.dest.idx));
  } else {
    aarch64_emit(emitter, SUB_32_REG(sub.shift, sub.imm6, sub.rhs.idx,
                                     sub.lhs.idx, sub.dest.idx));
  }
}

void aarch64_emit_subs(struct aarch64_emitter *emitter,
                       const struct aarch64_addsub_reg sub) {
  if (IS64(sub)) {
    aarch64_emit(emitter, SUBS_64_REG(sub.shift, sub.imm6, sub.rhs.idx,
                                      sub.lhs.idx, sub.dest.idx));
  } else if (IS32(sub)) {
    aarch64_emit(emitter, SUBS_32_REG(sub.shift, sub.imm6, sub.rhs.idx,
                                      sub.lhs.idx, sub.dest.idx));
  } else {
    bad_instr();
  }
}
void aarch64_emit_add(struct aarch64_emitter *emitter,
                      const struct aarch64_addsub_reg sub) {
  if (IS64(sub)) {
    aarch64_emit(emitter, ADD_64_REG(sub.shift, sub.imm6, sub.rhs.idx,
                                     sub.lhs.idx, sub.dest.idx));
  } else if (IS32(sub)) {
    aarch64_emit(emitter, ADD_32_REG(sub.shift, sub.imm6, sub.rhs.idx,
                                     sub.lhs.idx, sub.dest.idx));
  } else {
    bad_instr();
  }
}

void aarch64_emit_adds(struct aarch64_emitter *emitter,
                       const struct aarch64_addsub_reg sub) {
  if (IS64(sub)) {
    aarch64_emit(emitter, ADDS_64_REG(sub.shift, sub.imm6, sub.rhs.idx,
                                      sub.lhs.idx, sub.dest.idx));
  } else if (IS32(sub)) {
    aarch64_emit(emitter, ADDS_32_REG(sub.shift, sub.imm6, sub.rhs.idx,
                                      sub.lhs.idx, sub.dest.idx));
  }

  bad_instr();
}

/* Addressing (immediate) */

void aarch64_emit_adr(struct aarch64_emitter *emitter,
                      const struct aarch64_addr_imm addr) {
  unsigned immlo = CLAMP_BITS(addr.imm & 0b11, 2);
  unsigned immhi = CLAMP_BITS((unsigned int)addr.imm >> 2, 19);

  aarch64_emit(emitter, ADR(immlo, immhi, addr.dest.idx));
}

void aarch64_emit_adrp(struct aarch64_emitter *emitter,
                       const struct aarch64_addr_imm addr) {
  int immlo = addr.imm & 0b11;
  int immhi = addr.imm >> 2;

  aarch64_emit(emitter, ADRP(immlo, immhi, addr.dest.idx));
}

/* Add & subtract (immediate) */

void aarch64_emit_sub_imm(struct aarch64_emitter *emitter,
                          const struct aarch64_addsub_imm sub) {
  if (IS64(sub)) {
    aarch64_emit(emitter,
                 SUB_64_IMM(sub.shift, sub.imm, sub.source.idx, sub.dest.idx));
  } else {
    aarch64_emit(emitter,
                 SUB_32_IMM(sub.shift, sub.imm, sub.source.idx, sub.dest.idx));
  }
}

void aarch64_emit_subs_imm(struct aarch64_emitter *emitter,
                          const struct aarch64_addsub_imm sub) {
  if (IS64(sub)) {
    aarch64_emit(emitter,
                 SUBS_64_IMM(sub.shift, sub.imm, sub.source.idx, sub.dest.idx));
  } else {
    aarch64_emit(emitter,
                 SUBS_32_IMM(sub.shift, sub.imm, sub.source.idx, sub.dest.idx));
  }
}

void aarch64_emit_add_imm(struct aarch64_emitter *emitter,
                          const struct aarch64_addsub_imm add) {
  if (IS64(add)) {
    aarch64_emit(emitter,
                 ADD_64_IMM(add.shift, add.imm, add.source.idx, add.dest.idx));
  } else {
    aarch64_emit(emitter,
                 ADD_32_IMM(add.shift, add.imm, add.source.idx, add.dest.idx));
  }
}

void aarch64_emit_adds_imm(struct aarch64_emitter *emitter,
                          const struct aarch64_addsub_imm add) {
  if (IS64(add)) {
    aarch64_emit(emitter,
                 ADDS_64_IMM(add.shift, add.imm, add.source.idx, add.dest.idx));
  } else {
    aarch64_emit(emitter,
                 ADDS_32_IMM(add.shift, add.imm, add.source.idx, add.dest.idx));
  }
}

/* Multiply & multiply-add */

void aarch64_emit_madd(struct aarch64_emitter *emitter,
                       const struct aarch64_fma fma) {
  if (IS64(fma)) {
    aarch64_emit(emitter, MADD_64(fma.rhs.idx, fma.addsub.idx, fma.lhs.idx,
                                  fma.dest.idx));
  } else {
    aarch64_emit(emitter, MADD_32(fma.rhs.idx, fma.addsub.idx, fma.lhs.idx,
                                  fma.dest.idx));
  }
}

void aarch64_emit_msub(struct aarch64_emitter *emitter,
                       const struct aarch64_fma fma) {
  if (IS64(fma)) {
    aarch64_emit(emitter, MSUB_64(fma.rhs.idx, fma.addsub.idx, fma.lhs.idx,
                                  fma.dest.idx));
  } else {
    aarch64_emit(emitter, MSUB_32(fma.rhs.idx, fma.addsub.idx, fma.lhs.idx,
                                  fma.dest.idx));
  }
}

/* Shifts and rotates */

void aarch64_emit_lslv(struct aarch64_emitter *emitter,
                          const struct aarch64_reg_2_source shift) {
  if (IS64(shift)) {
    aarch64_emit(emitter,
                 LSLV_64(shift.rhs.idx, shift.lhs.idx, shift.dest.idx));
  } else {
    aarch64_emit(emitter,
                 LSLV_32(shift.rhs.idx, shift.lhs.idx, shift.dest.idx));
  }
}

void aarch64_emit_lsrv(struct aarch64_emitter *emitter,
                          const struct aarch64_reg_2_source shift) {
  if (IS64(shift)) {
    aarch64_emit(emitter,
                 LSRV_64(shift.rhs.idx, shift.lhs.idx, shift.dest.idx));
  } else {
    aarch64_emit(emitter,
                 LSRV_32(shift.rhs.idx, shift.lhs.idx, shift.dest.idx));
  }
}

void aarch64_emit_asrv(struct aarch64_emitter *emitter,
                          const struct aarch64_reg_2_source shift) {
  if (IS64(shift)) {
    aarch64_emit(emitter,
                 ASRV_64(shift.rhs.idx, shift.lhs.idx, shift.dest.idx));
  } else {
    aarch64_emit(emitter,
                 ASRV_32(shift.rhs.idx, shift.lhs.idx, shift.dest.idx));
  }
}

void aarch64_emit_rorv(struct aarch64_emitter *emitter,
                          const struct aarch64_reg_2_source shift) {
  if (IS64(shift)) {
    aarch64_emit(emitter,
                 RORV_64(shift.rhs.idx, shift.lhs.idx, shift.dest.idx));
  } else {
    aarch64_emit(emitter,
                 RORV_32(shift.rhs.idx, shift.lhs.idx, shift.dest.idx));
  }
}

/* Division */

void aarch64_emit_sdiv(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source div) {
  if (IS64(div)) {
    aarch64_emit(emitter, SDIV_64(div.rhs.idx, div.lhs.idx, div.dest.idx));
  } else {
    aarch64_emit(emitter, SDIV_32(div.rhs.idx, div.lhs.idx, div.dest.idx));
  }
}

void aarch64_emit_udiv(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source div) {
  if (IS64(div)) {
    aarch64_emit(emitter, UDIV_64(div.rhs.idx, div.lhs.idx, div.dest.idx));
  } else {
    aarch64_emit(emitter, UDIV_32(div.rhs.idx, div.lhs.idx, div.dest.idx));
  }
}

/* Loads and stores */

void aarch64_emit_load_pair_imm(struct aarch64_emitter *emitter,
                                const struct aarch64_load_pair_imm ldp) {
  switch (ldp.mode) {
  case AARCH64_ADDRESSING_MODE_OFFSET:
    if (IS64_REG(ldp.dest[0])) {
      aarch64_emit(emitter, LDP_OFFSET_64(ldp.imm, ldp.dest[1].idx,
                                          ldp.addr.idx, ldp.dest[0].idx));
    } else {
      aarch64_emit(emitter, LDP_OFFSET_32(ldp.imm, ldp.dest[1].idx,
                                          ldp.addr.idx, ldp.dest[0].idx));
    }
    break;
  case AARCH64_ADDRESSING_MODE_PREINDEX:
    if (IS64_REG(ldp.dest[0])) {
      aarch64_emit(emitter, LDP_PRE_INDEX_64(ldp.imm, ldp.dest[1].idx,
                                             ldp.addr.idx, ldp.dest[0].idx));
    } else {
      aarch64_emit(emitter, LDP_PRE_INDEX_32(ldp.imm, ldp.dest[1].idx,
                                             ldp.addr.idx, ldp.dest[0].idx));
    }
    break;
  case AARCH64_ADDRESSING_MODE_POSTINDEX:
    if (IS64_REG(ldp.dest[0])) {
      aarch64_emit(emitter, LDP_POST_INDEX_64(ldp.imm, ldp.dest[1].idx,
                                              ldp.addr.idx, ldp.dest[0].idx));
    } else {
      aarch64_emit(emitter, LDP_POST_INDEX_32(ldp.imm, ldp.dest[1].idx,
                                              ldp.addr.idx, ldp.dest[0].idx));
    }
    break;
  }
}

void aarch64_emit_store_pair_imm(struct aarch64_emitter *emitter,
                                 const struct aarch64_store_pair_imm stp) {
  switch (stp.mode) {
  case AARCH64_ADDRESSING_MODE_OFFSET:
    if (IS64_REG(stp.source[0])) {
      aarch64_emit(emitter, STP_OFFSET_64(stp.imm, stp.source[1].idx,
                                          stp.addr.idx, stp.source[0].idx));
    } else {
      aarch64_emit(emitter, STP_OFFSET_32(stp.imm, stp.source[1].idx,
                                          stp.addr.idx, stp.source[0].idx));
    }
    break;
  case AARCH64_ADDRESSING_MODE_PREINDEX:
    if (IS64_REG(stp.source[0])) {
      aarch64_emit(emitter, STP_PRE_INDEX_64(stp.imm, stp.source[1].idx,
                                             stp.addr.idx, stp.source[0].idx));
    } else {
      aarch64_emit(emitter, STP_PRE_INDEX_32(stp.imm, stp.source[1].idx,
                                             stp.addr.idx, stp.source[0].idx));
    }
    break;
  case AARCH64_ADDRESSING_MODE_POSTINDEX:
    if (IS64_REG(stp.source[0])) {
      aarch64_emit(emitter, STP_POST_INDEX_64(stp.imm, stp.source[1].idx,
                                              stp.addr.idx, stp.source[0].idx));
    } else {
      aarch64_emit(emitter, STP_POST_INDEX_32(stp.imm, stp.source[1].idx,
                                              stp.addr.idx, stp.source[0].idx));
    }
    break;
  }
}

void aarch64_emit_load_imm(struct aarch64_emitter *emitter,
                           const struct aarch64_load_imm ldr) {
  switch (ldr.mode) {
  case AARCH64_ADDRESSING_MODE_OFFSET:
    if (IS64(ldr)) {
      aarch64_emit(emitter,
                   LDR_64_IMM_UNSIGNED(ldr.imm, ldr.addr.idx, ldr.dest.idx));
    } else if (ISDBL(ldr)) {
      aarch64_emit(emitter,
                   LDR_FP_64_IMM_UNSIGNED(ldr.imm, ldr.addr.idx, ldr.dest.idx));
    } else if (ISFLT(ldr)) {
      aarch64_emit(emitter,
                   LDR_FP_32_IMM_UNSIGNED(ldr.imm, ldr.addr.idx, ldr.dest.idx));
    } else {
      aarch64_emit(emitter,
                   LDR_32_IMM_UNSIGNED(ldr.imm, ldr.addr.idx, ldr.dest.idx));
    }
    break;
  case AARCH64_ADDRESSING_MODE_PREINDEX:
    todo("preindex single loads");
    break;
  case AARCH64_ADDRESSING_MODE_POSTINDEX:
    todo("postindex single loads");
    break;
  }
}

void aarch64_emit_store_imm(struct aarch64_emitter *emitter,
                            const struct aarch64_store_imm str) {
  switch (str.mode) {
  case AARCH64_ADDRESSING_MODE_OFFSET:
    if (IS64_REG(str.source)) {
      aarch64_emit(emitter,
                   STR_64_IMM_UNSIGNED(str.imm, str.addr.idx, str.source.idx));
    } else if (ISDBL_REG(str.source)) {
      aarch64_emit(emitter,
                   STR_FP_64_IMM_UNSIGNED(str.imm, str.addr.idx, str.source.idx));
    } else if (ISFLT_REG(str.source)) {
      aarch64_emit(emitter,
                   STR_FP_32_IMM_UNSIGNED(str.imm, str.addr.idx, str.source.idx));
    } else {
      aarch64_emit(emitter,
                   STR_32_IMM_UNSIGNED(str.imm, str.addr.idx, str.source.idx));
    }
    break;
  case AARCH64_ADDRESSING_MODE_PREINDEX:
    todo("preindex single stores");
    break;
  case AARCH64_ADDRESSING_MODE_POSTINDEX:
    todo("postindex single stores");
    break;
  }
}

/* Conditional selects */

void aarch64_emit_csel(struct aarch64_emitter *emitter,
                       const struct aarch64_conditional_select select) {
  if (IS64(select)) {
    aarch64_emit(emitter, CSEL_64(select.false_source.idx, select.cond,
                                  select.true_source.idx, select.dest.idx));
  } else {
    aarch64_emit(emitter, CSEL_32(select.false_source.idx, select.cond,
                                  select.true_source.idx, select.dest.idx));
  }
}

void aarch64_emit_csinc(struct aarch64_emitter *emitter,
                        const struct aarch64_conditional_select select) {
  if (IS64(select)) {
    aarch64_emit(emitter, CSINC_64(select.false_source.idx, select.cond,
                                   select.true_source.idx, select.dest.idx));
  } else {
    aarch64_emit(emitter, CSINC_32(select.false_source.idx, select.cond,
                                   select.true_source.idx, select.dest.idx));
  }
}

void aarch64_emit_csinv(struct aarch64_emitter *emitter,
                        const struct aarch64_conditional_select select) {
  if (IS64(select)) {
    aarch64_emit(emitter, CSINV_64(select.false_source.idx, select.cond,
                                   select.true_source.idx, select.dest.idx));
  } else {
    aarch64_emit(emitter, CSINV_32(select.false_source.idx, select.cond,
                                   select.true_source.idx, select.dest.idx));
  }
}

void aarch64_emit_csneg(struct aarch64_emitter *emitter,
                        const struct aarch64_conditional_select select) {
  if (IS64(select)) {
    aarch64_emit(emitter, CSNEG_64(select.false_source.idx, select.cond,
                                   select.true_source.idx, select.dest.idx));
  } else {
    aarch64_emit(emitter, CSNEG_32(select.false_source.idx, select.cond,
                                   select.true_source.idx, select.dest.idx));
  }
}

/* Branches */

// NOTE: this relies on sequential ids

void aarch64_emit_b_cond(struct aarch64_emitter *emitter,
                         const struct aarch64_conditional_branch br) {
  signed long long cur_pos = aarch64_emitted_count(emitter);
  signed long long target_pos = br.target->first_instr->id;

  signed offset = target_pos - cur_pos;

  invariant_assert(SIG_FITS_IN_BITS(offset, 19),
                   "offset too big for branch instruction!");
  aarch64_emit(emitter, B_COND(CLAMP_BITS(offset, 19), br.cond));
}

void aarch64_emit_bc_cond(struct aarch64_emitter *emitter,
                          const struct aarch64_conditional_branch br) {
  signed long long cur_pos = aarch64_emitted_count(emitter);
  signed long long target_pos = br.target->first_instr->id;

  signed offset = target_pos - cur_pos;

  invariant_assert(SIG_FITS_IN_BITS(offset, 19),
                   "offset too big for branch instruction!");
  aarch64_emit(emitter, BC_COND(CLAMP_BITS(offset, 19), br.cond));
}

void aarch64_emit_b(struct aarch64_emitter *emitter,
                    const struct aarch64_branch br) {
  signed long long cur_pos = aarch64_emitted_count(emitter);
  signed long long target_pos = br.target->first_instr->id;

  signed offset = target_pos - cur_pos;

  invariant_assert(SIG_FITS_IN_BITS(offset, 26),
                   "offset too big for branch instruction!");
  aarch64_emit(emitter, B(CLAMP_BITS(offset, 26)));
}

void aarch64_emit_bl(struct aarch64_emitter *emitter,
                     const struct aarch64_branch br) {

  signed offset = 0;
  if (br.target) {
    signed long long cur_pos = aarch64_emitted_count(emitter);
    signed long long target_pos = br.target->first_instr->id;

    offset = target_pos - cur_pos;
  }

  invariant_assert(SIG_FITS_IN_BITS(offset, 26),
                   "offset too big for branch instruction!");
  aarch64_emit(emitter, BL(CLAMP_BITS(offset, 26)));
}

void aarch64_emit_cbz(struct aarch64_emitter *emitter,
                      const struct aarch64_compare_and_branch cmp) {
  signed long long cur_pos = aarch64_emitted_count(emitter);
  signed long long target_pos = cmp.target->first_instr->id;

  signed offset = target_pos - cur_pos;

  invariant_assert(SIG_FITS_IN_BITS(offset, 19),
                   "offset too big for branch instruction!");

  if (IS64_REG(cmp.cmp)) {
    aarch64_emit(emitter, CBZ_64_IMM(CLAMP_BITS(offset, 19), cmp.cmp.idx));
  } else {
    aarch64_emit(emitter, CBZ_32_IMM(CLAMP_BITS(offset, 19), cmp.cmp.idx));
  }
}

void aarch64_emit_cbnz(struct aarch64_emitter *emitter,
                       const struct aarch64_compare_and_branch cmp) {
  signed long long cur_pos = aarch64_emitted_count(emitter);
  signed long long target_pos = cmp.target->first_instr->id;

  signed offset = target_pos - cur_pos;

  invariant_assert(SIG_FITS_IN_BITS(offset, 19),
                   "offset too big for branch instruction!");

  if (IS64_REG(cmp.cmp)) {
    aarch64_emit(emitter, CBNZ_64_IMM(CLAMP_BITS(offset, 19), cmp.cmp.idx));
  } else {
    aarch64_emit(emitter, CBNZ_32_IMM(CLAMP_BITS(offset, 19), cmp.cmp.idx));
  }
}

void aarch64_emit_ret(struct aarch64_emitter *emitter,
                      const struct aarch64_ret ret) {
  aarch64_emit(emitter, RET(ret.target.idx));
}
