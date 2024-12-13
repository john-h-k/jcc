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

#define ISQUAD(d) (d).dest.ty == AARCH64_REG_TY_Q
#define ISQUAD_REG(d) (d).ty == AARCH64_REG_TY_Q
#define ISDBL(d) (d).dest.ty == AARCH64_REG_TY_D
#define ISDBL_REG(d) (d).ty == AARCH64_REG_TY_D
#define ISFLT(d) (d).dest.ty == AARCH64_REG_TY_S
#define ISFLT_REG(d) (d).ty == AARCH64_REG_TY_S
#define ISHLF(d) (d).dest.ty == AARCH64_REG_TY_H
#define ISHLF_REG(d) (d).ty == AARCH64_REG_TY_H

#define SF_FOR_REG(r)                                                          \
  (r).ty == AARCH64_REG_TY_X                                                   \
      ? SF_64                                                                  \
      : (DEBUG_ASSERT((r).ty == AARCH64_REG_TY_W,                              \
                      "SF_FOR_REG with non {X, W} register"),                  \
         SF_32)

#define FTYPE_FOR_REG(r)                                                       \
  (r).ty == AARCH64_REG_TY_D                                                   \
      ? FTYPE_DOUBLE                                                           \
      : ((r).ty == AARCH64_REG_TY_S                                            \
             ? FTYPE_SINGLE                                                    \
             : (DEBUG_ASSERT((r).ty == AARCH64_REG_TY_H,                       \
                             "FTYPE_FOR_REG with non {D, S, H} register"),     \
                FTYPE_HALF))

static void bad_instr(void) {
  BUG("register types or arguments did not make sense");
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

static void aarch64_emit_instr(struct aarch64_emitter *emitter,
                               uint32_t instr) {
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
  aarch64_emit_instr(emitter, NOP);
}

/* Single reg FP data processing */

void aarch64_emit_scvtf(struct aarch64_emitter *emitter,
                        const struct aarch64_reg_1_source scvtf) {
  struct aarch64_reg dest = scvtf.dest;
  struct aarch64_reg source = scvtf.source;

  if (source.ty == AARCH64_REG_TY_H) {
    if (dest.ty == AARCH64_REG_TY_W) {
      aarch64_emit_instr(emitter, FCVTZS_H_TO_32(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_X) {
      aarch64_emit_instr(emitter, FCVTZS_H_TO_64(source.idx, dest.idx));
      return;
    }
  } else if (source.ty == AARCH64_REG_TY_S) {
    if (dest.ty == AARCH64_REG_TY_W) {
      aarch64_emit_instr(emitter, FCVTZS_S_TO_32(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_X) {
      aarch64_emit_instr(emitter, FCVTZS_S_TO_64(source.idx, dest.idx));
      return;
    }
  } else if (source.ty == AARCH64_REG_TY_D) {
    if (dest.ty == AARCH64_REG_TY_W) {
      aarch64_emit_instr(emitter, FCVTZS_D_TO_32(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_X) {
      aarch64_emit_instr(emitter, FCVTZS_D_TO_64(source.idx, dest.idx));
      return;
    }
  } else if (source.ty == AARCH64_REG_TY_W) {
    if (dest.ty == AARCH64_REG_TY_H) {
      aarch64_emit_instr(emitter, SCVTF_32_TO_H(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_S) {
      aarch64_emit_instr(emitter, SCVTF_32_TO_S(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_D) {
      aarch64_emit_instr(emitter, SCVTF_32_TO_D(source.idx, dest.idx));
      return;
    }
  } else if (source.ty == AARCH64_REG_TY_X) {
    if (dest.ty == AARCH64_REG_TY_H) {
      aarch64_emit_instr(emitter, SCVTF_64_TO_H(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_S) {
      aarch64_emit_instr(emitter, SCVTF_64_TO_S(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_D) {
      aarch64_emit_instr(emitter, SCVTF_64_TO_D(source.idx, dest.idx));
      return;
    }
  }

  bad_instr();
}

void aarch64_emit_ucvtf(struct aarch64_emitter *emitter,
                        const struct aarch64_reg_1_source ucvtf) {
  struct aarch64_reg dest = ucvtf.dest;
  struct aarch64_reg source = ucvtf.source;

  if (source.ty == AARCH64_REG_TY_H) {
    if (dest.ty == AARCH64_REG_TY_W) {
      aarch64_emit_instr(emitter, FCVTZU_H_TO_32(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_X) {
      aarch64_emit_instr(emitter, FCVTZU_H_TO_64(source.idx, dest.idx));
      return;
    }
  } else if (source.ty == AARCH64_REG_TY_S) {
    if (dest.ty == AARCH64_REG_TY_W) {
      aarch64_emit_instr(emitter, FCVTZU_S_TO_32(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_X) {
      aarch64_emit_instr(emitter, FCVTZU_S_TO_64(source.idx, dest.idx));
      return;
    }
  } else if (source.ty == AARCH64_REG_TY_D) {
    if (dest.ty == AARCH64_REG_TY_W) {
      aarch64_emit_instr(emitter, FCVTZU_D_TO_32(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_X) {
      aarch64_emit_instr(emitter, FCVTZU_D_TO_64(source.idx, dest.idx));
      return;
    }
  } else if (source.ty == AARCH64_REG_TY_W) {
    if (dest.ty == AARCH64_REG_TY_H) {
      aarch64_emit_instr(emitter, UCVTF_32_TO_H(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_S) {
      aarch64_emit_instr(emitter, UCVTF_32_TO_S(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_D) {
      aarch64_emit_instr(emitter, UCVTF_32_TO_D(source.idx, dest.idx));
      return;
    }
  } else if (source.ty == AARCH64_REG_TY_X) {
    if (dest.ty == AARCH64_REG_TY_H) {
      aarch64_emit_instr(emitter, UCVTF_64_TO_H(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_S) {
      aarch64_emit_instr(emitter, UCVTF_64_TO_S(source.idx, dest.idx));
      return;
    } else if (dest.ty == AARCH64_REG_TY_D) {
      aarch64_emit_instr(emitter, UCVTF_64_TO_D(source.idx, dest.idx));
      return;
    }
  }

  bad_instr();
}

void aarch64_emit_fcvt(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_1_source fcvt) {
  struct aarch64_reg dest = fcvt.dest;
  struct aarch64_reg source = fcvt.source;

  aarch64_emit_instr(emitter, FCVT(FTYPE_FOR_REG(dest), FTYPE_FOR_REG(source),
                                   source.idx, dest.idx));
}

void aarch64_emit_fmov(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_1_source fmov) {
  struct aarch64_reg dest = fmov.dest;
  struct aarch64_reg source = fmov.source;

  if (dest.ty == AARCH64_REG_TY_H) {
    if (source.ty == AARCH64_REG_TY_W) {
      aarch64_emit_instr(emitter, FMOV_32_TO_H(source.idx, dest.idx));
      return;
    } else if (source.ty == AARCH64_REG_TY_X) {
      aarch64_emit_instr(emitter, FMOV_64_TO_H(source.idx, dest.idx));
      return;
    } else if (source.ty == AARCH64_REG_TY_H) {
      aarch64_emit_instr(emitter, FMOV_H(source.idx, dest.idx));
      return;
    }
  } else if (dest.ty == AARCH64_REG_TY_S) {
    if (source.ty == AARCH64_REG_TY_W) {
      aarch64_emit_instr(emitter, FMOV_32_TO_S(source.idx, dest.idx));
      return;
    } else if (source.ty == AARCH64_REG_TY_X) {
      aarch64_emit_instr(emitter, FMOV_64_TO_S(source.idx, dest.idx));
      return;
    } else if (source.ty == AARCH64_REG_TY_S) {
      aarch64_emit_instr(emitter, FMOV_S(source.idx, dest.idx));
      return;
    }
  } else if (dest.ty == AARCH64_REG_TY_D) {
    if (source.ty == AARCH64_REG_TY_W) {
      aarch64_emit_instr(emitter, FMOV_32_TO_D(source.idx, dest.idx));
      return;
    } else if (source.ty == AARCH64_REG_TY_X) {
      aarch64_emit_instr(emitter, FMOV_64_TO_D(source.idx, dest.idx));
      return;
    } else if (source.ty == AARCH64_REG_TY_D) {
      aarch64_emit_instr(emitter, FMOV_D(source.idx, dest.idx));
      return;
    }
  }

  bad_instr();
}

void aarch64_emit_fneg(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_1_source fneg) {
  aarch64_emit_instr(
      emitter, FNEG(FTYPE_FOR_REG(fneg.dest), fneg.source.idx, fneg.dest.idx));
}

void aarch64_emit_fsqrt(struct aarch64_emitter *emitter,
                        const struct aarch64_reg_1_source fsqrt) {
  aarch64_emit_instr(emitter, FSQRT(FTYPE_FOR_REG(fsqrt.dest), fsqrt.source.idx,
                                    fsqrt.dest.idx));
}

void aarch64_emit_fabs(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_1_source fabs) {
  aarch64_emit_instr(
      emitter, FABS(FTYPE_FOR_REG(fabs.dest), fabs.source.idx, fabs.dest.idx));
}

void aarch64_emit_fabs(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_1_source fabs);

void aarch64_emit_fcmp(struct aarch64_emitter *emitter,
                       const struct aarch64_fcmp fcmp) {
  aarch64_emit_instr(emitter, FCMP(FTYPE_FOR_REG(fcmp.lhs), fcmp.rhs.idx, fcmp.lhs.idx)));
}

void aarch64_emit_fcmp_zero(struct aarch64_emitter *emitter,
                            const struct aarch64_fcmp_zero fcmp_zero) {
  aarch64_emit_instr(emitter, FCMP_ZERO(FTYPE_FOR_REG(fcmp_zero.lhs), fcmp_zero.lhs.idx)));
}

/* Two reg FP data processing */

void aarch64_emit_fadd(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source fadd) {
  aarch64_emit_instr(emitter, FADD(FTYPE_FOR_REG(fadd.dest), fadd.rhs.idx,
                                   fadd.lhs.idx, fadd.dest.idx));
}

void aarch64_emit_fsub(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source fsub) {
  aarch64_emit_instr(emitter, FSUB(FTYPE_FOR_REG(fsub.dest), fsub.rhs.idx,
                                   fsub.lhs.idx, fsub.dest.idx));
}

void aarch64_emit_fmul(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source fmul) {
  aarch64_emit_instr(emitter, FMUL(FTYPE_FOR_REG(fmul.dest), fmul.rhs.idx,
                                   fmul.lhs.idx, fmul.dest.idx));
}

void aarch64_emit_fdiv(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source fdiv) {
  aarch64_emit_instr(emitter, FDIV(FTYPE_FOR_REG(fdiv.dest), fdiv.rhs.idx,
                                   fdiv.lhs.idx, fdiv.dest.idx));
}

void aarch64_emit_fmaxnm(struct aarch64_emitter *emitter,
                         const struct aarch64_reg_2_source fmax) {
  aarch64_emit_instr(emitter, FMAXNM(FTYPE_FOR_REG(fmax.dest), fmax.rhs.idx,
                                     fmax.lhs.idx, fmax.dest.idx));
}
void aarch64_emit_fminnm(struct aarch64_emitter *emitter,
                         const struct aarch64_reg_2_source fmin) {
  aarch64_emit_instr(emitter, FMINNM(FTYPE_FOR_REG(fmin.dest), fmin.rhs.idx,
                                     fmin.lhs.idx, fmin.dest.idx));
}

/* Register moves */

void aarch64_emit_movz(struct aarch64_emitter *emitter,
                       const struct aarch64_mov_imm movz) {
  aarch64_emit_instr(emitter, MOVZ(SF_FOR_REG(movz.dest), movz.shift, movz.imm,
                                   movz.dest.idx));
}

void aarch64_emit_movk(struct aarch64_emitter *emitter,
                       const struct aarch64_mov_imm movk) {
  aarch64_emit_instr(emitter, MOVK(SF_FOR_REG(movk.dest), movk.shift, movk.imm,
                                   movk.dest.idx));
}

void aarch64_emit_movn(struct aarch64_emitter *emitter,
                       const struct aarch64_mov_imm movn) {
  aarch64_emit_instr(emitter, MOVN(SF_FOR_REG(movn.dest), movn.shift, movn.imm,
                                   movn.dest.idx));
}

/* Bitfield operations (Immediate) */

void aarch64_emit_sbfm(struct aarch64_emitter *emitter,
                       const struct aarch64_bitfield bf) {
  aarch64_emit_instr(
      emitter, SBFM_IMM(0b1, bf.immr, bf.imms, bf.source.idx, bf.dest.idx));
}

void aarch64_emit_bfm(struct aarch64_emitter *emitter,
                      const struct aarch64_bitfield bf) {
  aarch64_emit_instr(emitter, BFM_IMM(SF_FOR_REG(bf.dest), bf.immr, bf.imms,
                                      bf.source.idx, bf.dest.idx));
}

void aarch64_emit_ubfm(struct aarch64_emitter *emitter,
                       const struct aarch64_bitfield bf) {
  aarch64_emit_instr(emitter, UBFM_IMM(SF_FOR_REG(bf.dest), bf.immr, bf.imms,
                                       bf.source.idx, bf.dest.idx));
}

/* Logical (register) */

void aarch64_emit_and(struct aarch64_emitter *emitter,
                      const struct aarch64_logical_reg log) {
  aarch64_emit_instr(emitter,
                     AND_REG(SF_FOR_REG(log.dest), log.shift, log.rhs.idx,
                             log.imm6, log.lhs.idx, log.dest.idx));
}

void aarch64_emit_ands(struct aarch64_emitter *emitter,
                       const struct aarch64_logical_reg log) {
  aarch64_emit_instr(emitter,
                     ANDS_REG(SF_FOR_REG(log.dest), log.shift, log.rhs.idx,
                              log.imm6, log.lhs.idx, log.dest.idx));
}

void aarch64_emit_eor(struct aarch64_emitter *emitter,
                      const struct aarch64_logical_reg log) {
  aarch64_emit_instr(emitter,
                     EOR_REG(SF_FOR_REG(log.dest), log.shift, log.rhs.idx,
                             log.imm6, log.lhs.idx, log.dest.idx));
}

void aarch64_emit_eon(struct aarch64_emitter *emitter,
                      const struct aarch64_logical_reg log) {
  aarch64_emit_instr(emitter,
                     EON_REG(SF_FOR_REG(log.dest), log.shift, log.rhs.idx,
                             log.imm6, log.lhs.idx, log.dest.idx));
}

void aarch64_emit_orr(struct aarch64_emitter *emitter,
                      const struct aarch64_logical_reg log) {
  aarch64_emit_instr(emitter,
                     ORR_REG(SF_FOR_REG(log.dest), log.shift, log.rhs.idx,
                             log.imm6, log.lhs.idx, log.dest.idx));
}

void aarch64_emit_orn(struct aarch64_emitter *emitter,
                      const struct aarch64_logical_reg log) {
  aarch64_emit_instr(emitter,
                     ORN_REG(SF_FOR_REG(log.dest), log.shift, log.rhs.idx,
                             log.imm6, log.lhs.idx, log.dest.idx));
}

/* Logical (immediate) */

void aarch64_emit_eor_imm(struct aarch64_emitter *emitter,
                          const struct aarch64_logical_imm log) {
  aarch64_emit_instr(emitter, EOR_IMM(SF_FOR_REG(log.dest), log.n, log.immr,
                                      log.imms, log.source.idx, log.dest.idx));
}

void aarch64_emit_orr_imm(struct aarch64_emitter *emitter,
                          const struct aarch64_logical_imm log) {
  aarch64_emit_instr(emitter, ORR_IMM(SF_FOR_REG(log.dest), log.n, log.immr,
                                      log.imms, log.source.idx, log.dest.idx));
}

void aarch64_emit_ands_imm(struct aarch64_emitter *emitter,
                           const struct aarch64_logical_imm log) {
  aarch64_emit_instr(emitter, ANDS_IMM(SF_FOR_REG(log.dest), log.n, log.immr,
                                       log.imms, log.source.idx, log.dest.idx));
}

void aarch64_emit_and_imm(struct aarch64_emitter *emitter,
                          const struct aarch64_logical_imm log) {
  aarch64_emit_instr(emitter, AND_IMM(SF_FOR_REG(log.dest), log.n, log.immr,
                                      log.imms, log.source.idx, log.dest.idx));
}

/* Add & subtract (register) */

void aarch64_emit_sub(struct aarch64_emitter *emitter,
                      const struct aarch64_addsub_reg sub) {
  aarch64_emit_instr(emitter, SUB_REG(SF_FOR_REG(sub.dest), sub.shift, sub.imm6,
                                      sub.rhs.idx, sub.lhs.idx, sub.dest.idx));
}

void aarch64_emit_subs(struct aarch64_emitter *emitter,
                       const struct aarch64_addsub_reg sub) {
  aarch64_emit_instr(emitter,
                     SUBS_REG(SF_FOR_REG(sub.dest), sub.shift, sub.imm6,
                              sub.rhs.idx, sub.lhs.idx, sub.dest.idx));
}
void aarch64_emit_add(struct aarch64_emitter *emitter,
                      const struct aarch64_addsub_reg add) {
  aarch64_emit_instr(emitter, ADD_REG(SF_FOR_REG(add.dest), add.shift, add.imm6,
                                      add.rhs.idx, add.lhs.idx, add.dest.idx));
}

void aarch64_emit_adds(struct aarch64_emitter *emitter,
                       const struct aarch64_addsub_reg add) {
  aarch64_emit_instr(emitter,
                     ADDS_REG(SF_FOR_REG(add.dest), add.shift, add.imm6,
                              add.rhs.idx, add.lhs.idx, add.dest.idx));
}

/* Add & subtract (extended register) */

void aarch64_emit_sub_ext(struct aarch64_emitter *emitter,
                          const struct aarch64_addsub_ext sub_ext) {
  aarch64_emit_instr(emitter, SUB_EXT(SF_FOR_REG(sub_ext.dest), sub_ext.extend,
                                      sub_ext.imm3, sub_ext.rhs.idx,
                                      sub_ext.lhs.idx, sub_ext.dest.idx));
}

void aarch64_emit_subs_ext(struct aarch64_emitter *emitter,
                           const struct aarch64_addsub_ext subs_ext) {
  aarch64_emit_instr(emitter,
                     SUBS_EXT(SF_FOR_REG(subs_ext.dest), subs_ext.extend,
                              subs_ext.imm3, subs_ext.rhs.idx, subs_ext.lhs.idx,
                              subs_ext.dest.idx));
}

void aarch64_emit_add_ext(struct aarch64_emitter *emitter,
                          const struct aarch64_addsub_ext add_ext) {
  aarch64_emit_instr(emitter, ADD_EXT(SF_FOR_REG(add_ext.dest), add_ext.extend,
                                      add_ext.imm3, add_ext.rhs.idx,
                                      add_ext.lhs.idx, add_ext.dest.idx));
}

void aarch64_emit_adds_ext(struct aarch64_emitter *emitter,
                           const struct aarch64_addsub_ext adds_ext) {
  aarch64_emit_instr(emitter,
                     ADDS_EXT(SF_FOR_REG(adds_ext.dest), adds_ext.extend,
                              adds_ext.imm3, adds_ext.rhs.idx, adds_ext.lhs.idx,
                              adds_ext.dest.idx));
}

/* Addressing (immediate) */

void aarch64_emit_adr(struct aarch64_emitter *emitter,
                      const struct aarch64_addr_imm addr) {
  imm_t immlo = addr.imm & 0b11;
  imm_t immhi = addr.imm >> 2;

  aarch64_emit_instr(emitter, ADR(immlo, immhi, addr.dest.idx));
}

void aarch64_emit_adrp(struct aarch64_emitter *emitter,
                       const struct aarch64_addr_imm addr) {
  imm_t immlo = addr.imm & 0b11;
  imm_t immhi = addr.imm >> 2;

  aarch64_emit_instr(emitter, ADRP(immlo, immhi, addr.dest.idx));
}

/* Add & subtract (immediate) */

void aarch64_emit_sub_imm(struct aarch64_emitter *emitter,
                          const struct aarch64_addsub_imm sub) {
  aarch64_emit_instr(emitter, SUB_IMM(SF_FOR_REG(sub.dest), sub.shift, sub.imm,
                                      sub.source.idx, sub.dest.idx));
}

void aarch64_emit_subs_imm(struct aarch64_emitter *emitter,
                           const struct aarch64_addsub_imm sub) {
  aarch64_emit_instr(emitter, SUBS_IMM(SF_FOR_REG(sub.dest), sub.shift, sub.imm,
                                       sub.source.idx, sub.dest.idx));
}

void aarch64_emit_add_imm(struct aarch64_emitter *emitter,
                          const struct aarch64_addsub_imm add) {
  aarch64_emit_instr(emitter, ADD_IMM(SF_FOR_REG(add.dest), add.shift, add.imm,
                                      add.source.idx, add.dest.idx));
}

void aarch64_emit_adds_imm(struct aarch64_emitter *emitter,
                           const struct aarch64_addsub_imm add) {
  aarch64_emit_instr(emitter, ADDS_IMM(SF_FOR_REG(add.dest), add.shift, add.imm,
                                       add.source.idx, add.dest.idx));
}

/* Multiply & multiply-add */

void aarch64_emit_madd(struct aarch64_emitter *emitter,
                       const struct aarch64_fma fma) {
  aarch64_emit_instr(emitter, MADD(SF_FOR_REG(fma.dest), fma.rhs.idx,
                                   fma.addsub.idx, fma.lhs.idx, fma.dest.idx));
}

void aarch64_emit_msub(struct aarch64_emitter *emitter,
                       const struct aarch64_fma fma) {
  aarch64_emit_instr(emitter, MSUB(SF_FOR_REG(fma.dest), fma.rhs.idx,
                                   fma.addsub.idx, fma.lhs.idx, fma.dest.idx));
}

/* Shifts and rotates */

void aarch64_emit_lslv(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source shift) {
  aarch64_emit_instr(emitter, LSLV(SF_FOR_REG(shift.dest), shift.rhs.idx,
                                   shift.lhs.idx, shift.dest.idx));
}

void aarch64_emit_lsrv(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source shift) {
  aarch64_emit_instr(emitter, LSRV(SF_FOR_REG(shift.dest), shift.rhs.idx,
                                   shift.lhs.idx, shift.dest.idx));
}

void aarch64_emit_asrv(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source shift) {
  aarch64_emit_instr(emitter, ASRV(SF_FOR_REG(shift.dest), shift.rhs.idx,
                                   shift.lhs.idx, shift.dest.idx));
}

void aarch64_emit_rorv(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source shift) {
  aarch64_emit_instr(emitter, RORV(SF_FOR_REG(shift.dest), shift.rhs.idx,
                                   shift.lhs.idx, shift.dest.idx));
}

/* Division */

void aarch64_emit_sdiv(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source div) {
  aarch64_emit_instr(emitter, SDIV(SF_FOR_REG(div.dest), div.rhs.idx,
                                   div.lhs.idx, div.dest.idx));
}

void aarch64_emit_udiv(struct aarch64_emitter *emitter,
                       const struct aarch64_reg_2_source div) {
  aarch64_emit_instr(emitter, UDIV(SF_FOR_REG(div.dest), div.rhs.idx,
                                   div.lhs.idx, div.dest.idx));
}

/* Loads and stores */

void aarch64_emit_load_pair_imm(struct aarch64_emitter *emitter,
                                const struct aarch64_load_pair_imm ldp) {
  if (!ldp.mode) {
    bad_instr();
  }

  size_t size = reg_size(ldp.dest[0].ty);
  unsigned fp = aarch64_reg_ty_is_fp(ldp.dest[0].ty);

  switch (size) {
  case 16:
    aarch64_emit_instr(emitter, LDP_128(ldp.mode, fp, ldp.imm, ldp.dest[1].idx,
                                        ldp.addr.idx, ldp.dest[0].idx));
    break;
  case 8:
    aarch64_emit_instr(emitter, LDP_64(ldp.mode, fp, ldp.imm, ldp.dest[1].idx,
                                       ldp.addr.idx, ldp.dest[0].idx));
    break;
  case 4:
    aarch64_emit_instr(emitter, LDP_32(ldp.mode, fp, ldp.imm, ldp.dest[1].idx,
                                       ldp.addr.idx, ldp.dest[0].idx));
    break;
  default:
    bad_instr();
  }
}

void aarch64_emit_store_pair_imm(struct aarch64_emitter *emitter,
                                 const struct aarch64_store_pair_imm stp) {
  if (!stp.mode) {
    bad_instr();
  }

  size_t size = reg_size(stp.source[0].ty);
  unsigned fp = aarch64_reg_ty_is_fp(stp.source[0].ty);

  switch (size) {
  case 16:
    aarch64_emit_instr(emitter,
                       STP_128(stp.mode, fp, stp.imm, stp.source[1].idx,
                               stp.addr.idx, stp.source[0].idx));
    break;
  case 8:
    aarch64_emit_instr(emitter, STP_64(stp.mode, fp, stp.imm, stp.source[1].idx,
                                       stp.addr.idx, stp.source[0].idx));
    break;
  case 4:
    aarch64_emit_instr(emitter, STP_32(stp.mode, fp, stp.imm, stp.source[1].idx,
                                       stp.addr.idx, stp.source[0].idx));
    break;
  default:
    bad_instr();
  }
}

void aarch64_emit_load_half_imm(struct aarch64_emitter *emitter,
                                const struct aarch64_load_imm ldrh) {
  switch (ldrh.mode) {
  case AARCH64_ADDRESSING_MODE_OFFSET:
    aarch64_emit_instr(
        emitter, LDR_16_IMM_UNSIGNED(ldrh.imm, ldrh.addr.idx, ldrh.dest.idx));
    break;
  case AARCH64_ADDRESSING_MODE_PREINDEX:
    TODO("preindex single loads");
  case AARCH64_ADDRESSING_MODE_POSTINDEX:
    TODO("postindex single loads");
  }
}

void aarch64_emit_store_half_imm(struct aarch64_emitter *emitter,
                                 const struct aarch64_store_imm strh) {
  switch (strh.mode) {
  case AARCH64_ADDRESSING_MODE_OFFSET:
    aarch64_emit_instr(
        emitter, STR_16_IMM_UNSIGNED(strh.imm, strh.addr.idx, strh.source.idx));
    break;
  case AARCH64_ADDRESSING_MODE_PREINDEX:
    TODO("preindex single loads");
  case AARCH64_ADDRESSING_MODE_POSTINDEX:
    TODO("postindex single loads");
  }
}

void aarch64_emit_load_byte_imm(struct aarch64_emitter *emitter,
                                const struct aarch64_load_imm ldrb) {
  switch (ldrb.mode) {
  case AARCH64_ADDRESSING_MODE_OFFSET:
    aarch64_emit_instr(
        emitter, LDR_8_IMM_UNSIGNED(ldrb.imm, ldrb.addr.idx, ldrb.dest.idx));
    break;
  case AARCH64_ADDRESSING_MODE_PREINDEX:
    TODO("preindex single loads");
  case AARCH64_ADDRESSING_MODE_POSTINDEX:
    TODO("postindex single loads");
  }
}

void aarch64_emit_store_byte_imm(struct aarch64_emitter *emitter,
                                 const struct aarch64_store_imm strb) {
  switch (strb.mode) {
  case AARCH64_ADDRESSING_MODE_OFFSET:
    aarch64_emit_instr(
        emitter, STR_8_IMM_UNSIGNED(strb.imm, strb.addr.idx, strb.source.idx));
    break;
  case AARCH64_ADDRESSING_MODE_PREINDEX:
    TODO("preindex single loads");
  case AARCH64_ADDRESSING_MODE_POSTINDEX:
    TODO("postindex single loads");
  }
}

void aarch64_emit_load_imm(struct aarch64_emitter *emitter,
                           const struct aarch64_load_imm ldr) {
  switch (ldr.mode) {
  case AARCH64_ADDRESSING_MODE_OFFSET:
    if (IS64(ldr)) {
      aarch64_emit_instr(
          emitter, LDR_64_IMM_UNSIGNED(ldr.imm, ldr.addr.idx, ldr.dest.idx));
    } else if (ISDBL(ldr)) {
      aarch64_emit_instr(
          emitter, LDR_FP_64_IMM_UNSIGNED(ldr.imm, ldr.addr.idx, ldr.dest.idx));
    } else if (ISFLT(ldr)) {
      aarch64_emit_instr(
          emitter, LDR_FP_32_IMM_UNSIGNED(ldr.imm, ldr.addr.idx, ldr.dest.idx));
    } else if (ISHLF(ldr)) {
      aarch64_emit_instr(
          emitter, LDR_FP_16_IMM_UNSIGNED(ldr.imm, ldr.addr.idx, ldr.dest.idx));
    } else {
      aarch64_emit_instr(
          emitter, LDR_32_IMM_UNSIGNED(ldr.imm, ldr.addr.idx, ldr.dest.idx));
    }
    break;
  case AARCH64_ADDRESSING_MODE_PREINDEX:
    TODO("preindex single loads");
  case AARCH64_ADDRESSING_MODE_POSTINDEX:
    TODO("postindex single loads");
  }
}

void aarch64_emit_store_imm(struct aarch64_emitter *emitter,
                            const struct aarch64_store_imm str) {
  switch (str.mode) {
  case AARCH64_ADDRESSING_MODE_OFFSET:
    if (IS64_REG(str.source)) {
      aarch64_emit_instr(
          emitter, STR_64_IMM_UNSIGNED(str.imm, str.addr.idx, str.source.idx));
    } else if (ISDBL_REG(str.source)) {
      aarch64_emit_instr(emitter, STR_FP_64_IMM_UNSIGNED(str.imm, str.addr.idx,
                                                         str.source.idx));
    } else if (ISFLT_REG(str.source)) {
      aarch64_emit_instr(emitter, STR_FP_32_IMM_UNSIGNED(str.imm, str.addr.idx,
                                                         str.source.idx));
    } else if (ISHLF_REG(str.source)) {
      aarch64_emit_instr(emitter, STR_FP_16_IMM_UNSIGNED(str.imm, str.addr.idx,
                                                         str.source.idx));
    } else {
      aarch64_emit_instr(
          emitter, STR_32_IMM_UNSIGNED(str.imm, str.addr.idx, str.source.idx));
    }
    break;
  case AARCH64_ADDRESSING_MODE_PREINDEX:
    TODO("preindex single stores");
  case AARCH64_ADDRESSING_MODE_POSTINDEX:
    TODO("postindex single stores");
  }
}

/* Conditional selects */

void aarch64_emit_csel(struct aarch64_emitter *emitter,
                       const struct aarch64_conditional_select select) {
  aarch64_emit_instr(emitter, CSEL(SF_FOR_REG(select.dest),
                                   select.false_source.idx, select.cond,
                                   select.true_source.idx, select.dest.idx));
}

void aarch64_emit_csinc(struct aarch64_emitter *emitter,
                        const struct aarch64_conditional_select select) {
  aarch64_emit_instr(emitter, CSINC(SF_FOR_REG(select.dest),
                                    select.false_source.idx, select.cond,
                                    select.true_source.idx, select.dest.idx));
}

void aarch64_emit_csinv(struct aarch64_emitter *emitter,
                        const struct aarch64_conditional_select select) {
  aarch64_emit_instr(emitter, CSINV(SF_FOR_REG(select.dest),
                                    select.false_source.idx, select.cond,
                                    select.true_source.idx, select.dest.idx));
}

void aarch64_emit_csneg(struct aarch64_emitter *emitter,
                        const struct aarch64_conditional_select select) {
  aarch64_emit_instr(emitter, CSNEG(SF_FOR_REG(select.dest),
                                    select.false_source.idx, select.cond,
                                    select.true_source.idx, select.dest.idx));
}

/* Branches */

// NOTE: this relies on sequential ids

void aarch64_emit_b_cond(struct aarch64_emitter *emitter,
                         const struct aarch64_conditional_branch br) {
  signed long long cur_pos = aarch64_emitted_count(emitter);
  signed long long target_pos = br.target->first_instr->id;

  simm_t offset = target_pos - cur_pos;

  aarch64_emit_instr(emitter, B_COND(offset, br.cond));
}

void aarch64_emit_bc_cond(struct aarch64_emitter *emitter,
                          const struct aarch64_conditional_branch br) {
  signed long long cur_pos = aarch64_emitted_count(emitter);
  signed long long target_pos = br.target->first_instr->id;

  simm_t offset = target_pos - cur_pos;

  aarch64_emit_instr(emitter, BC_COND(offset, br.cond));
}

void aarch64_emit_b(struct aarch64_emitter *emitter,
                    const struct aarch64_branch b) {
  signed long long cur_pos = aarch64_emitted_count(emitter);
  signed long long target_pos = b.target->first_instr->id;

  simm_t offset = target_pos - cur_pos;

  aarch64_emit_instr(emitter, B(offset));
}

void aarch64_emit_br(struct aarch64_emitter *emitter,
                     const struct aarch64_branch_reg br) {
  aarch64_emit_instr(emitter, BR(br.target.idx));
}

void aarch64_emit_bl(struct aarch64_emitter *emitter,
                     const struct aarch64_branch bl) {

  simm_t offset = 0;
  if (bl.target) {
    signed long long cur_pos = aarch64_emitted_count(emitter);
    signed long long target_pos = bl.target->first_instr->id;

    offset = target_pos - cur_pos;
  }

  aarch64_emit_instr(emitter, BL(offset));
}

void aarch64_emit_blr(struct aarch64_emitter *emitter,
                      const struct aarch64_branch_reg blr) {
  aarch64_emit_instr(emitter, BLR(blr.target.idx));
}

void aarch64_emit_cbz(struct aarch64_emitter *emitter,
                      const struct aarch64_compare_and_branch cmp) {
  signed long long cur_pos = aarch64_emitted_count(emitter);
  signed long long target_pos = cmp.target->first_instr->id;

  simm_t offset = target_pos - cur_pos;

  aarch64_emit_instr(emitter, CBZ(SF_FOR_REG(cmp.cmp), offset, cmp.cmp.idx));
}

void aarch64_emit_cbnz(struct aarch64_emitter *emitter,
                       const struct aarch64_compare_and_branch cmp) {
  signed long long cur_pos = aarch64_emitted_count(emitter);
  signed long long target_pos = cmp.target->first_instr->id;

  simm_t offset = target_pos - cur_pos;

  aarch64_emit_instr(emitter, CBNZ(SF_FOR_REG(cmp.cmp), offset, cmp.cmp.idx));
}

void aarch64_emit_ret(struct aarch64_emitter *emitter,
                      const struct aarch64_branch_reg ret) {
  aarch64_emit_instr(emitter, RET(ret.target.idx));
}
