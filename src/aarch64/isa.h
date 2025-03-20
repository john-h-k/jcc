#ifndef AARCH64_ISA_H
#define AARCH64_ISA_H

#define FITS_IN_BITS(value, bitc)                                              \
  _Generic((value),                                                            \
      imm_t: (((value) & ~((1ull << (bitc)) - 1ull)) == 0),                    \
      simm_t: ((llabs(((simm_t)value)) & ~((1ll << (bitc)) - 1l)) == 0))

// Constants must be casted to uint32_t to be well-defined behaviour
#define U32(v) ((uint32_t)(v))

#define SF_32 (0b0u)
#define SF_64 (0b1u)

#define FTYPE_SINGLE (0b00u)
#define FTYPE_DOUBLE (0b01u)
#define FTYPE_HALF (0b11u)

#define IMM_ASSERT(imm, expected) imm
// (debug_assert(imm == expected, "imm did not match expected"), imm)
#define IMM(imm, bitc)                                                         \
  (DEBUG_ASSERT(FITS_IN_BITS(imm, bitc), "immediate %lld did not fit!",        \
                (long long)imm),                                               \
   CLAMP_BITS(imm, bitc))

/* Nop */

#define NOP (uint32_t)(0b11010101000000110010000000011111)

/* Loads & Stores */

#define LDR_STR_PAIR(opc, V, mode, L, imm7, Rt2, Rn, Rt)                       \
  (uint32_t)((U32(opc) << 30) | (U32(0b101) << 27) | (U32(V) << 26) |          \
             (U32(mode) << 23) | (U32(L) << 22) | (U32(IMM(imm7, 7)) << 15) |  \
             (U32(Rt2) << 10) | (U32(Rn) << 5) | U32(Rt))

#define STP_32(mode, fp, imm7, Rt2, Rn, Rt)                                    \
  LDR_STR_PAIR(0b00, fp, mode, 0, imm7, Rt2, Rn, Rt)
#define LDP_32(mode, fp, imm7, Rt2, Rn, Rt)                                    \
  LDR_STR_PAIR(0b00, fp, mode, 1, imm7, Rt2, Rn, Rt)
#define STP_64(mode, fp, imm7, Rt2, Rn, Rt)                                    \
  LDR_STR_PAIR((fp ? 0b01 : 0b10), fp, mode, 0, imm7, Rt2, Rn, Rt)
#define LDP_64(mode, fp, imm7, Rt2, Rn, Rt)                                    \
  LDR_STR_PAIR((fp ? 0b01 : 0b10), fp, mode, 1, imm7, Rt2, Rn, Rt)
#define STP_128(mode, fp, imm7, Rt2, Rn, Rt)                                   \
  LDR_STR_PAIR(0b10, fp, mode, 0, imm7, Rt2, Rn, Rt)
#define LDP_128(mode, fp, imm7, Rt2, Rn, Rt)                                   \
  LDR_STR_PAIR(0b10, fp, mode, 1, imm7, Rt2, Rn, Rt)

#define LDR_STR_REG(size, VR, opc, Rm, option, S, Rn, Rt)                      \
  (uint32_t)((U32(size) << 30) | (U32(0b111) << 27) | (U32(VR) << 26) |        \
             (U32(opc) << 22) | (U32(0b1) << 21) | (U32(Rm) << 16) |           \
             (U32(option) << 13) | (U32(S) << 12) | (U32(0b10) << 10) |        \
             (U32(Rn) << 5) | U32(Rt))

#define LDR_REG(size, VR, Rm, option, S, Rn, Rt)                               \
  LDR_STR_REG(size, VR, 0b01, Rm, option, S, Rn, Rt)
#define STR_REG(size, VR, Rm, option, S, Rn, Rt)                               \
  LDR_STR_REG(size, VR, 0b00, Rm, option, S, Rn, Rt)

#define LDR_LITERAL(opc, V, imm19, Rt)                                         \
  (uin32_t)((U32(opc) << 30) | (U32(0b011) << 27) | (U32(V) << 26) |           \
            (U32(IMM(imm19, 19)) << 5) | U32(Rt))

#define LDR_STR_IMM_POST_INDEX(size, VR, opc, imm9, Rn, Rt)                    \
  (uint32_t)((U32(size) << 30) | (U32(0b111) << 27) | (U32(VR) << 26) |        \
             (U32(opc) << 22) | (U32(IMM(imm9, 9)) << 12) |                    \
             (U32(0b01) << 10) | (U32(Rn) << 5) | U32(Rt))

#define LDR_STR_IMM_PRE_INDEX(size, VR, opc, imm9, Rn, Rt)                     \
  (uint32_t)((U32(size) << 30) | (U32(0b111) << 27) | (U32(VR) << 26) |        \
             (U32(opc) << 22) | (U32(IMM(imm9, 9)) << 12) |                    \
             (U32(0b11) << 10) | (U32(Rn) << 5) | U32(Rt))

#define LDR_STR_IMM_UNSIGNED(size, VR, opc, imm12, Rn, Rt)                     \
  (uint32_t)((U32(size) << 30) | (U32(0b111) << 27) | (U32(VR) << 26) |        \
             (U32(0b01) << 24) | (U32(opc) << 22) |                            \
             (U32(IMM(imm12, 12)) << 10) | (U32(Rn) << 5) | U32(Rt))

#define LDR_STR_INT_IMM_UNSIGNED(size, opc, imm12, Rn, Rt)                     \
  LDR_STR_IMM_UNSIGNED(size, 0b0, opc, imm12, Rn, Rt)
#define LDR_STR_FP_IMM_UNSIGNED(size, opc, imm12, Rn, Rt)                      \
  LDR_STR_IMM_UNSIGNED(size, 0b1, opc, imm12, Rn, Rt)

#define STR_8_IMM_UNSIGNED(imm12, Rn, Rt)                                      \
  LDR_STR_INT_IMM_UNSIGNED(0b00, 0b00, imm12, Rn, Rt)
#define LDR_8_IMM_UNSIGNED(imm12, Rn, Rt)                                      \
  LDR_STR_INT_IMM_UNSIGNED(0b00, 0b01, imm12, Rn, Rt)

#define STR_16_IMM_UNSIGNED(imm12, Rn, Rt)                                     \
  LDR_STR_INT_IMM_UNSIGNED(0b01, 0b00, imm12, Rn, Rt)
#define LDR_16_IMM_UNSIGNED(imm12, Rn, Rt)                                     \
  LDR_STR_INT_IMM_UNSIGNED(0b01, 0b01, imm12, Rn, Rt)

#define STR_32_IMM_UNSIGNED(imm12, Rn, Rt)                                     \
  LDR_STR_INT_IMM_UNSIGNED(0b10, 0b00, imm12, Rn, Rt)
#define LDR_32_IMM_UNSIGNED(imm12, Rn, Rt)                                     \
  LDR_STR_INT_IMM_UNSIGNED(0b10, 0b01, imm12, Rn, Rt)

#define STR_64_IMM_UNSIGNED(imm12, Rn, Rt)                                     \
  LDR_STR_INT_IMM_UNSIGNED(0b11, 0b00, imm12, Rn, Rt)
#define LDR_64_IMM_UNSIGNED(imm12, Rn, Rt)                                     \
  LDR_STR_INT_IMM_UNSIGNED(0b11, 0b01, imm12, Rn, Rt)

#define STR_FP_8_IMM_UNSIGNED(imm12, Rn, Rt)                                   \
  LDR_STR_FP_IMM_UNSIGNED(0b00, 0b00, imm12, Rn, Rt)
#define LDR_FP_8_IMM_UNSIGNED(imm12, Rn, Rt)                                   \
  LDR_STR_FP_IMM_UNSIGNED(0b00, 0b01, imm12, Rn, Rt)

#define STR_FP_16_IMM_UNSIGNED(imm12, Rn, Rt)                                  \
  LDR_STR_FP_IMM_UNSIGNED(0b01, 0b00, imm12, Rn, Rt)
#define LDR_FP_16_IMM_UNSIGNED(imm12, Rn, Rt)                                  \
  LDR_STR_FP_IMM_UNSIGNED(0b01, 0b01, imm12, Rn, Rt)

#define STR_FP_32_IMM_UNSIGNED(imm12, Rn, Rt)                                  \
  LDR_STR_FP_IMM_UNSIGNED(0b10, 0b00, imm12, Rn, Rt)
#define LDR_FP_32_IMM_UNSIGNED(imm12, Rn, Rt)                                  \
  LDR_STR_FP_IMM_UNSIGNED(0b10, 0b01, imm12, Rn, Rt)

#define STR_FP_64_IMM_UNSIGNED(imm12, Rn, Rt)                                  \
  LDR_STR_FP_IMM_UNSIGNED(0b11, 0b00, imm12, Rn, Rt)
#define LDR_FP_64_IMM_UNSIGNED(imm12, Rn, Rt)                                  \
  LDR_STR_FP_IMM_UNSIGNED(0b11, 0b01, imm12, Rn, Rt)

/* Register moves */

#define MOV_WIDE_IMM(sf, opc, hw, imm16, Rd)                                   \
  (uint32_t)((U32(sf) << 31) | (U32(opc) << 29) | (U32(0b100101) << 23) |      \
             (U32(hw) << 21) | (U32(IMM(imm16, 16)) << 5) | U32(Rd))

#define MOVN(sf, hw, imm16, Rd) MOV_WIDE_IMM(sf, 0b00, hw, imm16, Rd)
#define MOVZ(sf, hw, imm16, Rd) MOV_WIDE_IMM(sf, 0b10, hw, imm16, Rd)
#define MOVK(sf, hw, imm16, Rd) MOV_WIDE_IMM(sf, 0b11, hw, imm16, Rd)

#define FMOV(sf, ftype, rmode, opcode, Rn, Rd)                                 \
  (uint32_t)((U32(sf) << 31) | (U32(0b0011110) << 24) | (U32(ftype) << 22) |   \
             (U32(0b1) << 21) | (U32(rmode) << 19) | (U32(opcode) << 16) |     \
             (U32(Rn) << 5) | U32(Rd))

#define FMOV_W_TO_S(Rn, Rd) FMOV(0b0, 0b00, 0b00, 0b111, Rn, Rd)
#define FMOV_W_TO_D(Rn, Rd) FMOV(0b0, 0b01, 0b00, 0b111, Rn, Rd)
#define FMOV_W_TO_H(Rn, Rd) FMOV(0b0, 0b11, 0b00, 0b111, Rn, Rd)

#define FMOV_H_TO_W(Rn, Rd) FMOV(0b0, 0b11, 0b00, 0b110, Rn, Rd)
#define FMOV_H_TO_X(Rn, Rd) FMOV(0b1, 0b11, 0b00, 0b110, Rn, Rd)

#define FMOV_S_TO_W(Rn, Rd) FMOV(0b0, 0b00, 0b00, 0b110, Rn, Rd)
#define FMOV_D_TO_X(Rn, Rd) FMOV(0b1, 0b01, 0b00, 0b110, Rn, Rd)

#define FMOV_X_TO_S(Rn, Rd) FMOV(0b1, 0b00, 0b00, 0b111, Rn, Rd)
#define FMOV_X_TO_D(Rn, Rd) FMOV(0b1, 0b01, 0b00, 0b111, Rn, Rd)
#define FMOV_X_TO_H(Rn, Rd) FMOV(0b1, 0b11, 0b00, 0b111, Rn, Rd)

#define FMOV_64_TO_TOP_HALF_Q(Rn, Rd) FMOV(0b1, 0b10, 0b01, 0b111, Rn, Rd)
#define FMOV_TOP_HALF_Q_TO_64(Rn, Rd) FMOV(0b1, 0b10, 0b01, 0b110, Rn, Rd)

/* Single reg FP data processing */

#define FP_1_REG(M, S, ftype, opcode, Rn, Rd)                                  \
  (uint32_t)((U32(M) << 31) | (U32(S) << 29) | (U32(0b11110) << 24) |          \
             (U32(ftype) << 22) | (U32(0b1) << 21) | (U32(opcode) << 15) |     \
             (U32(0b10000) << 10) | (U32(Rn) << 5) | U32(Rd))

#define FMOV_S_TO_S(Rn, Rd) FP_1_REG(0b0, 0b0, 0b00, 0b000000, Rn, Rd)
#define FMOV_D_TO_D(Rn, Rd) FP_1_REG(0b0, 0b0, 0b01, 0b000000, Rn, Rd)
#define FMOV_H_TO_H(Rn, Rd) FP_1_REG(0b0, 0b0, 0b11, 0b000000, Rn, Rd)

#define FCVT(ftype_to, ftype_from, Rn, Rd)                                     \
  FP_1_REG(0b0, 0b0, ftype_from, (0b000100 | (ftype_to)), Rn, Rd)

#define FABS(ftype, Rn, Rd) FP_1_REG(0b0, 0b0, ftype, 0b000001, Rn, Rd)
#define FNEG(ftype, Rn, Rd) FP_1_REG(0b0, 0b0, ftype, 0b000010, Rn, Rd)
#define FSQRT(ftype, Rn, Rd) FP_1_REG(0b0, 0b0, ftype, 0b000011, Rn, Rd)

/* Two reg FP data processing */

#define FP_CMP(M, S, ftype, Rm, op, Rn, opcode2)                               \
  (uint32_t)((U32(M) << 31) | (U32(S) << 29) | (U32(0b11110) << 24) |          \
             (U32(ftype) << 22) | (U32(0b1) << 21) | (U32(Rm) << 16) |         \
             (U32(op) << 14 | (U32(0b1000) << 10) | (U32(Rn) << 5) |        \
             U32(opcode2))

#define FCMP(ftype, Rm, Rn) FP_CMP(0b0, 0b0, ftype, Rm, 0b00, Rn, 0b00000)
#define FCMP_ZERO(ftype, Rn) FP_CMP(0b0, 0b0, ftype, 0b0000, 0b00, Rn, 0b01000)
#define FCMPE(ftype, Rm, Rn) FP_CMP(0b0, 0b0, ftype, Rm, 0b00, Rn, 0b10000)
#define FCMPE_ZERO(ftype, Rn) FP_CMP(0b0, 0b0, ftype, 0b0000, 0b00, Rn, 0b11000)

#define FP_2_REG(M, S, ftype, Rm, opcode, Rn, Rd)                              \
  (uint32_t)((U32(M) << 31) | (U32(S) << 29) | (U32(0b11110) << 24) |          \
             (U32(ftype) << 22) | (U32(0b1) << 21) | (U32(Rm) << 16) |         \
             (U32(opcode) << 12) | (U32(0b10) << 10) | (U32(Rn) << 5) |        \
             U32(Rd))

#define FMUL(ftype, Rm, Rn, Rd) FP_2_REG(0b0, 0b0, ftype, Rm, 0b0000, Rn, Rd)
#define FDIV(ftype, Rm, Rn, Rd) FP_2_REG(0b0, 0b0, ftype, Rm, 0b0001, Rn, Rd)
#define FADD(ftype, Rm, Rn, Rd) FP_2_REG(0b0, 0b0, ftype, Rm, 0b0010, Rn, Rd)
#define FSUB(ftype, Rm, Rn, Rd) FP_2_REG(0b0, 0b0, ftype, Rm, 0b0011, Rn, Rd)
#define FMAX(ftype, Rm, Rn, Rd) FP_2_REG(0b0, 0b0, ftype, Rm, 0b0100, Rn, Rd)
#define FMIN(ftype, Rm, Rn, Rd) FP_2_REG(0b0, 0b0, ftype, Rm, 0b0101, Rn, Rd)
#define FMAXNM(ftype, Rm, Rn, Rd) FP_2_REG(0b0, 0b0, ftype, Rm, 0b0110, Rn, Rd)
#define FMINNM(ftype, Rm, Rn, Rd) FP_2_REG(0b0, 0b0, ftype, Rm, 0b0111, Rn, Rd)
#define FNMUL(ftype, Rm, Rn, Rd) FP_2_REG(0b0, 0b0, ftype, Rm, 0b1000, Rn, Rd)
#define FMINNM(ftype, Rm, Rn, Rd) FP_2_REG(0b0, 0b0, ftype, Rm, 0b0111, Rn, Rd)

/* Integer <-> FP conversions */

#define INT_FIXP_CONV(sf, S, ftype, rmode, opcode, scale, Rn, Rd)              \
  (uint32_t)((U32(sf) << 31) | (U32(S) << 29) | (U32(0b11110) << 24) |         \
             (U32(ftype) << 22) | (U32(0b0) << 21) | (U32(rmode) << 19) |      \
             (U32(opcode) << 16) | (U32(scale) << 10) | (U32(Rn) << 5) |       \
             U32(Rd))

#define SCVTF_32FIXP_TO_S(scale, Rn, Rd)                                       \
  INT_FIXP_CONV(0b0, 0b0, 0b00, 0b00, 0b010, scale, Rn, Rd)
#define UCVTF_32FIXP_TO_S(scale, Rn, Rd)                                       \
  INT_FIXP_CONV(0b0, 0b0, 0b00, 0b00, 0b011, scale, Rn, Rd)

#define FCVTZS_S_TO_32FIXP(scale, Rn, Rd)                                      \
  INT_FIXP_CONV(0b0, 0b0, 0b00, 0b11, 0b000, scale, Rn, Rd)
#define FCVTZU_S_TO_32FIXP(scale, Rn, Rd)                                      \
  INT_FIXP_CONV(0b0, 0b0, 0b00, 0b11, 0b001, scale, Rn, Rd)

#define SCVTF_32FIXP_TO_D(scale, Rn, Rd)                                       \
  INT_FIXP_CONV(0b0, 0b0, 0b01, 0b00, 0b010, scale, Rn, Rd)
#define UCVTF_32FIXP_TO_D(scale, Rn, Rd)                                       \
  INT_FIXP_CONV(0b0, 0b0, 0b01, 0b00, 0b011, scale, Rn, Rd)

#define FCVTZS_D_TO_32FIXP(scale, Rn, Rd)                                      \
  INT_FIXP_CONV(0b0, 0b0, 0b01, 0b11, 0b000, scale, Rn, Rd)
#define FCVTZU_D_TO_32FIXP(scale, Rn, Rd)                                      \
  INT_FIXP_CONV(0b0, 0b0, 0b01, 0b11, 0b001, scale, Rn, Rd)

#define SCVTF_32FIXP_TO_H(scale, Rn, Rd)                                       \
  INT_FIXP_CONV(0b0, 0b0, 0b11, 0b00, 0b010, scale, Rn, Rd)
#define UCVTF_32FIXP_TO_H(scale, Rn, Rd)                                       \
  INT_FIXP_CONV(0b0, 0b0, 0b11, 0b00, 0b011, scale, Rn, Rd)

#define FCVTZS_H_TO_32FIXP(scale, Rn, Rd)                                      \
  INT_FIXP_CONV(0b0, 0b0, 0b11, 0b11, 0b000, scale, Rn, Rd)
#define FCVTZU_H_TO_32FIXP(scale, Rn, Rd)                                      \
  INT_FIXP_CONV(0b0, 0b0, 0b11, 0b11, 0b001, scale, Rn, Rd)

#define SCVTF_64FIXP_TO_S(scale, Rn, Rd)                                       \
  INT_FIXP_CONV(0b1, 0b0, 0b00, 0b00, 0b010, scale, Rn, Rd)
#define UCVTF_64FIXP_TO_S(scale, Rn, Rd)                                       \
  INT_FIXP_CONV(0b1, 0b0, 0b00, 0b00, 0b011, scale, Rn, Rd)

#define FCVTZS_S_TO_64FIXP(scale, Rn, Rd)                                      \
  INT_FIXP_CONV(0b1, 0b0, 0b00, 0b11, 0b000, scale, Rn, Rd)
#define FCVTZU_S_TO_64FIXP(scale, Rn, Rd)                                      \
  INT_FIXP_CONV(0b1, 0b0, 0b00, 0b11, 0b001, scale, Rn, Rd)

#define SCVTF_64FIXP_TO_D(scale, Rn, Rd)                                       \
  INT_FIXP_CONV(0b1, 0b0, 0b01, 0b00, 0b010, scale, Rn, Rd)
#define UCVTF_64FIXP_TO_D(scale, Rn, Rd)                                       \
  INT_FIXP_CONV(0b1, 0b0, 0b01, 0b00, 0b011, scale, Rn, Rd)

#define FCVTZS_D_TO_64FIXP(scale, Rn, Rd)                                      \
  INT_FIXP_CONV(0b1, 0b0, 0b01, 0b11, 0b000, scale, Rn, Rd)
#define FCVTZU_D_TO_64FIXP(scale, Rn, Rd)                                      \
  INT_FIXP_CONV(0b1, 0b0, 0b01, 0b11, 0b001, scale, Rn, Rd)

#define SCVTF_64FIXP_TO_H(scale, Rn, Rd)                                       \
  INT_FIXP_CONV(0b1, 0b0, 0b11, 0b00, 0b010, scale, Rn, Rd)
#define UCVTF_64FIXP_TO_H(scale, Rn, Rd)                                       \
  INT_FIXP_CONV(0b1, 0b0, 0b11, 0b00, 0b011, scale, Rn, Rd)

#define FCVTZS_H_TO_64FIXP(scale, Rn, Rd)                                      \
  INT_FIXP_CONV(0b1, 0b0, 0b11, 0b11, 0b000, scale, Rn, Rd)
#define FCVTZU_H_TO_64FIXP(scale, Rn, Rd)                                      \
  INT_FIXP_CONV(0b1, 0b0, 0b11, 0b11, 0b001, scale, Rn, Rd)

#define INT_FP_CONV(sf, S, ftype, rmode, opcode, Rn, Rd)                       \
  (uint32_t)((U32(sf) << 31) | (U32(S) << 29) | (U32(0b11110) << 24) |         \
             (U32(ftype) << 22) | (U32(0b1) << 21) | (U32(rmode) << 19) |      \
             (U32(opcode) << 16) | (U32(Rn) << 5) | U32(Rd))

#define SCVTF_32_TO_S(Rn, Rd) INT_FP_CONV(0b0, 0b0, 0b00, 0b00, 0b010, Rn, Rd)
#define UCVTF_32_TO_S(Rn, Rd) INT_FP_CONV(0b0, 0b0, 0b00, 0b00, 0b011, Rn, Rd)

#define SCVTF_64_TO_S(Rn, Rd) INT_FP_CONV(0b1, 0b0, 0b00, 0b00, 0b010, Rn, Rd)
#define UCVTF_64_TO_S(Rn, Rd) INT_FP_CONV(0b1, 0b0, 0b00, 0b00, 0b011, Rn, Rd)

#define FCVTZS_S_TO_32(Rn, Rd) INT_FP_CONV(0b0, 0b0, 0b00, 0b11, 0b000, Rn, Rd)
#define FCVTZU_S_TO_32(Rn, Rd) INT_FP_CONV(0b0, 0b0, 0b00, 0b11, 0b001, Rn, Rd)

#define FCVTZS_S_TO_64(Rn, Rd) INT_FP_CONV(0b1, 0b0, 0b00, 0b11, 0b000, Rn, Rd)
#define FCVTZU_S_TO_64(Rn, Rd) INT_FP_CONV(0b1, 0b0, 0b00, 0b11, 0b001, Rn, Rd)

#define SCVTF_32_TO_D(Rn, Rd) INT_FP_CONV(0b0, 0b0, 0b01, 0b00, 0b010, Rn, Rd)
#define UCVTF_32_TO_D(Rn, Rd) INT_FP_CONV(0b0, 0b0, 0b01, 0b00, 0b011, Rn, Rd)

#define FCVTZS_D_TO_32(Rn, Rd) INT_FP_CONV(0b0, 0b0, 0b01, 0b11, 0b000, Rn, Rd)
#define FCVTZU_D_TO_32(Rn, Rd) INT_FP_CONV(0b0, 0b0, 0b01, 0b11, 0b001, Rn, Rd)

#define SCVTF_32_TO_H(Rn, Rd) INT_FP_CONV(0b0, 0b0, 0b11, 0b00, 0b010, Rn, Rd)
#define UCVTF_32_TO_H(Rn, Rd) INT_FP_CONV(0b0, 0b0, 0b11, 0b00, 0b011, Rn, Rd)

#define FCVTZS_H_TO_32(Rn, Rd) INT_FP_CONV(0b0, 0b0, 0b11, 0b11, 0b000, Rn, Rd)
#define FCVTZU_H_TO_32(Rn, Rd) INT_FP_CONV(0b0, 0b0, 0b11, 0b11, 0b001, Rn, Rd)

#define SCVTF_64_TO_D(Rn, Rd) INT_FP_CONV(0b1, 0b0, 0b01, 0b00, 0b010, Rn, Rd)
#define UCVTF_64_TO_D(Rn, Rd) INT_FP_CONV(0b1, 0b0, 0b01, 0b00, 0b011, Rn, Rd)

#define FCVTZS_D_TO_64(Rn, Rd) INT_FP_CONV(0b1, 0b0, 0b01, 0b11, 0b000, Rn, Rd)
#define FCVTZU_D_TO_64(Rn, Rd) INT_FP_CONV(0b1, 0b0, 0b01, 0b11, 0b001, Rn, Rd)

#define SCVTF_64_TO_H(Rn, Rd) INT_FP_CONV(0b1, 0b0, 0b11, 0b00, 0b010, Rn, Rd)
#define UCVTF_64_TO_H(Rn, Rd) INT_FP_CONV(0b1, 0b0, 0b11, 0b00, 0b011, Rn, Rd)

#define FCVTZS_H_TO_64(Rn, Rd) INT_FP_CONV(0b1, 0b0, 0b11, 0b11, 0b000, Rn, Rd)
#define FCVTZU_H_TO_64(Rn, Rd) INT_FP_CONV(0b1, 0b0, 0b11, 0b11, 0b001, Rn, Rd)

/* Bitfield operations (Immediate) */

#define BITFIELD_IMM(sf, opc, immr, imms, Rn, Rd)                              \
  (uint32_t)((U32(sf) << 31) | (U32(opc) << 29) | (U32(0b100110) << 23) |      \
             (U32(sf) << 22) | (U32(IMM(immr, 6)) << 16) |                     \
             (U32(IMM(imms, 6)) << 10) | (U32(Rn) << 5) | U32(Rd))

#define SBFM_IMM(sf, immr, imms, Rn, Rd)                                       \
  BITFIELD_IMM(sf, 0b00, immr, imms, Rn, Rd)
#define BFM_IMM(sf, immr, imms, Rn, Rd)                                        \
  BITFIELD_IMM(sf, 0b01, immr, imms, Rn, Rd)
#define UBFM_IMM(sf, immr, imms, Rn, Rd)                                       \
  BITFIELD_IMM(sf, 0b10, immr, imms, Rn, Rd)

/* Addressing (Immediate) */

#define ADR_IMM(op, immlo, immhi, Rd)                                          \
  (uint32_t)((U32(op) << 31) | (U32(IMM(immlo, 2)) << 29) |                    \
             (U32(0b10000) << 24) | (U32(IMM(immhi, 19)) << 5) | U32(Rd))

#define ADR(immlo, immhi, Rd) ADR_IMM(0, immlo, immhi, Rd)
#define ADRP(immlo, immhi, Rd) ADR_IMM(1, immlo, immhi, Rd)

/* Arithmetic & logical operations (Immediate) */

#define LOGICAL_IMM(sf, opc, N, immr, imms, Rn, Rd)                            \
  (uint32_t)((U32(sf) << 31) | (U32(opc) << 29) | (U32(0b100100) << 23) |      \
             (U32(IMM(N, 1)) << 22) | (U32(IMM(immr, 6)) << 16) |              \
             (U32(IMM(imms, 6)) << 10) | (U32(Rn) << 5) | U32(Rd))

#define AND_IMM(sf, N, immr, imms, Rn, Rd)                                     \
  LOGICAL_IMM(sf, 0b00, IMM_ASSERT(N, 0), immr, imms, Rn, Rd)
#define ORR_IMM(sf, N, immr, imms, Rn, Rd)                                     \
  LOGICAL_IMM(sf, 0b01, IMM_ASSERT(N, 0), immr, imms, Rn, Rd)
#define EOR_IMM(sf, N, immr, imms, Rn, Rd)                                     \
  LOGICAL_IMM(sf, 0b10, IMM_ASSERT(N, 0), immr, imms, Rn, Rd)
#define ANDS_IMM(sf, N, immr, imms, Rn, Rd)                                    \
  LOGICAL_IMM(sf, 0b11, IMM_ASSERT(N, 0), immr, imms, Rn, Rd)

#define ADD_SUB_IMM_WITH_TAGS(sf, op, S, uimm6, op3, uimm4, Rn, Rd)            \
  (uint32_t)((U32(sf) << 31) | (U32(op) << 30) | (U32(S) << 29) |              \
             (U32(0b1000110) << 22) | (U32(IMM(uimm6, 6)) << 16) |             \
             (U32(op3) << 14) | (U32(IMM(uimm4, 4)) << 10) | (U32(Rn) << 5) |  \
             U32(Rd))

#define ADD_SUB_IMM(sf, op, S, sh, imm12, Rn, Rd)                              \
  (uint32_t)((U32(sf) << 31) | (U32(op) << 30) | (U32(S) << 29) |              \
             (U32(0b100010) << 23) | (U32(sh) << 22) |                         \
             (U32(IMM(imm12, 12)) << 10) | (U32(Rn) << 5) | U32(Rd))

#define ADD_IMM(sf, sh, imm12, Rn, Rd)                                         \
  ADD_SUB_IMM(sf, 0b0, 0b0, sh, imm12, Rn, Rd)
#define ADDS_IMM(sf, sh, imm12, Rn, Rd)                                        \
  ADD_SUB_IMM(sf, 0b0, 0b1, sh, imm12, Rn, Rd)

#define SUB_IMM(sf, sh, imm12, Rn, Rd)                                         \
  ADD_SUB_IMM(sf, 0b1, 0b0, sh, imm12, Rn, Rd)
#define SUBS_IMM(sf, sh, imm12, Rn, Rd)                                        \
  ADD_SUB_IMM(sf, 0b1, 0b1, sh, imm12, Rn, Rd)

/* Arithmetic & logical operations (Register) */

#define LOGICAL_SHIFTED_REG(sf, opc, shift, N, Rm, imm6, Rn, Rd)               \
  (uint32_t)((U32(sf) << 31) | (U32(opc) << 29) | (U32(0b01010) << 24) |       \
             (U32(shift) << 22) | (U32(N) << 21) | (U32(Rm) << 16) |           \
             (U32(IMM(imm6, 6)) << 10) | (U32(Rn) << 5) | U32(Rd))

#define ADD_SUB_SHIFTED_REG(sf, op, S, shift, Rm, imm6, Rn, Rd)                \
  (uint32_t)((U32(sf) << 31) | (U32(op) << 30) | (U32(S) << 29) |              \
             (U32(0b01011) << 24) | (U32(shift) << 22) | (U32(Rm) << 16) |     \
             (U32(IMM(imm6, 6)) << 10) | (U32(Rn) << 5) | U32(Rd))

#define ADD_REG(sf, shift, imm6, Rm, Rn, Rd)                                   \
  ADD_SUB_SHIFTED_REG(sf, 0b0, 0b0, shift, Rm, imm6, Rn, Rd)
#define SUB_REG(sf, shift, imm6, Rm, Rn, Rd)                                   \
  ADD_SUB_SHIFTED_REG(sf, 0b1, 0b0, shift, Rm, imm6, Rn, Rd)
#define ADDS_REG(sf, shift, imm6, Rm, Rn, Rd)                                  \
  ADD_SUB_SHIFTED_REG(sf, 0b0, 0b1, shift, Rm, imm6, Rn, Rd)
#define SUBS_REG(sf, shift, imm6, Rm, Rn, Rd)                                  \
  ADD_SUB_SHIFTED_REG(sf, 0b1, 0b1, shift, Rm, imm6, Rn, Rd)

#define ADD_SUB_EXTENDED_REG(sf, op, S, opt, Rm, option, imm3, Rn, Rd)         \
  (uint32_t)((U32(sf) << 31) | (U32(op) << 30) | (U32(S) << 29) |              \
             (U32(0b01011) << 24) | (U32(opt) << 22) | (U32(0b1) << 21) |      \
             (U32(Rm) << 16) | (U32(option) << 13) |                           \
             (U32(IMM(imm3, 3)) << 10) | (U32(Rn) << 5) | U32(Rd))

#define ADD_EXT(sf, option, imm3, Rm, Rn, Rd)                                  \
  ADD_SUB_EXTENDED_REG(sf, 0b0, 0b0, 0b00, Rm, option, imm3, Rn, Rd)
#define SUB_EXT(sf, option, imm3, Rm, Rn, Rd)                                  \
  ADD_SUB_EXTENDED_REG(sf, 0b1, 0b0, 0b00, Rm, option, imm3, Rn, Rd)
#define ADDS_EXT(sf, option, imm3, Rm, Rn, Rd)                                 \
  ADD_SUB_EXTENDED_REG(sf, 0b0, 0b1, 0b00, Rm, option, imm3, Rn, Rd)
#define SUBS_EXT(sf, option, imm3, Rm, Rn, Rd)                                 \
  ADD_SUB_EXTENDED_REG(sf, 0b1, 0b1, 0b00, Rm, option, imm3, Rn, Rd)

#define REG_2_SOURCE(sf, S, opcode, Rm, Rn, Rd)                                \
  (uint32_t)((U32(sf) << 31) | (U32(S) << 29) | (U32(0b11010110) << 21) |      \
             (U32(Rm) << 16) | (U32(opcode) << 10) | (U32(Rn) << 5) | U32(Rd))

#define REG_3_SOURCE(sf, op54, op31, Rm, o0, Ra, Rn, Rd)                       \
  (uint32_t)((U32(sf) << 31) | (U32(op54) << 29) | (U32(0b11011) << 24) |      \
             (U32(op31) << 21) | (U32(Rm) << 16) | (U32(o0) << 15) |           \
             (U32(Ra) << 10) | (U32(Rn) << 5) | U32(Rd))

#define MADD(sf, Rm, Ra, Rn, Rd)                                               \
  REG_3_SOURCE(sf, 0b00, 0b000, Rm, 0b0, Ra, Rn, Rd)
#define MSUB(sf, Rm, Ra, Rn, Rd)                                               \
  REG_3_SOURCE(sf, 0b00, 0b000, Rm, 0b1, Ra, Rn, Rd)

#define LSLV(sf, Rm, Rn, Rd) REG_2_SOURCE(sf, 0b0, 0b001000, Rm, Rn, Rd)
#define LSRV(sf, Rm, Rn, Rd) REG_2_SOURCE(sf, 0b0, 0b001001, Rm, Rn, Rd)
#define ASRV(sf, Rm, Rn, Rd) REG_2_SOURCE(sf, 0b0, 0b001010, Rm, Rn, Rd)
#define RORV(sf, Rm, Rn, Rd) REG_2_SOURCE(sf, 0b0, 0b001011, Rm, Rn, Rd)

#define UDIV(sf, Rm, Rn, Rd) REG_2_SOURCE(sf, 0b0, 0b000010, Rm, Rn, Rd)
#define SDIV(sf, Rm, Rn, Rd) REG_2_SOURCE(sf, 0b0, 0b000011, Rm, Rn, Rd)

#define SHIFT_LSL (0b00)
#define SHIFT_LSR (0b01)
#define SHIFT_ASR (0b10)
#define SHIFT_ROR (0b11)

#define AND_REG(sf, shift, Rm, imm6, Rn, Rd)                                   \
  LOGICAL_SHIFTED_REG(sf, 0b00, shift, 0b0, Rm, imm6, Rn, Rd)
#define ANDS_REG(sf, shift, Rm, imm6, Rn, Rd)                                  \
  LOGICAL_SHIFTED_REG(sf, 0b11, shift, 0b0, Rm, imm6, Rn, Rd)
#define ORR_REG(sf, shift, Rm, imm6, Rn, Rd)                                   \
  LOGICAL_SHIFTED_REG(sf, 0b01, shift, 0b0, Rm, imm6, Rn, Rd)
#define ORN_REG(sf, shift, Rm, imm6, Rn, Rd)                                   \
  LOGICAL_SHIFTED_REG(sf, 0b01, shift, 0b1, Rm, imm6, Rn, Rd)
#define EOR_REG(sf, shift, Rm, imm6, Rn, Rd)                                   \
  LOGICAL_SHIFTED_REG(sf, 0b10, shift, 0b0, Rm, imm6, Rn, Rd)
#define EON_REG(sf, shift, Rm, imm6, Rn, Rd)                                   \
  LOGICAL_SHIFTED_REG(sf, 0b10, shift, 0b1, Rm, imm6, Rn, Rd)

/* Conditional selects */

#define CONDITIONAL_SELECT(sf, op, S, Rm, cond, op2, Rn, Rd)                   \
  (uint32_t)((U32(sf) << 31) | (U32(op) << 30) | (U32(S) << 29) |              \
             (U32(0b11010100) << 21) | (U32(Rm) << 16) | (U32(cond) << 12) |   \
             (U32(op2) << 10) | (U32(Rn) << 5) | U32(Rd))

#define CSEL(sf, Rm, cond, Rn, Rd)                                             \
  CONDITIONAL_SELECT(sf, 0b0, 0b0, Rm, cond, 0b00, Rn, Rd)
#define CSINC(sf, Rm, cond, Rn, Rd)                                            \
  CONDITIONAL_SELECT(sf, 0b0, 0b0, Rm, cond, 0b01, Rn, Rd)
#define CSINV(sf, Rm, cond, Rn, Rd)                                            \
  CONDITIONAL_SELECT(sf, 0b1, 0b0, Rm, cond, 0b00, Rn, Rd)
#define CSNEG(sf, Rm, cond, Rn, Rd)                                            \
  CONDITIONAL_SELECT(sf, 0b0, 0b0, Rm, cond, 0b01, Rn, Rd)

/* Branches */

#define BRANCH(op0, op1, op2)                                                  \
  (uint32_t)((U32(op0) << 29) | (U32(0b101) << 26) | (U32(op1) << 5) | U32(op2))

#define BRANCH_COND_IMM(o1, o0, imm19, cond)                                   \
  (uint32_t)((U32(0b0101010) << 25) | (U32(o1) << 24) |                        \
             (U32(IMM(imm19, 19)) << 5) | (U32(o0) << 4) | U32(cond))

#define BRANCH_IMM(op, imm26)                                                  \
  (uint32_t)((U32(op) << 31) | (U32(0b00101) << 26) | U32(IMM(imm26, 26)))

#define B_COND(imm19, cond) BRANCH_COND_IMM(0b0, 0b0, imm19, cond)
#define BC_COND(imm19, cond) BRANCH_COND_IMM(0b0, 0b1, imm19, cond)

#define B(imm26) BRANCH_IMM(0b0, imm26)
#define BL(imm26) BRANCH_IMM(0b1, imm26)

#define CMP_AND_BRANCH(sf, op, imm19, Rt)                                      \
  (uint32_t)((U32(sf) << 31) | (U32(0b011010) << 25) | (U32(op) << 24) |       \
             (U32(IMM(imm19, 19)) << 5) | U32(Rt))

#define CBZ(sf, imm19, Rt) CMP_AND_BRANCH(sf, 0b0, imm19, Rt)
#define CBNZ(sf, imm19, Rt) CMP_AND_BRANCH(sf, 0b1, imm19, Rt)

#define UNCOND_BRANCH_REG(opc, op2, op3, Rn, op4)                              \
  BRANCH(0b110,                                                                \
         (U32(0b1) << 20) | (U32(opc) << 16) | (U32(op2) << 11) |              \
             (U32(op3) << 5) | U32(Rn),                                        \
         op4)

#define BR(Rn) UNCOND_BRANCH_REG(0b0000, 0b11111, 0b000000, Rn, 00000)
#define BLR(Rn) UNCOND_BRANCH_REG(0b0001, 0b11111, 0b000000, Rn, 00000)
#define RET(Rn) UNCOND_BRANCH_REG(0b0010, 0b11111, 0b000000, Rn, 00000)

#endif
