#ifndef AARCH64_ISA_H
#define AARCH64_ISA_H

// Constants must be casted to uint32_t to be well-defined behaviour
#define U32(v) ((uint32_t)(v))

/* Nop */

#define NOP (uint32_t)(0b11010101000000110010000000011111)

/* Loads & Stores */

#define LDR_STR(op0, op1, op2, op3, op4)                                       \
  (uint32_t)((U32(op0) << 28) | U32(0b1 << 27) |                               \
             (U32(op1) << 26) /* bit 25 is zero */                             \
             | (U32(op2) << 22) | (U32(op3) << 12) | U32(op4))

#define LDR_STR_PAIR_POST_INDEX(opc, V, L, imm7, Rt2, Rn, Rt)                  \
  (uint32_t)((U32(opc) << 30) | (U32(0b101) << 27) | (U32(V) << 26) |          \
             (U32(0b1) << 23) | (U32(L) << 22) | (U32(imm7) << 15) |           \
             (U32(Rt2) << 10) | (U32(Rn) << 5) | U32(Rt))

#define LDR_STR_PAIR_PRE_INDEX(opc, V, L, imm7, Rt2, Rn, Rt)                   \
  (uint32_t)((U32(opc) << 30) | (U32(0b101) << 27) | (U32(V) << 26) |          \
             (U32(0b11) << 23) | (U32(L) << 22) | (U32(imm7) << 15) |          \
             (U32(Rt2) << 10) | (U32(Rn) << 5) | U32(Rt))

#define LDR_STR_PAIR_OFFSET(opc, V, L, imm7, Rt2, Rn, Rt)                      \
  (uint32_t)((U32(opc) << 30) | (U32(0b101) << 27) | (U32(V) << 26) |          \
             (U32(0b10) << 23) | (U32(L) << 22) | (U32(imm7) << 15) |          \
             (U32(Rt2) << 10) | (U32(Rn) << 5) | U32(Rt))

#define STP_POST_INDEX_32(imm7, Rt2, Rn, Rt)                                   \
  LDR_STR_PAIR_POST_INDEX(0b00, 0, 0, imm7, Rt2, Rn, Rt)
#define LDP_POST_INDEX_32(imm7, Rt2, Rn, Rt)                                   \
  LDR_STR_PAIR_POST_INDEX(0b00, 0, 1, imm7, Rt2, Rn, Rt)
#define STP_POST_INDEX_64(imm7, Rt2, Rn, Rt)                                   \
  LDR_STR_PAIR_POST_INDEX(0b10, 0, 0, imm7, Rt2, Rn, Rt)
#define LDP_POST_INDEX_64(imm7, Rt2, Rn, Rt)                                   \
  LDR_STR_PAIR_POST_INDEX(0b10, 0, 1, imm7, Rt2, Rn, Rt)

#define STP_PRE_INDEX_32(imm7, Rt2, Rn, Rt)                                    \
  LDR_STR_PAIR_PRE_INDEX(0b00, 0, 0, imm7, Rt2, Rn, Rt)
#define LDP_PRE_INDEX_32(imm7, Rt2, Rn, Rt)                                    \
  LDR_STR_PAIR_PRE_INDEX(0b00, 0, 1, imm7, Rt2, Rn, Rt)
#define STP_PRE_INDEX_64(imm7, Rt2, Rn, Rt)                                    \
  LDR_STR_PAIR_PRE_INDEX(0b10, 0, 0, imm7, Rt2, Rn, Rt)
#define LDP_PRE_INDEX_64(imm7, Rt2, Rn, Rt)                                    \
  LDR_STR_PAIR_PRE_INDEX(0b10, 0, 1, imm7, Rt2, Rn, Rt)

#define STP_OFFSET_32(imm7, Rt2, Rn, Rt)                                       \
  LDR_STR_PAIR_OFFSET(0b00, 0, 0, imm7, Rt2, Rn, Rt)
#define LDP_OFFSET_32(imm7, Rt2, Rn, Rt)                                       \
  LDR_STR_PAIR_OFFSET(0b00, 0, 1, imm7, Rt2, Rn, Rt)
#define STP_OFFSET_64(imm7, Rt2, Rn, Rt)                                       \
  LDR_STR_PAIR_OFFSET(0b10, 0, 0, imm7, Rt2, Rn, Rt)
#define LDP_OFFSET_64(imm7, Rt2, Rn, Rt)                                       \
  LDR_STR_PAIR_OFFSET(0b10, 0, 1, imm7, Rt2, Rn, Rt)

#define LDR_LITERAL(opc, V, imm19, Rt)                                         \
  (uin32_t)((U32(opc) << 30) | (U32(0b011) << 27) | (U32(V) << 26) |           \
            (U32(imm19) << 5) | U32(Rt))

#define LDR_STR_IMM_POST_INDEX(size, VR, opc, imm9, Rn, Rt)                    \
  (uint32_t)((U32(size) << 30) | (U32(0b111) << 27) | (U32(VR) << 26) |        \
             (U32(opc) << 22) | (U32(imm9) << 12) | (U32(0b01) << 10) |        \
             (U32(Rn) << 5) | U32(Rt))

#define LDR_STR_IMM_PRE_INDEX(size, VR, opc, imm9, Rn, Rt)                     \
  (uint32_t)((U32(size) << 30) | (U32(0b111) << 27) | (U32(VR) << 26) |        \
             (U32(opc) << 22) | (U32(imm9) << 12) | (U32(0b11) << 10) |        \
             (U32(Rn) << 5) | U32(Rt))

#define LDR_STR_IMM_UNSIGNED(size, VR, opc, imm12, Rn, Rt)                     \
  (uint32_t)((U32(size) << 30) | (U32(0b111) << 27) | (U32(VR) << 26) |        \
             (U32(0b01) << 24) | (U32(opc) << 22) | (U32(imm12) << 10) |       \
             (U32(Rn) << 5) | U32(Rt))

#define STR_8_IMM_UNSIGNED(imm12, Rn, Rt)                                      \
  LDR_STR_IMM_UNSIGNED(0b00, 0b0, 0b00, imm12, Rn, Rt)
#define LDR_8_IMM_UNSIGNED(imm12, Rn, Rt)                                      \
  LDR_STR_IMM_UNSIGNED(0b00, 0b0, 0b01, imm12, Rn, Rt)

#define STR_16_IMM_UNSIGNED(imm12, Rn, Rt)                                     \
  LDR_STR_IMM_UNSIGNED(0b01, 0b0, 0b00, imm12, Rn, Rt)
#define LDR_16_IMM_UNSIGNED(imm12, Rn, Rt)                                     \
  LDR_STR_IMM_UNSIGNED(0b01, 0b0, 0b01, imm12, Rn, Rt)

#define STR_32_IMM_UNSIGNED(imm12, Rn, Rt)                                     \
  LDR_STR_IMM_UNSIGNED(0b10, 0b0, 0b00, imm12, Rn, Rt)
#define LDR_32_IMM_UNSIGNED(imm12, Rn, Rt)                                     \
  LDR_STR_IMM_UNSIGNED(0b10, 0b0, 0b01, imm12, Rn, Rt)

#define STR_64_IMM_UNSIGNED(imm12, Rn, Rt)                                     \
  LDR_STR_IMM_UNSIGNED(0b11, 0b0, 0b00, imm12, Rn, Rt)
#define LDR_64_IMM_UNSIGNED(imm12, Rn, Rt)                                     \
  LDR_STR_IMM_UNSIGNED(0b11, 0b0, 0b01, imm12, Rn, Rt)

/* Register moves */

#define MOV_WIDE_IMM(sf, opc, hw, imm16, Rd)                                   \
  (uint32_t)((U32(sf) << 31) | (U32(opc) << 29) | (U32(0b100101) << 23) |      \
             (U32(hw) << 21) | (U32(imm16) << 5) | U32(Rd))

#define MOVN_32(hw, imm16, Rd) MOV_WIDE_IMM(0b0, 0b00, hw, imm16, Rd)
#define MOVZ_32(hw, imm16, Rd) MOV_WIDE_IMM(0b0, 0b10, hw, imm16, Rd)
#define MOVK_32(hw, imm16, Rd) MOV_WIDE_IMM(0b0, 0b11, hw, imm16, Rd)

#define MOVN_64(hw, imm16, Rd) MOV_WIDE_IMM(0b1, 0b00, hw, imm16, Rd)
#define MOVZ_64(hw, imm16, Rd) MOV_WIDE_IMM(0b1, 0b10, hw, imm16, Rd)
#define MOVK_64(hw, imm16, Rd) MOV_WIDE_IMM(0b1, 0b11, hw, imm16, Rd)

/* Bitfield operations (Immediate) */

#define BITFIELD_IMM(sf, opc, N, immr, imms, Rn, Rd)                           \
  (uint32_t)((U32(sf) << 31) | (U32(opc) << 29) | (U32(0b100110) << 23) |      \
             (U32(N) << 22) | (U32(immr) << 16) | (U32(imms) << 10) |          \
             (U32(Rn) << 5) | U32(Rd))

#define SBFM_32_IMM(immr, imms, Rn, Rd)                                        \
  BITFIELD_IMM(0b0, 0b00, 0b0, immr, imms, Rn, Rd)
#define BFM_32_IMM(immr, imms, Rn, Rd)                                         \
  BITFIELD_IMM(0b0, 0b10, 0b0, immr, imms, Rn, Rd)
#define UBFM_32_IMM(immr, imms, Rn, Rd)                                        \
  BITFIELD_IMM(0b0, 0b01, 0b0, immr, imms, Rn, Rd)

#define SBFM_64_IMM(immr, imms, Rn, Rd)                                        \
  BITFIELD_IMM(0b1, 0b00, 0b1, immr, imms, Rn, Rd)
#define BFM_64_IMM(immr, imms, Rn, Rd)                                         \
  BITFIELD_IMM(0b1, 0b10, 0b1, immr, imms, Rn, Rd)
#define UBFM_64_IMM(immr, imms, Rn, Rd)                                        \
  BITFIELD_IMM(0b1, 0b01, 0b1, immr, imms, Rn, Rd)

/* Addressing (Immediate) */

#define ADR_IMM(op, immlo, immhi, Rd)                                          \
  (uint32_t)((U32(op) << 31) | (U32(immlo) << 29) | (U32(0b10000) << 24) |     \
             (U32(immhi) << 5) | U32(Rd))

#define ADR(immlo, immhi, Rd) ADR_IMM(0, immlo, immhi, Rd)
#define ADRP(immlo, immhi, Rd) ADR_IMM(1, immlo, immhi, Rd)

/* Arithmetic & logical operations (Immediate) */

#define LOGICAL_IMM(sf, opc, N, immr, imms, Rn, Rd)                            \
  (uint32_t)((U32(sf) << 31) | (U32(opc) << 29) | (U32(0b100100) << 23) |      \
             (U32(N) << 22) | (U32(immr) << 16) | (U32(imms) << 10) |          \
             (U32(Rn) << 5) | U32(Rd))

#define AND_32_IMM(immr, imms, Rn, Rd)                                         \
  LOGICAL_IMM(0b0, 0b00, 0b0, immr, imms, Rn, Rd)
#define ORR_32_IMM(immr, imms, Rn, Rd)                                         \
  LOGICAL_IMM(0b0, 0b01, 0b0, immr, imms, Rn, Rd)
#define EOR_32_IMM(immr, imms, Rn, Rd)                                         \
  LOGICAL_IMM(0b0, 0b10, 0b0, immr, imms, Rn, Rd)
#define ANDS_32_IMM(immr, imms, Rn, Rd)                                        \
  LOGICAL_IMM(0b0, 0b11, 0b0, immr, imms, Rn, Rd)

#define AND_64_IMM(immr, imms, Rn, Rd)                                         \
  LOGICAL_IMM(0b1, 0b00, 0b0, immr, imms, Rn, Rd)
#define ORR_64_IMM(immr, imms, Rn, Rd)                                         \
  LOGICAL_IMM(0b1, 0b01, 0b0, immr, imms, Rn, Rd)
#define EOR_64_IMM(immr, imms, Rn, Rd)                                         \
  LOGICAL_IMM(0b1, 0b10, 0b0, immr, imms, Rn, Rd)
#define ANDS_64_IMM(immr, imms, Rn, Rd)                                        \
  LOGICAL_IMM(0b1, 0b11, 0b0, immr, imms, Rn, Rd)

#define ADD_SUB_IMM_WITH_TAGS(sf, op, S, uimm6, op3, uimm4, Rn, Rd)            \
  (uint32_t)((U32(sf) << 31) | (U32(op) << 30) | (U32(S) << 29) |              \
             (U32(0b1000110) << 22) | (U32(uimm6) << 16) | (U32(op3) << 14) |  \
             (U32(uimm4) << 10) | (U32(Rn) << 5) | U32(Rd))

#define ADD_SUB_IMM(sf, op, S, sh, imm12, Rn, Rd)                              \
  (uint32_t)((U32(sf) << 31) | (U32(op) << 30) | (U32(S) << 29) |              \
             (U32(0b100010) << 23) | (U32(sh) << 22) | (U32(imm12) << 10) |    \
             (U32(Rn) << 5) | U32(Rd))

#define ADD_32_IMM(sh, imm12, Rn, Rd)                                          \
  ADD_SUB_IMM(0b0, 0b0, 0b0, sh, imm12, Rn, Rd)
#define ADDS_32_IMM(sh, imm12, Rn, Rd)                                         \
  ADD_SUB_IMM(0b0, 0b0, 0b1, sh, imm12, Rn, Rd)

#define SUB_32_IMM(sh, imm12, Rn, Rd)                                          \
  ADD_SUB_IMM(0b0, 0b1, 0b0, sh, imm12, Rn, Rd)
#define SUBS_32_IMM(sh, imm12, Rn, Rd)                                         \
  ADD_SUB_IMM(0b0, 0b1, 0b1, sh, imm12, Rn, Rd)

#define ADD_64_IMM(sh, imm12, Rn, Rd)                                          \
  ADD_SUB_IMM(0b1, 0b0, 0b0, sh, imm12, Rn, Rd)
#define ADDS_64_IMM(sh, imm12, Rn, Rd)                                         \
  ADD_SUB_IMM(0b1, 0b0, 0b1, sh, imm12, Rn, Rd)

#define SUB_64_IMM(sh, imm12, Rn, Rd)                                          \
  ADD_SUB_IMM(0b1, 0b1, 0b0, sh, imm12, Rn, Rd)
#define SUBS_64_IMM(sh, imm12, Rn, Rd)                                         \
  ADD_SUB_IMM(0b1, 0b1, 0b1, sh, imm12, Rn, Rd)

/* Arithmetic & logical operations (Register) */

#define LOGICAL_SHIFTED_REG(sf, opc, shift, N, Rm, imm6, Rn, Rd)               \
  (uint32_t)((U32(sf) << 31) | (U32(opc) << 29) | (U32(0b01010) << 24) |       \
             (U32(shift) << 22) | (U32(N) << 21) | (U32(Rm) << 16) |           \
             (U32(imm6) << 10) | (U32(Rn) << 5) | U32(Rd))

#define ADD_SUB_SHIFTED_REG(sf, op, S, shift, Rm, imm6, Rn, Rd)                \
  (uint32_t)((U32(sf) << 31) | (U32(op) << 30) | (U32(S) << 29) |              \
             (U32(0b01011) << 24) | (U32(shift) << 22) | (U32(Rm) << 16) |     \
             (U32(imm6) << 10) | (U32(Rn) << 5) | U32(Rd))

#define ADD_32_REG(shift, imm6, Rm, Rn, Rd)                                    \
  ADD_SUB_SHIFTED_REG(0b0, 0b0, 0b0, shift, Rm, imm6, Rn, Rd)
#define SUB_32_REG(shift, imm6, Rm, Rn, Rd)                                    \
  ADD_SUB_SHIFTED_REG(0b0, 0b1, 0b0, shift, Rm, imm6, Rn, Rd)
#define ADDS_32_REG(shift, imm6, Rm, Rn, Rd)                                   \
  ADD_SUB_SHIFTED_REG(0b0, 0b0, 0b1, shift, Rm, imm6, Rn, Rd)
#define SUBS_32_REG(shift, imm6, Rm, Rn, Rd)                                   \
  ADD_SUB_SHIFTED_REG(0b0, 0b1, 0b1, shift, Rm, imm6, Rn, Rd)

#define ADD_64_REG(shift, imm6, Rm, Rn, Rd)                                    \
  ADD_SUB_SHIFTED_REG(0b1, 0b0, 0b0, shift, Rm, imm6, Rn, Rd)
#define SUB_64_REG(shift, imm6, Rm, Rn, Rd)                                    \
  ADD_SUB_SHIFTED_REG(0b1, 0b1, 0b0, shift, Rm, imm6, Rn, Rd)
#define ADDS_64_REG(shift, imm6, Rm, Rn, Rd)                                   \
  ADD_SUB_SHIFTED_REG(0b1, 0b0, 0b1, shift, Rm, imm6, Rn, Rd)
#define SUBS_64_REG(shift, imm6, Rm, Rn, Rd)                                   \
  ADD_SUB_SHIFTED_REG(0b1, 0b1, 0b1, shift, Rm, imm6, Rn, Rd)

#define REG_2_SOURCE(sf, S, opcode, Rm, Rn, Rd)                                \
  (uint32_t)((U32(sf) << 31) | (U32(S) << 29) | (U32(0b11010110) << 21) |      \
             (U32(Rm) << 16) | (U32(opcode) << 10) | (U32(Rn) << 5) | U32(Rd))

#define REG_3_SOURCE(sf, op54, op31, Rm, o0, Ra, Rn, Rd)                       \
  (uint32_t)((U32(sf) << 31) | (U32(op54) << 29) | (U32(0b11011) << 24) |      \
             (U32(op31) << 21) | (U32(Rm) << 16) | (U32(o0) << 15) |           \
             (U32(Ra) << 10) | (U32(Rn) << 5) | U32(Rd))

#define MADD_32(Rm, Ra, Rn, Rd)                                                \
  REG_3_SOURCE(0b0, 0b00, 0b000, Rm, 0b0, Ra, Rn, Rd)
#define MSUB_32(Rm, Ra, Rn, Rd)                                                \
  REG_3_SOURCE(0b0, 0b00, 0b000, Rm, 0b1, Ra, Rn, Rd)

#define MADD_64(Rm, Ra, Rn, Rd)                                                \
  REG_3_SOURCE(0b1, 0b00, 0b000, Rm, 0b0, Ra, Rn, Rd)
#define MSUB_64(Rm, Ra, Rn, Rd)                                                \
  REG_3_SOURCE(0b1, 0b00, 0b000, Rm, 0b1, Ra, Rn, Rd)

#define LSLV_32(Rm, Rn, Rd) REG_2_SOURCE(0b0, 0b0, 0b001000, Rm, Rn, Rd)
#define LSRV_32(Rm, Rn, Rd) REG_2_SOURCE(0b0, 0b0, 0b001001, Rm, Rn, Rd)
#define ASRV_32(Rm, Rn, Rd) REG_2_SOURCE(0b0, 0b0, 0b001010, Rm, Rn, Rd)
#define RORV_32(Rm, Rn, Rd) REG_2_SOURCE(0b0, 0b0, 0b001011, Rm, Rn, Rd)

#define LSLV_64(Rm, Rn, Rd) REG_2_SOURCE(0b1, 0b0, 0b001000, Rm, Rn, Rd)
#define LSRV_64(Rm, Rn, Rd) REG_2_SOURCE(0b1, 0b0, 0b001001, Rm, Rn, Rd)
#define ASRV_64(Rm, Rn, Rd) REG_2_SOURCE(0b1, 0b0, 0b001010, Rm, Rn, Rd)
#define RORV_64(Rm, Rn, Rd) REG_2_SOURCE(0b1, 0b0, 0b001011, Rm, Rn, Rd)

#define UDIV_32(Rm, Rn, Rd) REG_2_SOURCE(0b0, 0b0, 0b000010, Rm, Rn, Rd)
#define SDIV_32(Rm, Rn, Rd) REG_2_SOURCE(0b0, 0b0, 0b000011, Rm, Rn, Rd)

#define UDIV_64(Rm, Rn, Rd) REG_2_SOURCE(0b1, 0b0, 0b000010, Rm, Rn, Rd)
#define SDIV_64(Rm, Rn, Rd) REG_2_SOURCE(0b1, 0b0, 0b000011, Rm, Rn, Rd)

#define SHIFT_LSL (0b00)
#define SHIFT_LSR (0b01)
#define SHIFT_ASR (0b10)
#define SHIFT_ROR (0b11)

#define AND_32_REG(shift, Rm, imm6, Rn, Rd)                                    \
  LOGICAL_SHIFTED_REG(0b0, 0b00, shift, 0b0, Rm, imm6, Rn, Rd)
#define AND_64_REG(shift, Rm, imm6, Rn, Rd)                                    \
  LOGICAL_SHIFTED_REG(0b1, 0b00, shift, 0b0, Rm, imm6, Rn, Rd)

#define ORR_32_REG(shift, Rm, imm6, Rn, Rd)                                    \
  LOGICAL_SHIFTED_REG(0b0, 0b01, shift, 0b0, Rm, imm6, Rn, Rd)
#define ORN_32_REG(shift, Rm, imm6, Rn, Rd)                                    \
  LOGICAL_SHIFTED_REG(0b0, 0b01, shift, 0b1, Rm, imm6, Rn, Rd)
#define EOR_32_REG(shift, Rm, imm6, Rn, Rd)                                    \
  LOGICAL_SHIFTED_REG(0b0, 0b10, shift, 0b0, Rm, imm6, Rn, Rd)
#define EON_32_REG(shift, Rm, imm6, Rn, Rd)                                    \
  LOGICAL_SHIFTED_REG(0b0, 0b10, shift, 0b1, Rm, imm6, Rn, Rd)


#define ORR_64_REG(shift, Rm, imm6, Rn, Rd)                                    \
  LOGICAL_SHIFTED_REG(0b1, 0b01, shift, 0b0, Rm, imm6, Rn, Rd)
#define ORN_64_REG(shift, Rm, imm6, Rn, Rd)                                    \
  LOGICAL_SHIFTED_REG(0b1, 0b01, shift, 0b1, Rm, imm6, Rn, Rd)
#define EOR_64_REG(shift, Rm, imm6, Rn, Rd)                                    \
  LOGICAL_SHIFTED_REG(0b1, 0b10, shift, 0b0, Rm, imm6, Rn, Rd)
#define EON_64_REG(shift, Rm, imm6, Rn, Rd)                                    \
  LOGICAL_SHIFTED_REG(0b1, 0b10, shift, 0b1, Rm, imm6, Rn, Rd)

#define MOV_32_REG(Rm, Rd) ORR_32_REG(SHIFT_LSL, Rm, 0b000000, ZERO_REG.idx, Rd)
#define MOVN_32_REG(Rm, Rd)                                                    \
  ORN_32_REG(SHIFT_LSL, Rm, 0b000000, ZERO_REG.idx, Rd)

#define MOV_64_REG(Rm, Rd) ORR_64_REG(SHIFT_LSL, Rm, 0b000000, ZERO_REG.idx, Rd)
#define MOVN_64_REG(Rm, Rd)                                                    \
  ORN_64_REG(SHIFT_LSL, Rm, 0b000000, ZERO_REG.idx, Rd)

/* Conditional selects */

#define CONDITIONAL_SELECT(sf, op, S, Rm, cond, op2, Rn, Rd)                   \
  (uint32_t)((U32(sf) << 31) | (U32(op) << 30) | (U32(S) << 29) |              \
             (U32(0b11010100) << 21) | (U32(Rm) << 16) | (U32(cond) << 12) |   \
             (U32(op2) << 10) | (U32(Rn) << 5) | U32(Rd))

#define CSEL_32(Rm, cond, Rn, Rd)                                              \
  CONDITIONAL_SELECT(0b0, 0b0, 0b0, Rm, cond, 0b00, Rn, Rd)
#define CSINC_32(Rm, cond, Rn, Rd)                                             \
  CONDITIONAL_SELECT(0b0, 0b0, 0b0, Rm, cond, 0b01, Rn, Rd)
#define CSINV_32(Rm, cond, Rn, Rd)                                             \
  CONDITIONAL_SELECT(0b0, 0b1, 0b0, Rm, cond, 0b00, Rn, Rd)
#define CSNEG_32(Rm, cond, Rn, Rd)                                             \
  CONDITIONAL_SELECT(0b0, 0b0, 0b0, Rm, cond, 0b01, Rn, Rd)

#define CSEL_64(Rm, cond, Rn, Rd)                                              \
  CONDITIONAL_SELECT(0b1, 0b0, 0b0, Rm, cond, 0b00, Rn, Rd)
#define CSINC_64(Rm, cond, Rn, Rd)                                             \
  CONDITIONAL_SELECT(0b1, 0b0, 0b0, Rm, cond, 0b01, Rn, Rd)
#define CSINV_64(Rm, cond, Rn, Rd)                                             \
  CONDITIONAL_SELECT(0b1, 0b1, 0b0, Rm, cond, 0b00, Rn, Rd)
#define CSNEG_64(Rm, cond, Rn, Rd)                                             \
  CONDITIONAL_SELECT(0b1, 0b0, 0b0, Rm, cond, 0b01, Rn, Rd)

/* Branches */

#define BRANCH(op0, op1, op2)                                                  \
  (uint32_t)((U32(op0) << 29) | (U32(0b101) << 26) | (U32(op1) << 5) | U32(op2))

#define BRANCH_COND_IMM(o1, o0, imm19, cond)                                   \
  (uint32_t)((U32(0b0101010) << 25) | (U32(o1) << 24) | (U32(imm19) << 5) |    \
             (U32(o0) << 4) | U32(cond))

#define BRANCH_IMM(op, imm26)                                                  \
  (uint32_t)((U32(op) << 31) | (U32(0b00101) << 26) | U32(imm26))

#define B_COND(imm19, cond) BRANCH_COND_IMM(0b0, 0b0, imm19, cond)
#define BC_COND(imm19, cond) BRANCH_COND_IMM(0b0, 0b1, imm19, cond)

#define B(imm26) BRANCH_IMM(0b0, imm26)
#define BL(imm26) BRANCH_IMM(0b1, imm26)

#define CMP_AND_BRANCH_IMM(sf, op, imm19, Rt)                                  \
  (uint32_t)((U32(sf) << 31) | (U32(0b011010) << 25) | (U32(op) << 24) |       \
             (U32(imm19) << 5) | U32(Rt))

#define CBZ_32_IMM(imm19, Rt) CMP_AND_BRANCH_IMM(0b0, 0b0, imm19, Rt)
#define CBNZ_32_IMM(imm19, Rt) CMP_AND_BRANCH_IMM(0b0, 0b1, imm19, Rt)
#define CBZ_64_IMM(imm19, Rt) CMP_AND_BRANCH_IMM(0b1, 0b0, imm19, Rt)
#define CBNZ_64_IMM(imm19, Rt) CMP_AND_BRANCH_IMM(0b1, 0b1, imm19, Rt)

#define UNCOND_BRANCH_REG(opc, op2, op3, Rn, op4)                              \
  BRANCH(0b110,                                                                \
         (U32(0b1) << 20) | (U32(opc) << 16) | (U32(op2) << 11) |              \
             (U32(op3) << 5) | U32(Rn),                                        \
         op4)

#define RET(Rn) UNCOND_BRANCH_REG(0b0010, 0b11111, 0b000000, Rn, 00000)

#endif
