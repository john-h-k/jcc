#ifndef AARCH64_ISA_H
#define AARCH64_ISA_H

/* Nop */

#define NOP (uint32_t)(0b11010101000000110010000000011111)

/* Loads & Stores */

#define LDR_STR(op0, op1, op2, op3, op4)                                       \
  (uint32_t)(((op0) << 28) | (0b1 << 27) | ((op1) << 26) /* bit 25 is zero */  \
             | ((op2) << 22) | ((op3) << 12) | (op4))

#define LDR_LITERAL(opc, V, imm19, Rt)                                         \
  (uin32_t)(((opc) << 30) | (0b011 << 27) | ((V) << 26) | ((imm19) << 5) | (Rt))
  
#define LDR_STR_IMM_POST_INDEX(size, VR, opc, imm9, Rn, Rt) (uint32_t)( \
  ((size) << 30) \
  | (0b111 << 27) \
  | ((VR) << 26) \
  | ((opc) << 22) \
  | ((imm9) << 12) \
  | (0b01 << 10) \
  | ((Rn) << 5) \
  | (Rt) \
)

#define LDR_STR_IMM_PRE_INDEX(size, VR, opc, imm9, Rn, Rt) (uint32_t)( \
  ((size) << 30) \
  | (0b111 << 27) \
  | ((VR) << 26) \
  | ((opc) << 22) \
  | ((imm9) << 12) \
  | (0b11 << 10) \
  | ((Rn) << 5) \
  | (Rt) \
)

#define LDR_STR_IMM_UNSIGNED(size, VR, opc, imm12, Rn, Rt) (uint32_t)( \
  ((size) << 30) \
  | (0b111 << 27) \
  | ((VR) << 26) \
  | (0b01 << 24) \
  | ((opc) << 22) \
  | ((imm12) << 10) \
  | ((Rn) << 5) \
  | (Rt) \
)

#define STR_32_IMM_UNSIGNED(imm12, Rn, Rt) LDR_STR_IMM_UNSIGNED(0b10, 0b0, 0b00, imm12, Rn, Rt)
#define LDR_32_IMM_UNSIGNED(imm12, Rn, Rt) LDR_STR_IMM_UNSIGNED(0b10, 0b0, 0b01, imm12, Rn, Rt)

/* Register moves */

#define MOV_WIDE_IMM(sf, opc, hw, imm16, Rd)                                   \
  (uint32_t)(((sf) << 31) | ((opc) << 29) | (0b100101 << 23) | ((hw) << 21) |  \
             ((imm16) << 5) | (Rd))

#define MOVN_32(hw, imm16, Rd) MOV_WIDE_IMM(0b0, 0b00, hw, imm16, Rd)
#define MOVZ_32(hw, imm16, Rd) MOV_WIDE_IMM(0b0, 0b10, hw, imm16, Rd)
#define MOVK_32(hw, imm16, Rd) MOV_WIDE_IMM(0b0, 0b11, hw, imm16, Rd)

#define MOVN_64(hw, imm16, Rd) MOV_WIDE_IMM(0b1, 0b00, hw, imm16, Rd)
#define MOVZ_64(hw, imm16, Rd) MOV_WIDE_IMM(0b1, 0b10, hw, imm16, Rd)
#define MOVK_64(hw, imm16, Rd) MOV_WIDE_IMM(0b1, 0b11, hw, imm16, Rd)


/* Arithmetic & logical operations (Immediate) */

#define LOGICAL_IMM(sf, opc, N, immr, imms, Rn, Rd)                            \
  (uint32_t)(((sf) << 31) | ((opc) << 29) | (0b100100 << 23) | ((N) << 22) |   \
             ((immr) << 16) | ((immr) << 10) | ((Rn) << 5) | (Rd))
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

#define ADD_SUB_IMM_WITH_TAGS(sf, op, S, uimm6, op3, uimm4, Rn, Rd)                            \
  (uint32_t)(((sf) << 31) | ((op) << 30) | ((S) << 29) | (0b1000110 << 22) | ((uimm6) << 16) |   \
             ((op3) << 14) | ((uimm4) << 10) | ((Rn) << 5) | (Rd))

#define ADD_SUB_IMM(sf, op, S, sh, imm12, Rn, Rd)                            \
  (uint32_t)(((sf) << 31) | ((op) << 30) | ((S) << 29) | (0b100010 << 23) | ((sh) << 22) |   \
             ((imm12) << 10) | ((Rn) << 5) | (Rd))

#define ADD_32_IMM(sh, imm12, Rn, Rd) ADD_SUB_IMM(0b0, 0b0, 0b0, sh, imm12, Rn, Rd)
#define ADDS_32_IMM(sh, imm12, Rn, Rd) ADD_SUB_IMM(0b0, 0b0, 0b1, sh, imm12, Rn, Rd)

#define SUB_32_IMM(sh, imm12, Rn, Rd) ADD_SUB_IMM(0b0, 0b1, 0b0, sh, imm12, Rn, Rd)
#define SUBS_32_IMM(sh, imm12, Rn, Rd) ADD_SUB_IMM(0b0, 0b1, 0b1, sh, imm12, Rn, Rd)

#define ADD_64_IMM(sh, imm12, Rn, Rd) ADD_SUB_IMM(0b1, 0b0, 0b0, sh, imm12, Rn, Rd)
#define ADDS_64_IMM(sh, imm12, Rn, Rd) ADD_SUB_IMM(0b1, 0b0, 0b1, sh, imm12, Rn, Rd)

#define SUB_64_IMM(sh, imm12, Rn, Rd) ADD_SUB_IMM(0b1, 0b1, 0b0, sh, imm12, Rn, Rd)
#define SUBS_64_IMM(sh, imm12, Rn, Rd) ADD_SUB_IMM(0b1, 0b1, 0b1, sh, imm12, Rn, Rd)

/* Arithmetic & logical operations (Register) */

#define LOGICAL_REG(sf, opc, shift, N, Rm, imm6, Rn, Rd)                       \
  (uint32_t)(((sf) << 31) | ((opc) << 29) | (0b01010 << 24) |                  \
             ((shift) << 22) | ((N) << 21) | ((Rm) << 16) | ((imm6) << 10) |   \
             ((Rn) << 5) | (Rd))

#define ADD_SUB_SHIFTED_REG(sf, op, S, shift, Rm, imm6, Rn, Rd)                \
  (uint32_t)(((sf) << 31) | ((op) << 30) | ((S) << 29) | ((0b01011) << 24) |   \
             ((shift) << 22) | ((Rm) << 16) | ((imm6) << 10) | ((Rn) << 5) |   \
             (Rd))

#define ADD_32_REG(shift, imm6, Rm, Rn, Rd)                                    \
  ADD_SUB_SHIFTED_REG(0, 0, 0, shift, Rm, imm6, Rn, Rd)
#define SUB_32_REG(shift, imm6, Rm, Rn, Rd)                                    \
  ADD_SUB_SHIFTED_REG(0, 1, 0, shift, Rm, imm6, Rn, Rd)

#define REG_2_SOURCE(sf, S, opcode, Rm, Rn, Rd)                                \
  (uint32_t)(((sf) << 31) | ((S) << 29) | (0b11010110 << 21) | ((Rm) << 16) |  \
             ((opcode) << 10) | ((Rn) << 5) | (Rd))

#define REG_3_SOURCE(sf, op54, op31, Rm, o0, Ra, Rn, Rd)                       \
  (uint32_t)(((sf) << 31) | ((op54) << 29) | (0b11011 << 24) |                 \
             ((op31) << 21) | ((Rm) << 16) | ((o0) << 15) | ((Ra) << 10) |     \
             ((Rn) << 5) | (Rd))

#define MADD_32(Rm, Ra, Rn, Rd)                                                \
  REG_3_SOURCE(0b0, 0b00, 0b000, Rm, 0b0, Ra, Rn, Rd)
#define MSUB_32(Rm, Ra, Rn, Rd)                                                \
  REG_3_SOURCE(0b0, 0b00, 0b000, Rm, 0b1, Ra, Rn, Rd)

#define MADD_64(Rm, Ra, Rn, Rd)                                                \
  REG_3_SOURCE(0b1, 0b00, 0b000, Rm, 0b0, Ra, Rn, Rd)
#define MSUB_64(Rm, Ra, Rn, Rd)                                                \
  REG_3_SOURCE(0b1, 0b00, 0b000, Rm, 0b1, Ra, Rn, Rd)

#define UDIV_32(Rm, Rn, Rd) REG_2_SOURCE(0b0, 0b0, 0b000010, Rm, Rn, Rd)
#define SDIV_32(Rm, Rn, Rd) REG_2_SOURCE(0b0, 0b0, 0b000011, Rm, Rn, Rd)

#define SHIFT_LSL (0b00)
#define SHIFT_LSR (0b01)
#define SHIFT_ASR (0b10)
#define SHIFT_ROR (0b11)

#define ORR_32_REG(shift, Rm, imm6, Rn, Rd)                                    \
  LOGICAL_REG(0b0, 0b01, shift, 0b0, Rm, imm6, Rn, Rd)
#define ORN_32_REG(shift, Rm, imm6, Rn, Rd)                                    \
  LOGICAL_REG(0b0, 0b01, shift, 0b1, Rm, imm6, Rn, Rd)

#define ORR_64_REG(shift, Rm, imm6, Rn, Rd)                                    \
  LOGICAL_REG(0b1, 0b01, shift, 0b0, Rm, imm6, Rn, Rd)
#define ORN_64_REG(shift, Rm, imm6, Rn, Rd)                                    \
  LOGICAL_REG(0b1, 0b01, shift, 0b1, Rm, imm6, Rn, Rd)

#define MOV_32_REG(Rm, Rd) ORR_32_REG(SHIFT_LSL, Rm, 0b000000, ZERO_REG.idx, Rd)
#define MOVN_32_REG(Rm, Rd)                                                    \
  ORN_32_REG(SHIFT_LSL, Rm, 0b000000, ZERO_REG.idx, Rd)

#define MOV_64_REG(Rm, Rd) ORR_64_REG(SHIFT_LSL, Rm, 0b000000, ZERO_REG.idx, Rd)
#define MOVN_64_REG(Rm, Rd)                                                    \
  ORN_64_REG(SHIFT_LSL, Rm, 0b000000, ZERO_REG.idx, Rd)

/* Branches */

#define BRANCH(op0, op1, op2)                                                  \
  (uint32_t)(((op0) << 29) | (0b101 << 26) | ((op1) << 5) | (op2))

#define BRANCH_IMM(op, imm26) \
  (uint32_t)(((op) << 31) | (0b00101 << 26) | (imm26))

#define B(imm26) BRANCH_IMM(0b0, imm26)
#define BL(imm26) BRANCH_IMM(0b1, imm26)

#define CMP_AND_BRANCH_IMM(sf, op, imm19, Rt) (uint32_t) \
  (((sf) << 31) | (0b011010 << 25) | ((op) << 24) | ((imm19) << 5) | (Rt))

#define CBZ_32_IMM(imm19, Rt) CMP_AND_BRANCH_IMM(0b0, 0b0, imm19, Rt)
#define CBNZ_32_IMM(imm19, Rt) CMP_AND_BRANCH_IMM(0b0, 0b1, imm19, Rt)
#define CBZ_64_IMM(imm19, Rt) CMP_AND_BRANCH_IMM(0b1, 0b0, imm19, Rt)
#define CBNZ_64_IMM(imm19, Rt) CMP_AND_BRANCH_IMM(0b1, 0b1, imm19, Rt)

#define UNCOND_BRANCH_REG(opc, op2, op3, Rn, op4)                              \
  BRANCH(0b110,                                                                \
         (0b1 << 20) | ((opc) << 16) | ((op2) << 11) | (op3 << 5) | (Rn), op4)

#define RET(Rn) UNCOND_BRANCH_REG(0b0010, 0b11111, 0b000000, Rn, 00000)

#endif

