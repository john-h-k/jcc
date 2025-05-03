#ifndef RV32I_ISA_H
#define RV32I_ISA_H

#define RV32I_REG_SIZE (2)
#define RV32I_INSTR_SIZE (2)

#define U32(v) ((uint32_t)(v))
#define CHECK_IMM(imm, sz)                                                     \
  (DEBUG_ASSERT(((imm) < 0 && ((simm_t)(imm) >> ((sz) - 1)) == -1) ||          \
                    ((imm) >= 0 && ((simm_t)(imm) >> ((sz) - 1)) == 0),        \
                "immediate %lld did not fit!", (long long)imm),                \
   imm)

#define U32_S(v, hi, lo) ((U32(v) & ((1u << (hi + 1)) - 1)) >> lo)

#define R_TYPE(funct7, rs2, rs1, funct3, rd, opcode)                           \
  (uint32_t)((U32(funct7) << 25) | (U32(rs2) << 20) | (U32(rs1) << 15) |       \
             (U32(funct3) << 12) | (U32(rd) << 7) | U32(opcode))

#define I_TYPE(imm12, rs1, funct3, rd, opcode)                                 \
  (uint32_t)((U32_S(CHECK_IMM(imm12, 12), 11, 0) << 20) | (U32(rs1) << 15) |   \
             (U32(funct3) << 12) | (U32(rd) << 7) | U32(opcode))

#define S_TYPE(imm12, rs2, rs1, funct3, opcode)                                \
  (uint32_t)((U32(((simm_t)(CHECK_IMM(imm12, 12)) >> 5) & 0b1111111) << 25) |  \
             (U32(rs2) << 20) | (U32(rs1) << 15) | (U32(funct3) << 12) |       \
             (U32((imm12) & 0b11111) << 7) | U32(opcode))

#define B_TYPE(imm12, rs2, rs1, funct3, opcode)                                \
  (DEBUG_ASSERT(!(imm12 & 1), "B-type instructions must be multiple of 2"),    \
   (uint32_t)((U32_S(CHECK_IMM(imm12, 12), 12, 12) << 31) |                    \
              (U32_S(imm12, 10, 5) << 25) | (U32(rs2) << 20) |                 \
              (U32(rs1) << 15) | (U32(funct3) << 12) |                         \
              (U32_S(imm12, 4, 1) << 8) | (U32_S(imm12, 11, 11) << 7) |        \
              U32(opcode)))

#define U_TYPE(imm20, rd, opcode)                                              \
  (uint32_t)((U32(CHECK_IMM(imm20, 20)) << 12) | (U32(rd) << 7) | U32(opcode))

#define J_TYPE(imm20, rd, opcode)                                              \
  (DEBUG_ASSERT(!(imm20 & 1), "J-type instructions must be multiple of 2"),    \
   (uint32_t)((U32_S(CHECK_IMM(imm20, 20), 20, 20) << 31) |                    \
              (U32_S(imm20, 10, 1) << 21) | (U32_S(imm20, 11, 11) << 20) |     \
              (U32_S(imm20, 19, 12) << 12) | (U32(rd) << 7) | U32(opcode)))

#define OPC_LOAD U32(0b0000011)
#define OPC_LOAD_FP U32(0b0000111)
// custom 0
#define OPC_MISC_MEM U32(0b0001111)
#define OPC_OP_IMM U32(0b0010011)
#define OPC_AUIPC U32(0b0010111)
#define OPC_IMM_32 U32(0b0011011)
// 48b
#define OPC_STORE U32(0b0100011)
#define OPC_STORE_FP U32(0b0100111)
// custom 1
#define OPC_AMO U32(0b0101111)
#define OPC_OP U32(0b0110011)
#define OPC_LUI U32(0b0110111)
#define OPC_32 U32(0b0111011)
// 64b
#define OPC_OP_MADD U32(0b1000011)
#define OPC_OP_MSUB U32(0b1000111)
#define OPC_OP_NMSUB U32(0b1001011)
#define OPC_OP_NMADD U32(0b1001111)
#define OPC_OP_FP U32(0b1010011)
// reserved
// custom 2 / rv128
// 48b
#define OPC_BRANCH U32(0b1100011)
#define OPC_JALR U32(0b1100111)
// reserved
#define OPC_JAL U32(0b1101111)
#define OPC_SYSTEM U32(0b1110011)
// reserved
// custom 3 / rv128
// >=80b

/* ----------------------------- */

/* ---------- funct3 ---------- */

#define FUNCT3_ADD U32(0b000)
#define FUNCT3_SUB U32(0b000)

#define FUNCT3_ADD U32(0b000)
#define FUNCT3_SLT U32(0b010)
#define FUNCT3_SLTU U32(0b011)
#define FUNCT3_XOR U32(0b100)
#define FUNCT3_OR U32(0b110)
#define FUNCT3_AND U32(0b111)

#define FUNCT3_SLL U32(0b001)
#define FUNCT3_SRL U32(0b101)
#define FUNCT3_SRA U32(0b101)

#define FUNCT3_JALR U32(0b000)

#define FUNCT3_BEQ U32(0b000)
#define FUNCT3_BNE U32(0b001)
#define FUNCT3_BLT U32(0b100)
#define FUNCT3_BGE U32(0b101)
#define FUNCT3_BLTU U32(0b110)
#define FUNCT3_BGEU U32(0b111)

#define FUNCT3_MUL U32(0b000)
#define FUNCT3_MULH U32(0b001)
#define FUNCT3_MULHU U32(0b010)
#define FUNCT3_MULHSU U32(0b011)
#define FUNCT3_DIV U32(0b100)
#define FUNCT3_DIVU U32(0b101)
#define FUNCT3_REM U32(0b110)
#define FUNCT3_REMU U32(0b111)

#define FUNCT3_LB U32(0b000)
#define FUNCT3_LH U32(0b001)
#define FUNCT3_LW U32(0b010)
#define FUNCT3_LBU U32(0b100)
#define FUNCT3_LHU U32(0b101)
#define FUNCT3_LD U32(0b011)

#define FUNCT3_SB U32(0b000)
#define FUNCT3_SH U32(0b001)
#define FUNCT3_SW U32(0b010)
#define FUNCT3_SD U32(0b011)

#define FUNCT3_FMV_WX U32(0b000)
#define FUNCT3_FMV_XW U32(0b000)

#define FUNCT3_FSGNJ U32(0b000)
#define FUNCT3_FSGNJN U32(0b001)
#define FUNCT3_FSGNJX U32(0b010)

#define FUNCT3_MIN U32(0b000)
#define FUNCT3_MAX U32(0b001)

#define FUNCT3_SLLI U32(0b001)
#define FUNCT3_SLRI U32(0b101)
#define FUNCT3_SRAI U32(0b101)

/* ----------------------------- */

/* ---------- funct7 ---------- */

#define FUNCT7_ADD U32(0b0000000)
#define FUNCT7_MULDIV U32(0b0000001)

#define FUNCT7_FADD_S U32(0b0000000)
#define FUNCT7_FSUB_S U32(0b0000100)
#define FUNCT7_FMUL_S U32(0b0001000)
#define FUNCT7_FDIV_S U32(0b0001100)
#define FUNCT7_FSGNJ_S U32(0b0010000)
#define FUNCT7_FMINMAX_S U32(0b0010100)
#define FUNCT7_FSQRT_S U32(0b0101100)
#define FUNCT7_FCMP_S U32(0b1010000)

#define FUNCT7_FADD_D U32(0b0000001)
#define FUNCT7_FSUB_D U32(0b0000101)
#define FUNCT7_FMUL_D U32(0b0001001)
#define FUNCT7_FDIV_D U32(0b0001101)
#define FUNCT7_FSGNJ_D U32(0b0010001)
#define FUNCT7_FMINMAX_D U32(0b0010101)
#define FUNCT7_FSQRT_D U32(0b0101101)
#define FUNCT7_FCMP_D U32(0b1010001)

#define FUNCT7_FCVT_S_D U32(0b0100000)
#define FUNCT7_FCVT_D_S U32(0b0100001)

#define FUNCT7_FCVT_WS U32(0b1100000)
#define FUNCT7_FCVT_SW U32(0b1101000)

#define FUNCT7_FCVT_WD U32(0b1100001)
#define FUNCT7_FCVT_DW U32(0b1101001)

#define FUNCT7_FMV_WS U32(0b1110000)
#define FUNCT7_FMV_SW U32(0b1111000)

#define FUNCT7_FMV_WD U32(0b1110001)
#define FUNCT7_FMV_DW U32(0b1111001)

#define FUNCT7_SUB U32(0b0100000)
#define FUNCT7_SRL U32(0b0000000)
#define FUNCT7_SRA U32(0b0100000)

#define RS2_FCVT U32(0b00000)
#define RS2_FCVTU U32(0b00001)

#define RM_FLE U32(0b000)
#define RM_FLT U32(0b001)
#define RM_FEQ U32(0b010)

#define ADDI(imm12, rs1, rd) I_TYPE(imm12, rs1, FUNCT3_ADD, rd, OPC_OP_IMM)
#define XORI(imm12, rs1, rd) I_TYPE(imm12, rs1, FUNCT3_XOR, rd, OPC_OP_IMM)

#define ORI(imm12, rs1, rd) I_TYPE(imm12, rs1, FUNCT3_OR, rd, OPC_OP_IMM)
#define ANDI(imm12, rs1, rd) I_TYPE(imm12, rs1, FUNCT3_AND, rd, OPC_OP_IMM)

#define SLTI(imm12, rs1, rd) I_TYPE(imm12, rs1, FUNCT3_SLT, rd, OPC_OP_IMM)
#define SLTIU(imm12, rs1, rd) I_TYPE(imm12, rs1, FUNCT3_SLTU, rd, OPC_OP_IMM)

#define SLLI(shamt, rs1, rd)                                                   \
  I_TYPE(U32((FUNCT7_SRL << 5) | shamt), rs1, FUNCT3_SLLI, rd, OPC_OP_IMM)
#define SRLI(shamt, rs1, rd)                                                   \
  I_TYPE(U32((FUNCT7_SRL << 5) | shamt), rs1, FUNCT3_SLRI, rd, OPC_OP_IMM)
#define SRAI(shamt, rs1, rd)                                                   \
  I_TYPE(U32((FUNCT7_SRA << 5) | shamt), rs1, FUNCT3_SRAI, rd, OPC_OP_IMM)

#define LUI(imm20, rd) U_TYPE(imm20, rd, OPC_LUI)
#define AUIPC(imm20, rd) U_TYPE(imm20, rd, OPC_AUIPC)

#define SLT(rs2, rs1, rd) R_TYPE(FUNCT7_ADD, rs2, rs1, FUNCT3_SLT, rd, OPC_OP)
#define SLTU(rs2, rs1, rd) R_TYPE(FUNCT7_ADD, rs2, rs1, FUNCT3_SLTU, rd, OPC_OP)

#define ADD(rs2, rs1, rd) R_TYPE(FUNCT7_ADD, rs2, rs1, FUNCT3_ADD, rd, OPC_OP)
#define SUB(rs2, rs1, rd) R_TYPE(FUNCT7_SUB, rs2, rs1, FUNCT3_SUB, rd, OPC_OP)

#define MUL(rs2, rs1, rd)                                                      \
  R_TYPE(FUNCT7_MULDIV, rs2, rs1, FUNCT3_MUL, rd, OPC_OP)
#define MULH(rs2, rs1, rd)                                                     \
  R_TYPE(FUNCT7_MULDIV, rs2, rs1, FUNCT3_MULH, rd, OPC_OP)
#define MULHU(rs2, rs1, rd)                                                    \
  R_TYPE(FUNCT7_MULDIV, rs2, rs1, FUNCT3_MULHU, rd, OPC_OP)
#define MULHSU(rs2, rs1, rd)                                                   \
  R_TYPE(FUNCT7_MULDIV, rs2, rs1, FUNCT3_MULHSU, rd, OPC_OP)

#define DIV(rs2, rs1, rd)                                                      \
  R_TYPE(FUNCT7_MULDIV, rs2, rs1, FUNCT3_DIV, rd, OPC_OP)
#define REM(rs2, rs1, rd)                                                      \
  R_TYPE(FUNCT7_MULDIV, rs2, rs1, FUNCT3_REM, rd, OPC_OP)
#define DIVU(rs2, rs1, rd)                                                     \
  R_TYPE(FUNCT7_MULDIV, rs2, rs1, FUNCT3_DIVU, rd, OPC_OP)
#define REMU(rs2, rs1, rd)                                                     \
  R_TYPE(FUNCT7_MULDIV, rs2, rs1, FUNCT3_REMU, rd, OPC_OP)

#define OR(rs2, rs1, rd) R_TYPE(FUNCT7_ADD, rs2, rs1, FUNCT3_OR, rd, OPC_OP)
#define XOR(rs2, rs1, rd) R_TYPE(FUNCT7_ADD, rs2, rs1, FUNCT3_XOR, rd, OPC_OP)
#define AND(rs2, rs1, rd) R_TYPE(FUNCT7_ADD, rs2, rs1, FUNCT3_AND, rd, OPC_OP)

#define SLL(rs2, rs1, rd) R_TYPE(FUNCT7_SRL, rs2, rs1, FUNCT3_SLL, rd, OPC_OP)
#define SRL(rs2, rs1, rd) R_TYPE(FUNCT7_SRL, rs2, rs1, FUNCT3_SRL, rd, OPC_OP)
#define SRA(rs2, rs1, rd) R_TYPE(FUNCT7_SRA, rs2, rs1, FUNCT3_SRA, rd, OPC_OP)

#define JALR(imm12, rs1, rd) I_TYPE(imm12, rs1, FUNCT3_JALR, rd, OPC_JALR)
#define JAL(imm20, rd) J_TYPE(imm20, rd, OPC_JAL)

#define BEQ(imm, rs2, rs1) B_TYPE(imm, rs2, rs1, FUNCT3_BEQ, OPC_BRANCH)
#define BNE(imm, rs2, rs1) B_TYPE(imm, rs2, rs1, FUNCT3_BNE, OPC_BRANCH)
#define BLT(imm, rs2, rs1) B_TYPE(imm, rs2, rs1, FUNCT3_BLT, OPC_BRANCH)
#define BGE(imm, rs2, rs1) B_TYPE(imm, rs2, rs1, FUNCT3_BGE, OPC_BRANCH)
#define BLTU(imm, rs2, rs1) B_TYPE(imm, rs2, rs1, FUNCT3_BLTU, OPC_BRANCH)
#define BGEU(imm, rs2, rs1) B_TYPE(imm, rs2, rs1, FUNCT3_BGEU, OPC_BRANCH)

#define FCVT_S_D(rs1, rd)                                                      \
  R_TYPE(FUNCT7_FCVT_S_D, RS2_FCVTU, rs1, FUNCT3_FMV_WX, rd, OPC_OP_FP)
#define FCVT_D_S(rs1, rd)                                                      \
  R_TYPE(FUNCT7_FCVT_D_S, RS2_FCVT, rs1, FUNCT3_FMV_WX, rd, OPC_OP_FP)

#define FCVT_SW(rs1, rd)                                                       \
  R_TYPE(FUNCT7_FCVT_SW, RS2_FCVT, rs1, FUNCT3_FMV_WX, rd, OPC_OP_FP)
#define FCVT_WS(rs1, rd)                                                       \
  R_TYPE(FUNCT7_FCVT_WS, RS2_FCVT, rs1, FUNCT3_FMV_WX, rd, OPC_OP_FP)

#define FCVTU_SW(rs1, rd)                                                      \
  R_TYPE(FUNCT7_FCVT_SW, RS2_FCVTU, rs1, FUNCT3_FMV_WX, rd, OPC_OP_FP)
#define FCVTU_WS(rs1, rd)                                                      \
  R_TYPE(FUNCT7_FCVT_WS, RS2_FCVTU, rs1, FUNCT3_FMV_WX, rd, OPC_OP_FP)

#define FCVT_DW(rs1, rd)                                                       \
  R_TYPE(FUNCT7_FCVT_DW, RS2_FCVT, rs1, FUNCT3_FMV_WX, rd, OPC_OP_FP)
#define FCVT_WD(rs1, rd)                                                       \
  R_TYPE(FUNCT7_FCVT_WD, RS2_FCVT, rs1, FUNCT3_FMV_WX, rd, OPC_OP_FP)

#define FCVTU_DW(rs1, rd)                                                      \
  R_TYPE(FUNCT7_FCVT_DW, RS2_FCVTU, rs1, FUNCT3_FMV_WX, rd, OPC_OP_FP)
#define FCVTU_WD(rs1, rd)                                                      \
  R_TYPE(FUNCT7_FCVT_WD, RS2_FCVTU, rs1, FUNCT3_FMV_WX, rd, OPC_OP_FP)

#define FMV_SW(rs1, rd)                                                        \
  R_TYPE(FUNCT7_FMV_SW, 0b00, rs1, FUNCT3_FMV_XW, rd, OPC_OP_FP)
#define FMV_WS(rs1, rd)                                                        \
  R_TYPE(FUNCT7_FMV_WS, 0b00, rs1, FUNCT3_FMV_WX, rd, OPC_OP_FP)

#define FSQRT_S(rs1, rd)                                                       \
  R_TYPE(FUNCT7_FSQRT_S, 0b00000, rs1, 0b000, rd, OPC_OP_FP)

#define FSQRT_D(rs1, rd)                                                       \
  R_TYPE(FUNCT7_FSQRT_D, 0b00000, rs1, 0b000, rd, OPC_OP_FP)

#define FEQ_S(rs2, rs1, rd)                                                    \
  R_TYPE(FUNCT7_FCMP_S, rs2, rs1, RM_FEQ, rd, OPC_OP_FP)
#define FLT_S(rs2, rs1, rd)                                                    \
  R_TYPE(FUNCT7_FCMP_S, rs2, rs1, RM_FLT, rd, OPC_OP_FP)
#define FLE_S(rs2, rs1, rd)                                                    \
  R_TYPE(FUNCT7_FCMP_S, rs2, rs1, RM_FLE, rd, OPC_OP_FP)

#define FADD_S(rs2, rs1, rd)                                                   \
  R_TYPE(FUNCT7_FADD_S, rs2, rs1, 0b000, rd, OPC_OP_FP)
#define FSUB_S(rs2, rs1, rd)                                                   \
  R_TYPE(FUNCT7_FSUB_S, rs2, rs1, 0b000, rd, OPC_OP_FP)
#define FMUL_S(rs2, rs1, rd)                                                   \
  R_TYPE(FUNCT7_FMUL_S, rs2, rs1, 0b000, rd, OPC_OP_FP)
#define FDIV_S(rs2, rs1, rd)                                                   \
  R_TYPE(FUNCT7_FDIV_S, rs2, rs1, 0b000, rd, OPC_OP_FP)

#define FMAX_S(rs2, rs1, rd)                                                   \
  R_TYPE(FUNCT7_FMINMAX_S, rs2, rs1, FUNCT3_MIN, rd, OPC_OP_FP)
#define FMIN_S(rs2, rs1, rd)                                                   \
  R_TYPE(FUNCT7_FMINMAX_S, rs2, rs1, FUNCT3_MAX, rd, OPC_OP_FP)

#define FSGNJ_S(rs2, rs1, rd)                                                  \
  R_TYPE(FUNCT7_FSGNJ_S, rs2, rs1, FUNCT3_FSGNJ, rd, OPC_OP_FP)
#define FSGNJN_S(rs2, rs1, rd)                                                 \
  R_TYPE(FUNCT7_FSGNJ_S, rs2, rs1, FUNCT3_FSGNJN, rd, OPC_OP_FP)
#define FSGNJX_S(rs2, rs1, rd)                                                 \
  R_TYPE(FUNCT7_FSGNJ_S, rs2, rs1, FUNCT3_FSGNJX, rd, OPC_OP_FP)

#define FEQ_D(rs2, rs1, rd)                                                    \
  R_TYPE(FUNCT7_FCMP_D, rs2, rs1, RM_FEQ, rd, OPC_OP_FP)
#define FLT_D(rs2, rs1, rd)                                                    \
  R_TYPE(FUNCT7_FCMP_D, rs2, rs1, RM_FLT, rd, OPC_OP_FP)
#define FLE_D(rs2, rs1, rd)                                                    \
  R_TYPE(FUNCT7_FCMP_D, rs2, rs1, RM_FLE, rd, OPC_OP_FP)

#define FADD_D(rs2, rs1, rd)                                                   \
  R_TYPE(FUNCT7_FADD_D, rs2, rs1, 0b000, rd, OPC_OP_FP)
#define FSUB_D(rs2, rs1, rd)                                                   \
  R_TYPE(FUNCT7_FSUB_D, rs2, rs1, 0b000, rd, OPC_OP_FP)
#define FMUL_D(rs2, rs1, rd)                                                   \
  R_TYPE(FUNCT7_FMUL_D, rs2, rs1, 0b000, rd, OPC_OP_FP)
#define FDIV_D(rs2, rs1, rd)                                                   \
  R_TYPE(FUNCT7_FDIV_D, rs2, rs1, 0b000, rd, OPC_OP_FP)

#define FMAX_D(rs2, rs1, rd)                                                   \
  R_TYPE(FUNCT7_FMINMAX_D, rs2, rs1, FUNCT3_MIN, rd, OPC_OP_FP)
#define FMIN_D(rs2, rs1, rd)                                                   \
  R_TYPE(FUNCT7_FMINMAX_D, rs2, rs1, FUNCT3_MAX, rd, OPC_OP_FP)

#define FSGNJ_D(rs2, rs1, rd)                                                  \
  R_TYPE(FUNCT7_FSGNJ_D, rs2, rs1, FUNCT3_FSGNJ, rd, OPC_OP_FP)
#define FSGNJN_D(rs2, rs1, rd)                                                 \
  R_TYPE(FUNCT7_FSGNJ_D, rs2, rs1, FUNCT3_FSGNJN, rd, OPC_OP_FP)
#define FSGNJX_D(rs2, rs1, rd)                                                 \
  R_TYPE(FUNCT7_FSGNJ_D, rs2, rs1, FUNCT3_FSGNJX, rd, OPC_OP_FP)

#define SB(imm, rs2, rs1) S_TYPE(imm, rs2, rs1, FUNCT3_SB, OPC_STORE)
#define SH(imm, rs2, rs1) S_TYPE(imm, rs2, rs1, FUNCT3_SH, OPC_STORE)
#define SW(imm, rs2, rs1) S_TYPE(imm, rs2, rs1, FUNCT3_SW, OPC_STORE)
#define FSW(imm, rs2, rs1) S_TYPE(imm, rs2, rs1, FUNCT3_SW, OPC_STORE_FP)
#define FSD(imm, rs2, rs1) S_TYPE(imm, rs2, rs1, FUNCT3_SD, OPC_STORE_FP)

#define LB(imm, rs1, rd) I_TYPE(imm, rs1, FUNCT3_LB, rd, OPC_LOAD)
#define LBU(imm, rs1, rd) I_TYPE(imm, rs1, FUNCT3_LBU, rd, OPC_LOAD)
#define LH(imm, rs1, rd) I_TYPE(imm, rs1, FUNCT3_LH, rd, OPC_LOAD)
#define LHU(imm, rs1, rd) I_TYPE(imm, rs1, FUNCT3_LHU, rd, OPC_LOAD)
#define LW(imm, rs1, rd) I_TYPE(imm, rs1, FUNCT3_LW, rd, OPC_LOAD)
#define FLW(imm, rs1, rd) I_TYPE(imm, rs1, FUNCT3_LW, rd, OPC_LOAD_FP)
#define FLD(imm, rs1, rd) I_TYPE(imm, rs1, FUNCT3_LD, rd, OPC_LOAD_FP)

#define IMM12_ECALL (0b000000000000)
#define IMM12_EBREAK (0b000000000001)

#define ECALL I_TYPE(IMM12_ECALL, 0b00000, 0b000, 0b00000, OPC_SYSTEM)
#define EBREAK I_TYPE(IMM12_EBREAK, 0b00000, 0b000, 0b00000, OPC_SYSTEM)

#endif
