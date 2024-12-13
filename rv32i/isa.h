#ifndef RV32I_ISA_H
#define RV32I_ISA_H

#define RV32I_REG_SIZE (2)
#define RV32I_INSTR_SIZE (2)

#define U32(v) ((uint32_t)(v))
#define U32_S(v, hi, lo) ((v & ((1ul << (hi + 1)) - 1ul)) >> lo)

#define R_TYPE(funct7, rs2, rs1, funct3, rd, opcode)                           \
  (uint32_t)((U32(funct7) << 25) | (U32(rs2) << 20) | (U32(rs1) << 15) |       \
             (U32(funct3) << 12) | (U32(rd) << 7) | U32(opcode))

#define I_TYPE(imm12, rs1, funct3, rd, opcode)                                 \
  (uint32_t)((U32_S(imm12, 11, 0) << 20) | (U32(rs1) << 15) |                  \
             (U32(funct3) << 12) | (U32(rd) << 7) | U32(opcode))

#define S_TYPE(imm12, rs2, rs1, funct3, opcode)                                \
  (uint32_t)((U32_S(imm12, 11, 5) << 25) | (U32(rs2) << 20) |                  \
             (U32(rs1) << 15) | (U32(funct3) << 12) |                          \
             (U32_S(imm12, 4, 0) << 8) | U32(opcode))

#define B_TYPE(imm12, rs2, rs1, funct3, opcode)                                \
  (uint32_t)((U32_S(imm12, 12, 12) << 31) | (U32_S(imm12, 10, 5) << 25) |      \
             (U32(rs2) << 20) | (U32(rs1) << 15) | (U32(funct3) << 12) |       \
             (U32_S(imm12, 4, 1) << 8) | (U32_S(imm12, 11, 11)) | U32(opcode))

#define U_TYPE(imm20, rd, opcode)                                              \
  (uint32_t)((U32(imm20) << 12) | (U32(rd) << 7) | U32(opcode))

#define J_TYPE(imm20, rd, opcode)                                              \
  (uint32_t)((U32_S(imm20, 20, 20) << 31) | (U32_S(imm20, 10, 1) << 21) |      \
             (U32_S(imm20, 11, 11) << 20) | (U32_S(imm20, 19, 12) << 12) |     \
             (U32(rd) << 7) | U32(opcode))

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

#define FUNCT3_SB U32(0b000)
#define FUNCT3_SH U32(0b001)
#define FUNCT3_SW U32(0b010)

#define FUNCT3_FMV_WX U32(0b000)
#define FUNCT3_FMV_XW U32(0b000)

#define FUNCT3_FSGNJ U32(0b000)
#define FUNCT3_FSGNJN U32(0b001)
#define FUNCT3_FSGNJX U32(0b010)

#define FUNCT3_MIN U32(0b000)
#define FUNCT3_MAX U32(0b001)

/* ----------------------------- */

/* ---------- funct7 ---------- */

#define FUNCT7_ADD U32(0b0000000)
#define FUNCT7_MULDIV U32(0b0000001)

#define FUNCT7_FADD U32(0b0000000)
#define FUNCT7_FSUB U32(0b0000100)
#define FUNCT7_FMUL U32(0b0001000)
#define FUNCT7_FDIV U32(0b0001100)
#define FUNCT7_FSGNJ U32(0b0010000)
#define FUNCT7_FMINMAX U32(0b0010100)
#define FUNCT7_FSQRT U32(0b0101101)

#define FUNCT7_SUB U32(0b0100000)
#define FUNCT7_SRL U32(0b0000000)
#define FUNCT7_SRA U32(0b0100000)

#define FUNCT7_FMV_WX U32(0b1110000)
#define FUNCT7_FMV_XW U32(0b1111000)

#define ADDI(imm12, rs1, rd) I_TYPE(imm12, rs1, FUNCT3_ADD, rd, OPC_OP_IMM)
#define XORI(imm12, rs1, rd) I_TYPE(imm12, rs1, FUNCT3_XOR, rd, OPC_OP_IMM)

#define LUI(imm20, rd) U_TYPE(imm20, rd, OPC_LUI)
#define AUIPC(imm20, rd) U_TYPE(imm20, rd, OPC_AUIPC)

#define ADD(rs2, rs1, rd) R_TYPE(FUNCT7_ADD, rs2, rs1, FUNCT3_ADD, rd, OPC_OP)
#define SUB(rs2, rs1, rd) R_TYPE(FUNCT7_SUB, rs2, rs1, FUNCT3_SUB, rd, OPC_OP)

#define MUL(rs2, rs1, rd)                                                      \
  R_TYPE(FUNCT7_MULDIV, rs2, rs1, FUNCT3_MUL, rd, OPC_OP)
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

#define FMV_XW(rs1, rd)                                                        \
  R_TYPE(FUNCT7_FMV_XW, 0b00, rs1, FUNCT3_FMV_XW, rd, OPC_OP_FP)
#define FMV_WX(rs1, rd)                                                        \
  R_TYPE(FUNCT7_FMV_WX, 0b00, rs1, FUNCT3_FMV_WX, rd, OPC_OP_FP)

#define FSQRT_S(rs1, rd)                                                       \
  R_TYPE(FUNCT7_FSQRT, 0b00000, rs1, 0b000, rd, OPC_OP_FP)

#define FADD_S(rs2, rs1, rd) R_TYPE(FUNCT7_FADD, rs2, rs1, 0b000, rd, OPC_OP_FP)
#define FSUB_S(rs2, rs1, rd) R_TYPE(FUNCT7_FSUB, rs2, rs1, 0b000, rd, OPC_OP_FP)
#define FMUL_S(rs2, rs1, rd) R_TYPE(FUNCT7_FMUL, rs2, rs1, 0b000, rd, OPC_OP_FP)
#define FDIV_S(rs2, rs1, rd) R_TYPE(FUNCT7_FDIV, rs2, rs1, 0b000, rd, OPC_OP_FP)

#define FMAX_S(rs2, rs1, rd)                                                   \
  R_TYPE(FUNCT7_FMINMAX, rs2, rs1, FUNCT3_MIN, rd, OPC_OP_FP)
#define FMIN_S(rs2, rs1, rd)                                                   \
  R_TYPE(FUNCT7_FMINMAX, rs2, rs1, FUNCT3_MAX, rd, OPC_OP_FP)

#define FSGNJ_S(rs2, rs1, rd)                                                  \
  R_TYPE(FUNCT7_FSGNJ, rs2, rs1, FUNCT3_FSGNJ, rd, OPC_OP_FP)
#define FSGNJN_S(rs2, rs1, rd)                                                 \
  R_TYPE(FUNCT7_FSGNJ, rs2, rs1, FUNCT3_FSGNJN, rd, OPC_OP_FP)
#define FSGNJX_S(rs2, rs1, rd)                                                 \
  R_TYPE(FUNCT7_FSGNJ, rs2, rs1, FUNCT3_FSGNJX, rd, OPC_OP_FP)

#define SB(imm, rs2, rs1) S_TYPE(imm, rs2, rs1, FUNCT3_SB, OPC_STORE)
#define SH(imm, rs2, rs1) S_TYPE(imm, rs2, rs1, FUNCT3_SH, OPC_STORE)
#define SW(imm, rs2, rs1) S_TYPE(imm, rs2, rs1, FUNCT3_SW, OPC_STORE)
#define FSW(imm, rs2, rs1) S_TYPE(imm, rs2, rs1, FUNCT3_SW, OPC_STORE_FP)

#define LB(imm, rs1, rd) I_TYPE(imm, rs1, FUNCT3_LB, rd, OPC_LOAD)
#define LBU(imm, rs1, rd) I_TYPE(imm, rs1, FUNCT3_LBU, rd, OPC_LOAD)
#define LH(imm, rs1, rd) I_TYPE(imm, rs1, FUNCT3_LH, rd, OPC_LOAD)
#define LHU(imm, rs1, rd) I_TYPE(imm, rs1, FUNCT3_LHU, rd, OPC_LOAD)
#define LW(imm, rs1, rd) I_TYPE(imm, rs1, FUNCT3_LW, rd, OPC_LOAD)
#define FLW(imm, rs1, rd) I_TYPE(imm, rs1, FUNCT3_LW, rd, OPC_LOAD_FP)

#endif
