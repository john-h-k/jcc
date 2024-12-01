#ifndef RV32I_ISA_H
#define RV32I_ISA_H

#define RV32I_REG_SIZE (2)
#define RV32I_INSTR_SIZE (2)

// #define R_TYPE(funct7, rs2, rs1, funct3, rd, opcode)
// #define I_TYPE(imm12, rs1, funct3, rd, opcode)
// #define S_TYPE(imm11_5, rs2, rs1, funct3, imm4_0, opcode)
// #define B_TYPE(imm12, imm11_5, rs2, rs1, funct3, imm4_1, imm11, opcode)
// #define U_TYPE(imm31_12, rd, opcode)
// #define J_TYPE(imm20, imm10_1, imm11, imm19_12, rd, opcode)

#define U32(v) ((uint32_t)(v))
#define U32_S(v, hi, lo) ((v & ((1ul << (hi + 1)) - 1ul)) >> lo)

#define R_TYPE(funct7, rs2, rs1, funct3, rd, opcode) \
  (uint32_t)((U32(funct7) << 25) \
  | (U32(rs2) << 20) | (U32(rs1) << 15) | (U32(funct3) << 12) | (U32(rd) << 7) | U32(opcode))

#define I_TYPE(imm12, rs1, funct3, rd, opcode) \
  (uint32_t)((U32_S(imm12, 11, 0) << 20) \
  | (U32(rs1) << 15) | (U32(funct3) << 12) | (U32(rd) << 7) | U32(opcode))

#define S_TYPE(imm12, rs2, rs1, funct3, opcode) \
  (uint32_t)((U32_S(imm12, 11, 5) << 25) \
  | (U32(rs2) << 20) | (U32(rs1) << 15) | (U32(funct3) << 12) | (U32_S(imm12, 4, 0) << 8) | U32(opcode))

#define B_TYPE(imm12, rs2, rs1, funct3, opcode) \
  (uint32_t)((U32_S(imm12, 12, 12) << 31) | (U32_S(imm12, 10, 5) << 25) \
  | (U32(rs2) << 20) | (U32(rs1) << 15) | (U32(funct3) << 12) | (U32_S(imm12, 4, 1) << 8) | (U32_S(imm12, 11, 11)) | U32(opcode))

#define U_TYPE(imm20, rd, opcode) (uint32_t)((U32(imm20) << 12) | (U32(rd) << 7) | U32(opcode))

#define J_TYPE(imm20, rd, opcode) (uint32_t)

#define OPC_LOAD     U32(0b0000011)
#define OPC_LOAD_FP  U32(0b0000111)
// custom 0
#define OPC_MISC_MEM U32(0b0001111)
#define OPC_OP_IMM   U32(0b0010011)
#define OPC_AUIPC    U32(0b0010111)
#define OPC_IMM_32   U32(0b0011011)
// 48b
#define OPC_STORE    U32(0b0100011)
#define OPC_STORE_FP U32(0b0100111)
// custom 1
#define OPC_AMO      U32(0b0101111)
#define OPC_OP       U32(0b0110011)
#define OPC_LUI      U32(0b0110111)
#define OPC_32       U32(0b0111011)
// 64b
#define OPC_OP_MADD  U32(0b1000011)
#define OPC_OP_MSUB  U32(0b1000111)
#define OPC_OP_NMSUB U32(0b1001011)
#define OPC_OP_NMADD U32(0b1001111)
#define OPC_OP_FP    U32(0b1010011)
// reserved
// custom 2 / rv128
// 48b
#define OPC_BRANCH   U32(0b1100011)
#define OPC_JALR     U32(0b1100111)
// reserved
#define OPC_JAL      U32(0b1101111)
#define OPC_SYSTEM   U32(0b1110011)
// reserved
// custom 3 / rv128
// >=80b

/* ----------------------------- */


/* ---------- funct3 ---------- */

#define FUNCT3_ADDI     U32(0b000)

#define FUNCT3_ADD      U32(0b000)

#define FUNCT3_JALR     U32(0b000)

#define FUNCT3_BEQ      U32(0b000)
#define FUNCT3_BNE      U32(0b001)
#define FUNCT3_BLT      U32(0b100)
#define FUNCT3_BGE      U32(0b101)
#define FUNCT3_BLTU     U32(0b110)
#define FUNCT3_BGEU     U32(0b111)

#define FUNCT3_MUL      U32(0b000)
#define FUNCT3_MULH     U32(0b001)
#define FUNCT3_MULHU    U32(0b010)
#define FUNCT3_MULHSU   U32(0b011)
#define FUNCT3_DIV      U32(0b100)
#define FUNCT3_DIVU     U32(0b101)
#define FUNCT3_REM      U32(0b110)
#define FUNCT3_REMU     U32(0b111)

/* ----------------------------- */


/* ---------- funct7 ---------- */

#define FUNCT7_MULDIV   U32(0b0000001)
#define FUNCT7_ADD      U32(0b0000000)

#define ADDI(imm12, rs1, rd) I_TYPE(imm12, rs1, FUNCT3_ADDI, rd, OPC_OP_IMM)
#define LUI(imm20, rd) U_TYPE(imm20, rd, OPC_LUI)

#define ADD(rs2, rs1, rd) R_TYPE(FUNCT7_ADD, rs2, rs1, FUNCT3_ADD, rd, OPC_OP)
#define JALR(imm12, rs1, rd) I_TYPE(imm12, rs1, FUNCT3_JALR, rd, OPC_JALR)

#endif
