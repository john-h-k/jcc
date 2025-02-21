#ifndef X64_ISA_H
#define X64_ISA_H

#include <stddef.h>
#include <stdint.h>
#include "../bit_twiddle.h"

// NOTE: if you get an instruction encoding issue
// it is likely `rm=100` encoding a SIB but us not generating a SIB

#define FITS_IN_BITS(value, bitc)                                              \
  _Generic((value),                                                            \
      size_t: (((value) & ~((1ull << (bitc + 1)) - 1ull)) == 0),               \
      imm_t: (((value) & ~((1ull << (bitc + 1)) - 1ull)) == 0),                \
      simm_t: ((llabs(((simm_t)value)) & ~((1ll << (bitc)) - 1l)) == 0))

struct x64_raw_instr {
  size_t len;
  uint8_t buff[16];
};

#define U8(v) ((uint8_t)(v))
#define IMM8(v) ((uint8_t)(v))
#define IMM32(v) ((uint32_t)(v))
#define IMM64(v) ((uint64_t)(v))

#define NOP ((struct x64_raw_instr){.len = 1, .buff = {0x90}})

#define IMM(imm, bitc)                                                         \
  (DEBUG_ASSERT(FITS_IN_BITS((imm), (bitc)), "immediate did not fit!"),            \
   CLAMP_BITS((imm), (bitc)))

#define REX_W_64_OP ((size_t)0b1)
#define REX_W_32_OP ((size_t)0b0)

#define REX_R_4B_REG ((size_t)0b1)
#define REX_R_2B_REG ((size_t)0b0)

#define REX_B_4B_RM ((size_t)0b1)
#define REX_B_2B_RM ((size_t)0b0)

#define REX_X_4B_RM ((size_t)0b1)
#define REX_X_2B_RM ((size_t)0b0)

#define REX(W, R, X, B)                                                        \
  U8((0b0100 << 4) | (IMM(((size_t)W), 1) << 3) | (IMM(((size_t)R), 1) << 2) |                 \
     (IMM(((size_t)X), 1) << 1) | IMM(((size_t)B), 1))

#define NEEDS_REX(reg) ((reg).ty == X64_REG_TY_R || (reg).idx > 7)
#define NEEDS_REX_MEM(reg) ((reg).idx > 7)
#define REX_W(reg) ((reg).ty == X64_REG_TY_R ? (size_t)1 : (size_t)0)

#define MOD_REG ((size_t)0b11)
#define MOD_RM_IMM8 ((size_t)0b01)
#define MOD_RM_IMM32 ((size_t)0b10)
#define MOD_RM_MEM ((size_t)0b00)

#define MODRM(mod, reg, rm)                                                    \
  U8((IMM((mod), 2) << 6) | (IMM((reg), 3) << 3) | IMM((rm), 3))

#define SIB_SCALE_1 ((size_t)(0b00))
#define SIB_INDEX_NONE ((size_t)(0b100))
#define SIB_BASE_RSP ((size_t)(REG_IDX_SP))

#define SIB(scale, index, base)                                                \
  U8((IMM((scale), 2) << 6) | (IMM((index), 3) << 3) | IMM((base), 3))

#define NEEDS_SIB(reg) ((reg).ty == X64_REG_TY_R && (reg).idx == REG_IDX_SP)

#define NBYTE(imm, n) ((uint8_t)(((imm) >> ((n) * 8)) & 0xFF))

#define IMM_BYTES8(imm) (IMM((imm), 32), NBYTE((imm), 0))
#define IMM_BYTES16(imm) NBYTE((imm), 0), NBYTE((imm), 1)
#define IMM_BYTES32(imm)                                                       \
  NBYTE((imm), 0), NBYTE((imm), 1), NBYTE((imm), 2),         \
      NBYTE((imm), 3)

#define IMM_BYTES64(imm)                                                       \
  NBYTE((imm), 0), NBYTE((imm), 1), NBYTE((imm), 2),         \
      NBYTE((imm), 3), NBYTE((imm), 4), NBYTE((imm), 5), NBYTE((imm), 6), NBYTE((imm), 7),

#define ALU_RM32(opc, dest, rhs)                                               \
  ((struct x64_raw_instr){                                                     \
      .len = 2, .buff = {(opc), MODRM(MOD_REG, (rhs).idx, (dest).idx)}})

#define ALU_RM32_REX(opc, dest, rhs)                                           \
  ((struct x64_raw_instr){                                                     \
      .len = 3,                                                                \
      .buff = {REX((size_t)0, (size_t)((rhs).idx > 7), (size_t)0,  \
                   (size_t)((dest).idx > 7)),                      \
               (opc), MODRM(MOD_REG, (rhs).idx % 8, (dest).idx % 8)}})

#define ALU_RM64_REX(opc, dest, rhs)                                           \
  ((struct x64_raw_instr){                                                     \
      .len = 3,                                                                \
      .buff = {REX((size_t)1, (size_t)((rhs).idx > 7), (size_t)0,              \
                   (size_t)((dest).idx > 7)),                                  \
               (opc), MODRM(MOD_REG, (rhs).idx % 8, (dest).idx % 8)}})

#define ALU_RM(opc, dest, rhs)                                                 \
  ((dest).ty == X64_REG_TY_R ? ALU_RM64_REX((opc), (dest), (rhs))              \
                             : (NEEDS_REX(dest) || NEEDS_REX(rhs)              \
                                    ? ALU_RM32_REX((opc), (dest), (rhs))       \
                                    : ALU_RM32((opc), (dest), (rhs))))

#define ADD_REG(dest, rhs) ALU_RM((size_t)0x1, (dest), (rhs))
#define SUB_REG(dest, rhs) ALU_RM((size_t)0x29, (dest), (rhs))

#define XOR_REG(dest, rhs) ALU_RM((size_t)0x31, (dest), (rhs))
#define OR_REG(dest, rhs) ALU_RM((size_t)0x09, (dest), (rhs))
#define AND_REG(dest, rhs) ALU_RM((size_t)0x21, (dest), (rhs))

#define MOV_REG(dest, rhs) ALU_RM((size_t)0x89, (dest), (rhs))

#define DIV_REG32(rhs) \
  ((struct x64_raw_instr){                                                     \
      .len = 2,                                                                \
      .buff = { U8(0xF7),                                  \
               MODRM(MOD_REG, (size_t)0b110, (rhs).idx % 8)}})

#define DIV_REG64(rhs) \
  ((struct x64_raw_instr){                                                     \
      .len = 3,                                                                \
      .buff = {REX(REX_W(rhs), (size_t)0, (size_t)0, (size_t)((rhs).idx >= 8)),            \
                   U8(0xF7),                                  \
               MODRM(MOD_REG, (size_t)0b110, (rhs).idx % 8)}})

#define IMULIDIV_REG32(opc, rhs) \
  ((struct x64_raw_instr){                                                     \
      .len = 2,                                                                \
      .buff = { U8(0xF7),                                  \
               MODRM(MOD_REG, (size_t)opc, (rhs).idx % 8)}})

#define IMULIDIV_REG64(opc, rhs) \
  ((struct x64_raw_instr){                                                     \
      .len = 3,                                                                \
      .buff = {REX(REX_W(rhs), (size_t)0, (size_t)0, (size_t)((rhs).idx >= 8)),            \
                   U8(0xF7),                                  \
               MODRM(MOD_REG, (size_t)opc, (rhs).idx % 8)}})

#define DIV_REG(rhs) \
  NEEDS_REX((rhs)) ? DIV_REG64((rhs)) : DIV_REG32((rhs))

#define IDIV_REG(rhs) \
  NEEDS_REX((rhs)) ? IMULIDIV_REG64(0b111, (rhs)) : IMULIDIV_REG32(0b111, (rhs))

#define IMUL_REG(rhs) \
  NEEDS_REX((rhs)) ? IMULIDIV_REG64(0b100, (rhs)) : IMULIDIV_REG32(0b100, (rhs))


#define MOVSX8_32_NOREX(dest, source) \
  ((struct x64_raw_instr){                                                     \
      .len = 4,                                                                \
      .buff = { 0x0F, 0xBE,   \
               MODRM(MOD_REG, (dest).idx % 8, (source).idx % 8)}})

#define MOVSX16_32_NOREX(dest, source) \
  ((struct x64_raw_instr){                                                     \
      .len = 4,                                                                \
      .buff = { 0x0F, 0xBF,   \
               MODRM(MOD_REG, (dest).idx % 8, (source).idx % 8)}})

#define MOVSX8_32(dest, source) NEEDS_REX((source)) || NEEDS_REX((dest)) ? MOVSX8_64((source), (dest)) : MOVSX8_32_NOREX((source), (dest)) 
#define MOVSX16_32(dest, source) NEEDS_REX((source)) || NEEDS_REX((dest)) ? MOVSX16_64((source), (dest)) : MOVSX16_32_NOREX((source), (dest)) 


#define MOVSX8_64(dest, source) \
  ((struct x64_raw_instr){                                                     \
      .len = 4,                                                                \
      .buff = {REX(REX_W(source), (size_t)((dest).idx > 7), (size_t)0,              \
                   (size_t)((source).idx > 7)),                                  \
                0x0F, 0xBE,   \
               MODRM(MOD_REG, (dest).idx % 8, (source).idx % 8)}})

#define MOVSX16_64(dest, source) \
  ((struct x64_raw_instr){                                                     \
      .len = 4,                                                                \
      .buff = {REX(REX_W(source), (size_t)((dest).idx > 7), (size_t)0,              \
                   (size_t)((source).idx > 7)),                                  \
                0x0F, 0xBF,   \
               MODRM(MOD_REG, (dest).idx % 8, (source).idx % 8)}})

#define MOVSX32_64(dest, source) \
  ((struct x64_raw_instr){                                                     \
      .len = 3,                                                                \
      .buff = {REX(REX_W(source), (size_t)((dest).idx > 7), (size_t)0,              \
                   (size_t)((source).idx > 7)),                                  \
                0x63,   \
               MODRM(MOD_REG, (dest).idx % 8, (source).idx % 8)}})


#define ALU_IMM32(opc0, opc1, dest, imm)                                                           \
  ((struct x64_raw_instr){                                                     \
      .len = 6, .buff = {(opc0), MODRM(MOD_REG, (size_t)(opc1), (dest).idx), IMM_BYTES32((imm))} \
  })

#define ALU_IMM32_REX(opc0, opc1, dest, imm)                                          \
  ((struct x64_raw_instr){                                                     \
      .len = 7,                                                                \
      .buff = {                                                                \
          REX((size_t)0, (size_t)0, (size_t)0, (size_t)((dest).idx > 7)),    \
          (opc0), \
          MODRM(MOD_REG, (size_t)(opc1), (dest).idx % 8), IMM_BYTES32(imm)}    \
  })

#define ALU_IMM64_REX(opc0, opc1, dest, imm)                                          \
  ((struct x64_raw_instr){                                                     \
      .len = 7,                                                                \
      .buff = {                                                                \
          REX((size_t)1, (size_t)0, (size_t)0, (size_t)((dest).idx > 7)),  \
          (opc0),\
          MODRM(MOD_REG, (opc1), (dest).idx % 8), IMM_BYTES32(imm)}})

#define ALU_IMM(opc0, opc1, dest, imm)                                                \
  ((dest).ty == X64_REG_TY_R                                                   \
       ? ALU_IMM64_REX((opc0), (opc1), (dest), (imm))                                   \
       : (NEEDS_REX(dest) ? ALU_IMM32_REX((opc0), (opc1), (dest), (imm))                \
                          : ALU_IMM32((opc0), (opc1), (dest), (imm))))

#define ADD_IMM(dest, imm) ALU_IMM((size_t)0x81, (size_t)0b000, (dest), (imm))
#define SUB_IMM(dest, imm) ALU_IMM((size_t)0x81, (size_t)0b101, (dest), (imm))

#define OR_IMM(dest, imm) ALU_IMM((size_t)0x81, (size_t)0b001, (dest), (imm))
#define XOR_IMM(dest, imm) ALU_IMM((size_t)0x81, (size_t)0b110, (dest), (imm))
#define AND_IMM(dest, imm) ALU_IMM((size_t)0x81, (size_t)0b100, (dest), (imm))

#define MOV_IMM32(dest, imm)                                                   \
  ((struct x64_raw_instr){                                                     \
      .len = 5, .buff = {U8(0xB8 + (dest).idx), IMM_BYTES32((imm))}})

#define MOV_IMM32_REX(dest, imm)                                               \
  ((struct x64_raw_instr){.len = 6,                                            \
                          .buff = {REX((size_t)0, (size_t)0, (size_t)0,        \
                                       (size_t)((dest).idx > 7)),  \
                                   U8(0xB8 + ((dest).idx % 8)),                  \
                                   IMM_BYTES32((imm))}})

#define MOV_IMM64_REX(dest, imm)                                               \
  ((struct x64_raw_instr){                                                     \
      .len = 10,                                                                \
      .buff = {REX((size_t)1, (size_t)0, (size_t)0, (size_t)((dest).idx > 7)), \
               U8(0xB8 + ((dest).idx % 8)), IMM_BYTES64((imm))}})

#define MOV_IMM(dest, imm)                                                     \
  ((dest).ty == X64_REG_TY_R ? MOV_IMM64_REX((dest), (imm))                    \
                             : (NEEDS_REX(dest) ? MOV_IMM32_REX((dest), (imm)) \
                                                : MOV_IMM32((dest), (imm))))

#define ALU_UNARY_RM32(opc0, opc1, dest)                                       \
  ((struct x64_raw_instr){                                                     \
      .len = 2, .buff = {(opc0), MODRM(MOD_REG, (size_t)(opc1), (dest).idx)}})

#define ALU_UNARY_RM32_REX(opc0, opc1, dest)                                   \
  ((struct x64_raw_instr){                                                     \
      .len = 3,                                                                \
      .buff = {REX((size_t)0, (size_t)0, (size_t)0,                            \
                   (size_t)((dest).idx > 7)),                      \
               (opc0), MODRM(MOD_REG, (size_t)(opc1), (dest).idx % 8)}})

#define ALU_UNARY_RM64_REX(opc0, opc1, dest)                                   \
  ((struct x64_raw_instr){                                                     \
      .len = 3,                                                                \
      .buff = {REX((size_t)1, (size_t)0, (size_t)0, (size_t)((dest).idx > 7)), \
               (opc0), MODRM(MOD_REG, (size_t)(opc1), (dest).idx % 8)}})

#define ALU_UNARY_RM(opc0, opc1, dest)                                         \
  ((dest).ty == X64_REG_TY_R                                                   \
       ? ALU_UNARY_RM64_REX((opc0), (opc1), (dest))                            \
       : (NEEDS_REX(dest) ? ALU_UNARY_RM32_REX((opc0), (opc1), (dest))         \
                          : ALU_UNARY_RM32((opc0), (opc1), (dest))))

#define NOT_REG(dest) ALU_UNARY_RM(0xF7, 0b010, dest)
#define NEG_REG(dest) ALU_UNARY_RM(0xF7, 0b011, dest)

#define SHL(dest) ALU_UNARY_RM(0xD3, 0b100, dest)
#define SHR(dest) ALU_UNARY_RM(0xD3, 0b101, dest)
#define SAR(dest) ALU_UNARY_RM(0xD3, 0b111, dest)

#define REG_W(reg) ((reg.ty == X64_REG_TY_R) ? (size_t)1 : (size_t)0)

#define MOV_MEM_IMM8(opc, reg, addr, imm)                                    \
  ((struct x64_raw_instr){                                                     \
      .len = 3,                                                                \
      .buff = {(opc), MODRM(MOD_RM_IMM8, (reg).idx % 8, (addr).idx % 8),       \
               IMM_BYTES8((imm))}})

#define MOV_MEM_IMM32(opc, reg, addr, imm)                                   \
  ((struct x64_raw_instr){                                                     \
      .len = 6,                                                                \
      .buff = {(opc), MODRM(MOD_RM_IMM32, (reg).idx % 8, (addr).idx % 8),      \
               IMM_BYTES32((imm))}})

#define MOV_MEM_IMM8_REX(opc, reg, addr, imm)                                \
  ((struct x64_raw_instr){                                                     \
      .len = 4,                                                                \
      .buff = {REX(REX_W(reg), (size_t)((reg).idx > 7), (size_t)0, (size_t)((addr).idx > 7)),  \
               (opc), MODRM(MOD_RM_IMM8, (reg).idx % 8, (addr).idx % 8),       \
               IMM_BYTES8((imm))}})

#define MOV_MEM_IMM32_REX(opc, reg, addr, imm)                               \
  ((struct x64_raw_instr){                                                     \
      .len = 7,                                                                \
      .buff = {REX(REX_W(reg), (size_t)((reg).idx > 7), (size_t)0, (size_t)((addr).idx > 7)),  \
               (opc), MODRM(MOD_RM_IMM32, (reg).idx % 8, (addr).idx % 8),      \
               IMM_BYTES32((imm))}})

#define MOV_MEM_IMM8_SIB(opc, reg, addr, imm)                                \
  ((struct x64_raw_instr){                                                     \
      .len = 4,                                                                \
      .buff = {(opc), MODRM(MOD_RM_IMM8, (reg).idx % 8, (addr).idx % 8),       \
               SIB(SIB_SCALE_1, SIB_INDEX_NONE, (addr).idx % 8),               \
               IMM_BYTES8((imm))}})

#define MOV_MEM_IMM32_SIB(opc, reg, addr, imm)                               \
  ((struct x64_raw_instr){                                                     \
      .len = 7,                                                                \
      .buff = {(opc), MODRM(MOD_RM_IMM32, (reg).idx % 8, (addr).idx % 8),      \
               SIB(SIB_SCALE_1, SIB_INDEX_NONE, (addr).idx % 8),               \
               IMM_BYTES32((imm))}})

#define MOV_MEM_IMM8_REX_SIB(opc, reg, addr, imm)                            \
  ((struct x64_raw_instr){                                                     \
      .len = 5,                                                                \
      .buff = {REX(REX_W(reg), (size_t)((reg).idx > 7), (size_t)0, (size_t)((addr).idx > 7)),  \
               (opc), MODRM(MOD_RM_IMM8, (reg).idx % 8, (addr).idx % 8),       \
               SIB(SIB_SCALE_1, SIB_INDEX_NONE, (addr).idx % 8),               \
               IMM_BYTES8((imm))}})

#define MOV_MEM_IMM32_REX_SIB(opc, reg, addr, imm)                           \
  ((struct x64_raw_instr){                                                     \
      .len = 8,                                                                \
      .buff = {REX(REX_W(reg), (size_t)((reg).idx > 7), (size_t)0, (size_t)((addr).idx > 7)),  \
               (opc), MODRM(MOD_RM_IMM32, (reg).idx % 8, (addr).idx % 8),      \
               SIB(SIB_SCALE_1, SIB_INDEX_NONE, (addr).idx % 8),               \
               IMM_BYTES32((imm))}})

#define MOV_MEM_IMM_SIB(opc, reg, addr, imm)                                    \
  ((imm) < 256 ? (NEEDS_REX((reg)) || NEEDS_REX((addr))                                              \
                        ? MOV_MEM_IMM8_REX_SIB((opc), (reg), (addr), (imm))  \
                    : MOV_MEM_IMM8_SIB((opc), (reg), (addr), (imm)))          \
      : (NEEDS_REX((reg)) || NEEDS_REX((addr)))                                                      \
                ? MOV_MEM_IMM32_REX_SIB((opc), (reg), (addr), (imm))         \
            : MOV_MEM_IMM32_SIB((opc), (reg), (addr), (imm)))                 \

#define MOV_MEM_IMM_NOSIB(opc, reg, addr, imm)                                    \
  ((imm) < 256 ? (NEEDS_REX((reg)) || NEEDS_REX((addr))                                             \
                        ? MOV_MEM_IMM8_REX((opc), (reg), (addr), (imm))  \
                    : MOV_MEM_IMM8((opc), (reg), (addr), (imm)))          \
      : (NEEDS_REX((reg)) || NEEDS_REX((addr)))                                                      \
                ? MOV_MEM_IMM32_REX((opc), (reg), (addr), (imm))         \
            : MOV_MEM_IMM32((opc), (reg), (addr), (imm)))                 \

#define MOV_MEM_IMM(opc, reg, addr, imm)                                    \
  ((NEEDS_SIB(addr)) ? MOV_MEM_IMM_SIB((opc), (reg), (addr), (imm)) : MOV_MEM_IMM_NOSIB((opc), (reg), (addr), (imm)))


#define MOV_STORE_IMM(reg, addr, imm)                                        \
  MOV_MEM_IMM((size_t)0x89, (reg), (addr), (imm))
#define MOV_LOAD_IMM(reg, addr, imm)                                         \
  MOV_MEM_IMM((size_t)0x8B, (reg), (addr), (imm))

#define MOV_MEM16_IMM8(opc, reg, addr, imm)                                    \
  ((struct x64_raw_instr){                                                     \
      .len = 4,                                                                \
      .buff = {0x66, (opc), MODRM(MOD_RM_IMM8, (reg).idx % 8, (addr).idx % 8),       \
               IMM_BYTES8((imm))}})

#define MOV_MEM16_IMM32(opc, reg, addr, imm)                                   \
  ((struct x64_raw_instr){                                                     \
      .len = 7,                                                                \
      .buff = {0x66, (opc), MODRM(MOD_RM_IMM32, (reg).idx % 8, (addr).idx % 8),      \
               IMM_BYTES32((imm))}})

#define MOV_MEM16_IMM8_REX(opc, reg, addr, imm)                                \
  ((struct x64_raw_instr){                                                     \
      .len = 5,                                                                \
      .buff = {0x66, REX(REX_W(reg), (size_t)((reg).idx > 7), (size_t)0, (size_t)((addr).idx > 7)),  \
               (opc), MODRM(MOD_RM_IMM8, (reg).idx % 8, (addr).idx % 8),       \
               IMM_BYTES8((imm))}})

#define MOV_MEM16_IMM32_REX(opc, reg, addr, imm)                               \
  ((struct x64_raw_instr){                                                     \
      .len = 8,                                                                \
      .buff = {0x66, REX(REX_W(reg), (size_t)((reg).idx > 7), (size_t)0, (size_t)((addr).idx > 7)),  \
               (opc), MODRM(MOD_RM_IMM32, (reg).idx % 8, (addr).idx % 8),      \
               IMM_BYTES32((imm))}})

#define MOV_MEM16_IMM8_SIB(opc, reg, addr, imm)                                \
  ((struct x64_raw_instr){                                                     \
      .len = 5,                                                                \
      .buff = {0x66, (opc), MODRM(MOD_RM_IMM8, (reg).idx % 8, (addr).idx % 8),       \
               SIB(SIB_SCALE_1, SIB_INDEX_NONE, (addr).idx % 8),               \
               IMM_BYTES8((imm))}})

#define MOV_MEM16_IMM32_SIB(opc, reg, addr, imm)                               \
  ((struct x64_raw_instr){                                                     \
      .len = 8,                                                                \
      .buff = {0x66, (opc), MODRM(MOD_RM_IMM32, (reg).idx % 8, (addr).idx % 8),      \
               SIB(SIB_SCALE_1, SIB_INDEX_NONE, (addr).idx % 8),               \
               IMM_BYTES32((imm))}})

#define MOV_MEM16_IMM8_REX_SIB(opc, reg, addr, imm)                            \
  ((struct x64_raw_instr){                                                     \
      .len = 6,                                                                \
      .buff = {0x66, REX(REX_W(reg), (size_t)((reg).idx > 7), (size_t)0, (size_t)((addr).idx > 7)),  \
               (opc), MODRM(MOD_RM_IMM8, (reg).idx % 8, (addr).idx % 8),       \
               SIB(SIB_SCALE_1, SIB_INDEX_NONE, (addr).idx % 8),               \
               IMM_BYTES8((imm))}})

#define MOV_MEM16_IMM32_REX_SIB(opc, reg, addr, imm)                           \
  ((struct x64_raw_instr){                                                     \
      .len = 9,                                                                \
      .buff = {0x66, REX(REX_W(reg), (size_t)((reg).idx > 7), (size_t)0, (size_t)((addr).idx > 7)),  \
               (opc), MODRM(MOD_RM_IMM32, (reg).idx % 8, (addr).idx % 8),      \
               SIB(SIB_SCALE_1, SIB_INDEX_NONE, (addr).idx % 8),               \
               IMM_BYTES32((imm))}})

#define MOV_MEM16_IMM_SIB(opc, reg, addr, imm)                                    \
  ((imm) < 256 ? (NEEDS_REX((reg)) || NEEDS_REX((addr))                                             \
                        ? MOV_MEM16_IMM8_REX_SIB((opc), (reg), (addr), (imm))  \
                    : MOV_MEM16_IMM8_SIB((opc), (reg), (addr), (imm)))          \
      : (NEEDS_REX((reg)) || NEEDS_REX((addr)))                                                      \
                ? MOV_MEM16_IMM32_REX_SIB((opc), (reg), (addr), (imm))         \
            : MOV_MEM16_IMM32_SIB((opc), (reg), (addr), (imm)))                 \

#define MOV_MEM16_IMM_NOSIB(opc, reg, addr, imm)                                    \
  ((imm) < 256 ? (NEEDS_REX((reg)) || NEEDS_REX((addr))                                          \
                        ? MOV_MEM16_IMM8_REX((opc), (reg), (addr), (imm))  \
                    : MOV_MEM16_IMM8((opc), (reg), (addr), (imm)))          \
      : (NEEDS_REX((reg)) || NEEDS_REX((addr)))                                                      \
                ? MOV_MEM16_IMM32_REX((opc), (reg), (addr), (imm))         \
            : MOV_MEM16_IMM32((opc), (reg), (addr), (imm)))                 \



#define MOV_MEM16_IMM(opc, reg, addr, imm)                                    \
  ((NEEDS_SIB(addr)) ? MOV_MEM16_IMM_SIB((opc), (reg), (addr), (imm)) : MOV_MEM16_IMM_NOSIB((opc), (reg), (addr), (imm)))

#define MOV_STORE_HALF_IMM(reg, addr, imm)                                        \
  MOV_MEM16_IMM((size_t)0x89, (reg), (addr), (imm))

#define MOV_STORE_BYTE_IMM(reg, addr, imm)                                        \
  MOV_MEM_IMM((size_t)0x88, (reg), (addr), (imm))

#define MOVZX_LOAD_IMM_NOREX(opc, dest, addr, imm)                                         \
  ((struct x64_raw_instr){.len = 3,                                            \
                          .buff = {0x0F, (opc), MODRM(MOD_RM_MEM, (size_t)((dest).idx), (size_t)((addr).idx)) }})

#define MOVZX_LOAD_IMM_REX(opc, dest, addr, imm)                                         \
  ((struct x64_raw_instr){.len = 4,                                            \
                          .buff = { \
                          REX(REX_W(dest), (size_t)((dest).idx > 7), (size_t)0, (size_t)((addr).idx > 7)),\
                          0x0F, (opc), MODRM(MOD_RM_MEM, (size_t)((dest).idx), (size_t)((addr).idx)) }})

#define MOVZX_LOAD_HALF_IMM(reg, addr, imm)                                         \
  (NEEDS_REX((reg)) || NEEDS_REX_MEM((addr)) ? MOVZX_LOAD_IMM_REX(0xB7, (reg), (addr), (imm)) : MOVZX_LOAD_IMM_NOREX(0xB7, (reg), (addr), (imm)))

#define MOVZX_LOAD_BYTE_IMM(reg, addr, imm)                                         \
  (NEEDS_REX((reg)) || NEEDS_REX_MEM((addr)) ? MOVZX_LOAD_IMM_REX(0xB6, (reg), (addr), (imm)) : MOVZX_LOAD_IMM_NOREX(0xB6, (reg), (addr), (imm)))

#define JMP_REL8(disp)                                                        \
  ((struct x64_raw_instr){.len = 2,                                            \
                          .buff = {0xEB, IMM_BYTES8(disp) }})

#define JMP_REL32(disp)                                                        \
  ((struct x64_raw_instr){.len = 5,                                            \
                          .buff = {0xE9, IMM_BYTES32(disp) }})

// #define JMP_REL(disp) (disp) < 256 ? JMP_REL8((disp)) : JMP_REL32((disp))
#define JMP_REL() JMP_REL32((size_t)(0x0))

#define JMP_REG_BASE(reg)                                                        \
  ((struct x64_raw_instr){.len = 2,                                            \
                          .buff = {0xFF, MODRM(MOD_RM_MEM, (size_t)0b100, (reg).idx) }})
#define JMP_REG_REX(reg)                                                        \
  ((struct x64_raw_instr){.len = 3,                                            \
                          .buff = {REX_PREFIX(1, 0, 0, 1), 0xFF, MODRM(MOD_RM_MEM, (size_t)0b100, (reg).idx % 8) }})

#define JMP_REG(reg) (reg).idx < 8 ? JMP_REG_BASE((reg)) : JMP_REG_REX((reg))

#define JMP_COND_REL8(cc, disp)                                                        \
  ((struct x64_raw_instr){.len = 2,                                            \
                          .buff = {0x70 + U8(cc), IMM_BYTES8(disp) }})

#define JMP_COND_REL32(cc, disp)                                                        \
  ((struct x64_raw_instr){.len = 6,                                            \
                          .buff = {0x0F, 0x80 + U8(cc), IMM_BYTES32(disp) }})

// #define JMP_COND_REL(cc, disp) (disp) < 256 ? JMP_COND_REL8((cc), (disp)) : JMP_COND_REL32((cc), (disp))
#define JMP_COND_REL(cc) JMP_COND_REL32((cc), (0x0))

#define SET_COND(cc, dest)  \
  ((dest).idx < 4 ? \
  ((struct x64_raw_instr){.len = 3,                                            \
                          .buff = {0x0F, 0x90 + U8(cc), MODRM(MOD_REG, (size_t)0, (size_t)((dest).idx))}}) :  \
    ((struct x64_raw_instr){.len = 4,                                            \
                          .buff = { REX((size_t)1, (size_t)0, (size_t)0, (size_t)(((dest).idx) > 8)),  \
                          0x0F, 0x90 + U8(cc), MODRM(MOD_REG, (size_t)0, (size_t)((dest).idx) % 8)}}))  \
 
#define RET ((struct x64_raw_instr){.len = 1, .buff = {0xc3}})

#define PUSH_REG64(reg) \
  (reg).idx < 8 ? \
  ((struct x64_raw_instr){.len = 1,                                            \
                          .buff = {0x50 + U8((reg).idx)}}) :  \
    ((struct x64_raw_instr){.len = 2,                                            \
                              .buff = {0x41, 0x50 + U8((reg).idx)}}) 

#define POP_REG64(reg) \
  (reg).idx < 8 ? \
  ((struct x64_raw_instr){.len = 1,                                            \
                          .buff = {0x58 + U8((reg).idx)}}) :  \
    ((struct x64_raw_instr){.len = 2,                                            \
                              .buff = {0x41, 0x58 + U8((reg).idx)}}) 

#define TEST_REG(lhs, rhs) \
  (NEEDS_REX((lhs)) || NEEDS_REX((rhs)) ? \
  ((struct x64_raw_instr){.len = 3,                                            \
                          .buff = {REX(REX_W(lhs), (size_t)((rhs).idx > 7), (size_t)0, (size_t)((lhs).idx > 7)),  \
                          0x85, MODRM(MOD_REG, (rhs).idx % 8 , (lhs).idx % 8) }}) \
                            : \
    ((struct x64_raw_instr){.len = 2,                                            \
                          .buff = {0x85, MODRM(MOD_REG, (rhs).idx, (lhs).idx) }})) 

                          // 0x3B, 
#define CMP_REG(lhs, rhs) \
  (NEEDS_REX((lhs)) || NEEDS_REX((rhs)) ? \
  ((struct x64_raw_instr){.len = 3,                                            \
                          .buff = { \
                          REX(REX_W(lhs), (size_t)((lhs).idx > 7), (size_t)0, (size_t)((rhs).idx > 7)),  \
                          0x3B, MODRM(MOD_REG, (lhs).idx % 8 , (rhs).idx % 8) }}) \
                            : \
    ((struct x64_raw_instr){.len = 2,                                            \
                          .buff = {0x3B, MODRM(MOD_REG, (lhs).idx, (rhs).idx) }})) 

#define CALL_REL32(disp)                                                       \
  ((struct x64_raw_instr){.len = 5,                                            \
                          .buff = {0xE8, IMM_BYTES32((disp)) }})

#define CALL_REG(reg)                                                       \
  ((reg).idx ? \
  ((struct x64_raw_instr){.len = 3,                                            \
                          .buff = { \
                          REX((size_t)1, (size_t)((reg).idx > 7), (size_t)0, (size_t)(0)),  \
                          0xFF, \
                          MODRM(MOD_REG, (size_t)2, (reg).idx % 8) }}) \
                          : \
  ((struct x64_raw_instr){.len = 2,                                            \
                          .buff = {0xFF, MODRM(MOD_REG, (size_t)2, (reg).idx % 8) }}))


#define LEA_PCREL(dest, offset) \
  ((struct x64_raw_instr){.len = 7,                                            \
                          .buff = { \
                          REX((size_t)1, (size_t)((dest.idx) > 7), (size_t)0, (size_t)0), \
                          0x8D, \
                          MODRM(MOD_RM_MEM, ((dest).idx % 8), (size_t)0b101), \
                          IMM_BYTES32(offset) \
                          }}) \

#define LEA_REG64(dest, index, base, scale, offset) \
  ((offset) == 0 \
    ? LEA_REG64_IMM0((dest), (index), (base), (scale), (offset))  \
    : ((offset) < 256) ? LEA_REG64_IMM8((dest), (index), (base), (scale), (offset))  \
      : LEA_REG64_IMM32((dest), (index), (base), (scale), (offset))  \
    )

#define LEA_REG64_IMM0(dest, index, base, scale, offset)                                                       \
  ((struct x64_raw_instr){.len = 4,                                            \
                          .buff = { \
                          REX((size_t)1, (size_t)((dest).idx > 7), (size_t)((index).idx > 7), (size_t)((base).idx > 7)),  \
                          0x8D, \
                          MODRM(MOD_RM_MEM, ((dest).idx % 8), (size_t)0b100), \
                          SIB(scale, ((index).idx % 8), ((base).idx % 8)) \
                          }}) \

#define LEA_REG64_IMM8(dest, index, base, scale, offset)                                                       \
  ((struct x64_raw_instr){.len = 5,                                            \
                          .buff = { \
                          REX((size_t)1, (size_t)((dest).idx > 7), (size_t)((index).idx > 7), (size_t)((base).idx > 7)),  \
                          0x8D, \
                          MODRM(MOD_RM_IMM8, ((dest).idx % 8), (size_t)0b100), \
                          SIB(scale, ((index).idx % 8), ((base).idx % 8)), \
                          IMM_BYTES8(offset) \
                          }}) \

#define LEA_REG64_IMM32(dest, index, base, scale, offset)                                                       \
  ((struct x64_raw_instr){.len = 8,                                            \
                          .buff = { \
                          REX((size_t)1, (size_t)((dest).idx > 7), (size_t)((index).idx > 7), (size_t)((base).idx > 7)),  \
                          0x8D, \
                          MODRM(MOD_RM_IMM32, ((dest).idx % 8), (size_t)0b100), \
                          SIB(scale, ((index).idx % 8), ((base).idx % 8)), \
                          IMM_BYTES32(offset) \
                          }}) \

#define LEA_NOIDX_REG64(dest, base, offset) \
  ((offset) == 0 \
    ? LEA_NOIDX_REG64_IMM0((dest), (base), (offset))  \
    : ((offset) < 256) ? LEA_NOIDX_REG64_IMM8((dest), (base), (offset))  \
      : LEA_NOIDX_REG64_IMM32((dest), (base), (offset))  \
    )

#define LEA_NOIDX_REG64_IMM0(dest, base, offset)                                                       \
  ((struct x64_raw_instr){.len = 3,                                            \
                          .buff = { \
                          REX((size_t)1, (size_t)((dest).idx > 7), (size_t)0, (size_t)((base).idx > 7)),  \
                          0x8D, \
                          MODRM(MOD_RM_MEM, ((dest).idx % 8), ((base).idx % 8)), \
                          }}) \

#define LEA_NOIDX_REG64_IMM8(dest, base, offset)                                                       \
  ((struct x64_raw_instr){.len = 4,                                            \
                          .buff = { \
                          REX((size_t)1, (size_t)((dest).idx > 7), (size_t)0, (size_t)((base).idx > 7)),  \
                          0x8D, \
                          MODRM(MOD_RM_IMM8, ((dest).idx % 8), ((base).idx % 8)), \
                          IMM_BYTES8(offset) \
                          }}) \

#define LEA_NOIDX_REG64_IMM32(dest, base, offset)                                                       \
  ((struct x64_raw_instr){.len = 7,                                            \
                          .buff = { \
                          REX((size_t)1, (size_t)((dest).idx > 7), (size_t)0, (size_t)((base).idx > 7)),  \
                          0x8D, \
                          MODRM(MOD_RM_IMM32, ((dest).idx % 8), ((base).idx % 8)), \
                          IMM_BYTES32(offset) \
                          }}) \


#define SSE_MOVQ(opc, dest, source) \
  (NEEDS_REX((dest)) || NEEDS_REX((source)) ?\
  ((struct x64_raw_instr){.len = 5,                                            \
                          .buff = { \
                          0x66,\
                          REX((size_t)1, (size_t)((dest).idx > 7), (size_t)0, (size_t)((source).idx > 7)),  \
                          0x0F, \
                          (opc), \
                          MODRM(MOD_REG, ((dest).idx % 8), ((source).idx % 8)) }}) \
                          : \
  ((struct x64_raw_instr){.len = 5,                                            \
                          .buff = { \
                          0x66,\
                          REX((size_t)1, (size_t)((dest).idx > 7), (size_t)0, (size_t)((source).idx > 7)),  \
                          0x0F, \
                          (opc), \
                          MODRM(MOD_REG, ((dest).idx % 8), ((source).idx % 8)) }}))

#define AVX_MOVQ(dest, source) \
  ((struct x64_raw_instr){.len = 5,                                            \
                          .buff = { \
                          0x66,\
                          REX((size_t)1, (size_t)((dest).idx > 7), (size_t)0, (size_t)((source).idx > 7)),  \
                          0x0F, \
                          (opc), \
                          MODRM(MOD_REG, ((dest).idx % 8), ((source).idx % 8)) }})

#define SSE_MOVD(opc, dest, source) \
  (NEEDS_REX((dest)) || NEEDS_REX((source)) ?\
  ((struct x64_raw_instr){.len = 4,                                            \
                          .buff = { \
                          0x66,\
                          REX((size_t)0, (size_t)((dest).idx > 7), (size_t)0, (size_t)((source).idx > 7)),  \
                          0x0F, \
                          (opc), \
                          MODRM(MOD_REG, ((dest).idx % 8), ((source).idx % 8)) }}) \
                          : \
  ((struct x64_raw_instr){.len = 4,                                            \
                          .buff = { \
                          0x66,\
                          0x0F, \
                          (opc), \
                          MODRM(MOD_REG, ((dest).idx % 8), ((source).idx % 8)) }}))

#define AVX_MOVD(dest, source) \
  ((struct x64_raw_instr){.len = 4,                                            \
                          .buff = { \
                          0x66,\
                          REX((size_t)0, (size_t)((dest).idx > 7), (size_t)0, (size_t)((source).idx > 7)),  \
                          0x0F, \
                          0x6E, \
                          MODRM(MOD_REG, ((dest).idx % 8), ((source).idx % 8)) }})

#define SCALAR_SINGLE ((size_t)0xF3)
#define SCALAR_DOUBLE ((size_t)0xF2)
#define PACKED_DOUBLE ((size_t)0x66)

#define SSE_INSTR_PS(opc, dest, source) \
  (NEEDS_REX((dest)) || NEEDS_REX((source)) ?\
   ((struct x64_raw_instr){.len = 4,                                            \
                          .buff = { \
                          REX((size_t)1, (size_t)((dest).idx > 7), (size_t)0, (size_t)((source).idx > 7)),  \
                          0x0F, \
                          opc, \
                          MODRM(MOD_REG, ((dest).idx % 8), ((source).idx % 8)) }}) \
    : \
   ((struct x64_raw_instr){.len = 3,                                            \
                          .buff = { \
                          0x0F, \
                          opc, \
                          MODRM(MOD_REG, ((dest).idx % 8), ((source).idx % 8)) }}))

#define SSE_INSTR_NON_PS(opc, pref, dest, source) \
  (NEEDS_REX((dest)) || NEEDS_REX((source)) ?\
   ((struct x64_raw_instr){.len = 5,                                            \
                          .buff = { \
                          U8(pref), \
                          REX((size_t)1, (size_t)((dest).idx > 7), (size_t)0, (size_t)((source).idx > 7)),  \
                          0x0F, \
                          opc, \
                          MODRM(MOD_REG, ((dest).idx % 8), ((source).idx % 8)) }}) \
    : \
   ((struct x64_raw_instr){.len = 4,                                            \
                          .buff = { \
                          U8(pref), \
                          0x0F, \
                          opc, \
                          MODRM(MOD_REG, ((dest).idx % 8), ((source).idx % 8)) }}))

#define SSE_INSTR_PS_IMM32(opc, dest, source, imm) \
  (NEEDS_REX((dest)) || NEEDS_REX((source)) ?\
   ((struct x64_raw_instr){.len = 9,                                            \
                          .buff = { \
                          REX((size_t)1, (size_t)((dest).idx > 7), (size_t)0, (size_t)((source).idx > 7)),  \
                          0x0F, \
                          opc, \
                          MODRM(MOD_RM_IMM32, ((dest).idx % 8), ((source).idx % 8)), \
                          SIB(SIB_SCALE_1, SIB_INDEX_NONE, (source).idx % 8),               \
                          IMM_BYTES32(imm) \
                          }}) \
    : \
   ((struct x64_raw_instr){.len = 8,                                            \
                          .buff = { \
                          0x0F, \
                          opc, \
                          MODRM(MOD_RM_IMM32, ((dest).idx % 8), ((source).idx % 8)), \
                          SIB(SIB_SCALE_1, SIB_INDEX_NONE, (source).idx % 8),               \
                          IMM_BYTES32(imm) \
                          }}))

#define SSE_INSTR_NON_PS_IMM32(opc, pref, dest, source, imm) \
  (NEEDS_REX((dest)) || NEEDS_REX((source)) ?\
   ((struct x64_raw_instr){.len = 10,                                         \
                          .buff = { \
                          U8(pref), \
                          REX((size_t)0, (size_t)((dest).idx > 7), (size_t)0, (size_t)((source).idx > 7)),  \
                          0x0F, \
                          opc, \
                          MODRM(MOD_RM_IMM32, ((dest).idx % 8), ((source).idx % 8)), \
                          SIB(SIB_SCALE_1, SIB_INDEX_NONE, (source).idx % 8),               \
                          IMM_BYTES32(imm) \
                          }}) \
    : \
   ((struct x64_raw_instr){.len = 9,                                            \
                          .buff = { \
                          U8(pref), \
                          0x0F, \
                          opc, \
                          MODRM(MOD_RM_IMM32, ((dest).idx % 8), ((source).idx % 8)), \
                          SIB(SIB_SCALE_1, SIB_INDEX_NONE, (source).idx % 8),               \
                          IMM_BYTES32(imm) \
                           }}))

#define SSE_INSTR_NON_PS_MEM_REX_IMM32(opc, pref, dest, source, imm) \
  (NEEDS_SIB((dest)) || NEEDS_SIB((source)) ? \
   ((struct x64_raw_instr){.len = 10,                                         \
                          .buff = { \
                          U8(pref), \
                          REX((size_t)0, (size_t)((dest).idx > 7), (size_t)0, (size_t)((source).idx > 7)),  \
                          0x0F, \
                          opc, \
                          MODRM(MOD_RM_IMM32, ((dest).idx % 8), ((source).idx % 8)), \
                          SIB(SIB_SCALE_1, SIB_INDEX_NONE, (source).idx % 8),               \
                          IMM_BYTES32(imm) \
                          }}) \
    : \
   ((struct x64_raw_instr){.len = 9,                                            \
                          .buff = { \
                          U8(pref), \
                          REX((size_t)0, (size_t)((dest).idx > 7), (size_t)0, (size_t)((source).idx > 7)),  \
                          0x0F, \
                          opc, \
                          MODRM(MOD_RM_IMM32, ((dest).idx % 8), ((source).idx % 8)), \
                          IMM_BYTES32(imm) \
                           }}))

#define SSE_INSTR_NON_PS_MEM_NOREX_IMM32(opc, pref, dest, source, imm) \
  (NEEDS_SIB((dest)) || NEEDS_SIB((source)) ? \
   ((struct x64_raw_instr){.len = 9,                                         \
                          .buff = { \
                          U8(pref), \
                          0x0F, \
                          opc, \
                          MODRM(MOD_RM_IMM32, ((dest).idx % 8), ((source).idx % 8)), \
                          SIB(SIB_SCALE_1, SIB_INDEX_NONE, (source).idx % 8),               \
                          IMM_BYTES32(imm) \
                          }}) \
    : \
   ((struct x64_raw_instr){.len = 8,                                            \
                          .buff = { \
                          U8(pref), \
                          0x0F, \
                          opc, \
                          MODRM(MOD_RM_IMM32, ((dest).idx % 8), ((source).idx % 8)), \
                          IMM_BYTES32(imm) \
                           }}))

#define SSE_INSTR_NON_PS_MEM_IMM32(opc, pref, dest, source, imm) \
  (NEEDS_REX_MEM((dest)) || NEEDS_REX_MEM((source)) ? \
      SSE_INSTR_NON_PS_MEM_REX_IMM32(opc, pref, dest, source, imm) \
      : SSE_INSTR_NON_PS_MEM_NOREX_IMM32(opc, pref, dest, source, imm))
  

#define MOV_LOAD_SS(dest, source, imm) SSE_INSTR_NON_PS_MEM_IMM32(0x10, SCALAR_SINGLE, (dest), (source), (imm))
#define MOV_STORE_SS(dest, source, imm) SSE_INSTR_NON_PS_MEM_IMM32(0x11, SCALAR_SINGLE, (dest), (source), (imm))
#define MOV_LOAD_SD(dest, source, imm) SSE_INSTR_NON_PS_MEM_IMM32(0x10, SCALAR_DOUBLE, (dest), (source), (imm))
#define MOV_STORE_SD(dest, source, imm) SSE_INSTR_NON_PS_MEM_IMM32(0x11, SCALAR_DOUBLE, (dest), (source), (imm))

#define MOVAPS(dest, source) SSE_INSTR_PS(0x28, (dest), (source))
#define MOVAPD(dest, source) SSE_INSTR_NON_PS(0x28, PACKED_DOUBLE, (dest), (source))

#define ANDPS(dest, source) SSE_INSTR_PS(0x54, (dest), (source))
#define ORPS(dest, source) SSE_INSTR_PS(0x56, (dest), (source))
#define XORPS(dest, source) SSE_INSTR_PS(0x57, (dest), (source))

#define ANDPD(dest, source) SSE_INSTR_NON_PS(0x54, PACKED_DOUBLE, (dest), (source))
#define ORPD(dest, source) SSE_INSTR_NON_PS(0x56, PACKED_DOUBLE, (dest), (source))
#define XORPD(dest, source) SSE_INSTR_NON_PS(0x57, PACKED_DOUBLE, (dest), (source))

#define UCOMISS(dest, source) SSE_INSTR_PS(0x2E, (dest), (source))
#define UCOMISD(dest, source) SSE_INSTR_NON_PS(0x2E, PACKED_DOUBLE, (dest), (source))

#define SQRTSS(dest, source) SSE_INSTR_NON_PS(0x51, SCALAR_SINGLE, (dest), (source))
#define SQRTSD(dest, source) SSE_INSTR_NON_PS(0x51, SCALAR_DOUBLE, (dest), (source))

#define ADDSS(dest, source) SSE_INSTR_NON_PS(0x58, SCALAR_SINGLE, (dest), (source))
#define ADDSD(dest, source) SSE_INSTR_NON_PS(0x58, SCALAR_DOUBLE, (dest), (source))

#define SUBSS(dest, source) SSE_INSTR_NON_PS(0x5C, SCALAR_SINGLE, (dest), (source))
#define SUBSD(dest, source) SSE_INSTR_NON_PS(0x5C, SCALAR_DOUBLE, (dest), (source))

#define MULSS(dest, source) SSE_INSTR_NON_PS(0x59, SCALAR_SINGLE, (dest), (source))
#define MULSD(dest, source) SSE_INSTR_NON_PS(0x59, SCALAR_DOUBLE, (dest), (source))

#define DIVSS(dest, source) SSE_INSTR_NON_PS(0x5E, SCALAR_SINGLE, (dest), (source))
#define DIVSD(dest, source) SSE_INSTR_NON_PS(0x5E, SCALAR_DOUBLE, (dest), (source))

#define CVTSI2SS(dest, source) SSE_INSTR_NON_PS(0x2A, SCALAR_SINGLE, (dest), (source))
#define CVTSI2SD(dest, source) SSE_INSTR_NON_PS(0x2A, SCALAR_DOUBLE, (dest), (source))

#define CVTSS2SI(dest, source) SSE_INSTR_NON_PS(0x2D, SCALAR_SINGLE, (dest), (source))
#define CVTSD2SI(dest, source) SSE_INSTR_NON_PS(0x2D, SCALAR_DOUBLE, (dest), (source))

#define CVTTSS2SI(dest, source) SSE_INSTR_NON_PS(0x2C, SCALAR_SINGLE, (dest), (source))
#define CVTTSD2SI(dest, source) SSE_INSTR_NON_PS(0x2C, SCALAR_DOUBLE, (dest), (source))

#define CVTSS2SD(dest, source) SSE_INSTR_NON_PS(0x5A, SCALAR_SINGLE, (dest), (source))
#define CVTSD2SS(dest, source) SSE_INSTR_NON_PS(0x5A, SCALAR_DOUBLE, (dest), (source))

#endif
