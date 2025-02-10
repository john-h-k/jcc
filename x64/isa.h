#ifndef X64_ISA_H
#define X64_ISA_H

#include <stddef.h>
#include <stdint.h>

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
  U8((0b0100 << 4) | (IMM((W), 1) << 3) | (IMM((R), 1) << 2) |                 \
     (IMM((X), 1) << 1) | IMM((B), 1))

#define NEEDS_REX(reg) ((reg).ty == X64_REG_TY_R || (reg).ty == X64_REG_TY_RD)
#define REX_W(reg) ((reg).ty == X64_REG_TY_R ? (size_t)1 : (size_t)0)

#define MOD_REG ((size_t)0b11)
#define MOD_RM_IMM8 ((size_t)0b01)
#define MOD_RM_IMM32 ((size_t)0b10)

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

#define ALU_RM32(opc, dest, rhs)                                               \
  ((struct x64_raw_instr){                                                     \
      .len = 2, .buff = {(opc), MODRM(MOD_REG, (rhs).idx, (dest).idx)}})

#define ALU_RM32_REX(opc, dest, rhs)                                           \
  ((struct x64_raw_instr){                                                     \
      .len = 3,                                                                \
      .buff = {REX((size_t)0, (size_t)((rhs).ty == X64_REG_TY_RD), (size_t)0,  \
                   (size_t)((dest).ty == X64_REG_TY_RD)),                      \
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

#define EOR_REG(dest, rhs) ALU_RM((size_t)0x31, (dest), (rhs))
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

#define IDIV_REG32(rhs) \
  ((struct x64_raw_instr){                                                     \
      .len = 2,                                                                \
      .buff = { U8(0xF7),                                  \
               MODRM(MOD_REG, (size_t)0b111, (rhs).idx % 8)}})

#define IDIV_REG64(rhs) \
  ((struct x64_raw_instr){                                                     \
      .len = 3,                                                                \
      .buff = {REX(REX_W(rhs), (size_t)0, (size_t)0, (size_t)((rhs).idx >= 8)),            \
                   U8(0xF7),                                  \
               MODRM(MOD_REG, (size_t)0b111, (rhs).idx % 8)}})

#define DIV_REG(rhs) \
  NEEDS_REX((rhs)) ? DIV_REG64((rhs)) : DIV_REG32((rhs))

#define IDIV_REG(rhs) \
  NEEDS_REX((rhs)) ? IDIV_REG64((rhs)) : IDIV_REG32((rhs))


#define MOVSX8_32_NOREX(dest, source) \
  ((struct x64_raw_instr){                                                     \
      .len = 4,                                                                \
      .buff = { 0x0F, 0xBE,   \
               MODRM(MOD_REG, (source).idx % 8, (dest).idx % 8)}})

#define MOVSX16_32_NOREX(dest, source) \
  ((struct x64_raw_instr){                                                     \
      .len = 4,                                                                \
      .buff = { 0x0F, 0xBF,   \
               MODRM(MOD_REG, (source).idx % 8, (dest).idx % 8)}})

#define MOVSX8_32(dest, source) NEEDS_REX((dest)) || NEEDS_REX((source)) ? MOVSX8_64((dest), (source)) : MOVSX8_32_NOREX((dest), (source)) 
#define MOVSX16_32(dest, source) NEEDS_REX((dest)) || NEEDS_REX((source)) ? MOVSX16_64((dest), (source)) : MOVSX16_32_NOREX((dest), (source)) 


#define MOVSX8_64(dest, source) \
  ((struct x64_raw_instr){                                                     \
      .len = 4,                                                                \
      .buff = {REX(REX_W(dest), (size_t)((source).idx > 7), (size_t)0,              \
                   (size_t)((dest).idx > 7)),                                  \
                0x0F, 0xBE,   \
               MODRM(MOD_REG, (source).idx % 8, (dest).idx % 8)}})

#define MOVSX16_64(dest, source) \
  ((struct x64_raw_instr){                                                     \
      .len = 4,                                                                \
      .buff = {REX(REX_W(dest), (size_t)((source).idx > 7), (size_t)0,              \
                   (size_t)((dest).idx > 7)),                                  \
                0x0F, 0xBF,   \
               MODRM(MOD_REG, (source).idx % 8, (dest).idx % 8)}})

#define MOVSX32_64(dest, source) \
  ((struct x64_raw_instr){                                                     \
      .len = 3,                                                                \
      .buff = {REX(REX_W(dest), (size_t)((source).idx > 7), (size_t)0,              \
                   (size_t)((dest).idx > 7)),                                  \
                0x63,   \
               MODRM(MOD_REG, (source).idx % 8, (dest).idx % 8)}})


#define ALU_IMM32(opc0, opc1, dest, imm)                                                           \
  ((struct x64_raw_instr){                                                     \
      .len = 6, .buff = {(opc0), MODRM(MOD_REG, (size_t)(opc1), (dest).idx), IMM_BYTES32((imm))} \
  })

#define ALU_IMM32_REX(opc0, opc1, dest, imm)                                          \
  ((struct x64_raw_instr){                                                     \
      .len = 7,                                                                \
      .buff = {                                                                \
          REX((size_t)0, (size_t)0, (size_t)0, (size_t)((dest).ty == X64_REG_TY_RD)),    \
          (opc0), \
          MODRM(MOD_REG, (size_t)(opc1), (dest).idx % 8), IMM_BYTES32(imm)}    \
  })

#define ALU_IMM64_REX(opc0, opc1, dest, imm)                                          \
  ((struct x64_raw_instr){                                                     \
      .len = 7,                                                                \
      .buff = {                                                                \
          REX((size_t)1, (size_t)0, (size_t)0, (size_t)((dest).ty == X64_REG_TY_RD)),  \
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
#define EOR_IMM(dest, imm) ALU_IMM((size_t)0x81, (size_t)0b110, (dest), (imm))
#define AND_IMM(dest, imm) ALU_IMM((size_t)0x81, (size_t)0b100, (dest), (imm))

#define MOV_IMM32(dest, imm)                                                   \
  ((struct x64_raw_instr){                                                     \
      .len = 5, .buff = {U8(0xB8 + (dest.idx)), IMM_BYTES32((imm))}})

#define MOV_IMM32_REX(dest, imm)                                               \
  ((struct x64_raw_instr){.len = 6,                                            \
                          .buff = {REX((size_t)0, (size_t)0, (size_t)0,        \
                                       (size_t)((dest).ty == X64_REG_TY_RD)),  \
                                   U8(0xB8 + (dest.idx) % 8),                  \
                                   IMM_BYTES32((imm))}})

#define MOV_IMM64_REX(dest, imm)                                               \
  ((struct x64_raw_instr){                                                     \
      .len = 6,                                                                \
      .buff = {REX((size_t)1, (size_t)0, (size_t)0, (size_t)((dest).idx > 7)), \
               U8(0xB8 + (dest.idx) % 8), IMM_BYTES32((imm))}})

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
                   (size_t)((dest).ty == X64_REG_TY_RD)),                      \
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
#define NEG_REG(dest) ALU_UNARY_RM(0xF7, 0b010, dest)

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
  (imm) < 256 ? (NEEDS_REX((reg))                                              \
                        ? MOV_MEM_IMM8_REX_SIB((opc), (reg), (addr), (imm))  \
                    : MOV_MEM_IMM8_SIB((opc), (reg), (addr), (imm)))          \
      : (NEEDS_REX((reg))                                                      \
                ? MOV_MEM_IMM32_REX_SIB((opc), (reg), (addr), (imm))         \
            : MOV_MEM_IMM32_SIB((opc), (reg), (addr), (imm)))                 \

#define MOV_MEM_IMM_NOSIB(opc, reg, addr, imm)                                    \
  (imm) < 256 ? (NEEDS_REX((reg))                                              \
                        ? MOV_MEM_IMM8_REX((opc), (reg), (addr), (imm))  \
                    : MOV_MEM_IMM8((opc), (reg), (addr), (imm)))          \
      : (NEEDS_REX((reg))                                                      \
                ? MOV_MEM_IMM32_REX((opc), (reg), (addr), (imm))         \
            : MOV_MEM_IMM32((opc), (reg), (addr), (imm)))                 \

#define MOV_MEM_IMM(opc, reg, addr, imm)                                    \
  ((NEEDS_SIB(addr)) ? MOV_MEM_IMM_SIB((opc), (reg), (addr), (imm)) : MOV_MEM_IMM_NOSIB((opc), (reg), (addr), (imm)))


#define MOV_STORE_IMM(reg, addr, imm)                                        \
  MOV_MEM_IMM((size_t)0x89, (reg), (addr), (imm))
#define MOV_LOAD_IMM(reg, addr, imm)                                         \
  MOV_MEM_IMM((size_t)0x8B, (reg), (addr), (imm))

#define JMP_REL32(disp)                                                        \
  ((struct x64_raw_instr){.len = 5,                                            \
                          .buff = {0xe9, (uint8_t)((disp) & 0xFF),             \
                                   (uint8_t)(((disp) >> 8) & 0xFF),            \
                                   (uint8_t)(((disp) >> 16) & 0xFF),           \
                                   (uint8_t)(((disp) >> 24) & 0xFF)}})

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


#define CALL_REL32(disp)                                                       \
  ((struct x64_raw_instr){.len = 5,                                            \
                          .buff = {0xe8, (uint8_t)((disp) & 0xFF),             \
                                   (uint8_t)(((disp) >> 8) & 0xFF),            \
                                   (uint8_t)(((disp) >> 16) & 0xFF),           \
                                   (uint8_t)(((disp) >> 24) & 0xFF)}})

#define CMP_REG_IMM32(reg, imm)                                                \
  ((struct x64_raw_instr){.len = 7,                                            \
                          .buff = {0x48, 0x81, (uint8_t)(0xf8 | (reg)),        \
                                   (uint8_t)((imm) & 0xFF),                    \
                                   (uint8_t)(((imm) >> 8) & 0xFF),             \
                                   (uint8_t)(((imm) >> 16) & 0xFF),            \
                                   (uint8_t)(((imm) >> 24) & 0xFF)}})

#define CMP_REG_REG(dst, src)                                                  \
  ((struct x64_raw_instr){                                                     \
      .len = 3, .buff = {0x48, 0x39, (uint8_t)(0xc0 | ((src) << 3) | (dst))}})

#define JMP_REG(reg)                                                           \
  ((struct x64_raw_instr){.len = 3,                                            \
                          .buff = {0x48, 0xFF, (uint8_t)(0xe0 | (reg))}})

#define JCC_REL8(cc, disp)                                                     \
  ((struct x64_raw_instr){                                                     \
      .len = 2, .buff = {(uint8_t)(0x70 + (cc)), (uint8_t)((disp) & 0xFF)}})

#define JCC_REL32(cc, disp)                                                    \
  ((struct x64_raw_instr){.len = 6,                                            \
                          .buff = {0x0f, (uint8_t)(0x80 + (cc)),               \
                                   (uint8_t)((disp) & 0xFF),                   \
                                   (uint8_t)(((disp) >> 8) & 0xFF),            \
                                   (uint8_t)(((disp) >> 16) & 0xFF),           \
                                   (uint8_t)(((disp) >> 24) & 0xFF)}})

#define LEA_RIP_REL32(reg, disp)                                               \
  ((struct x64_raw_instr){.len = 7,                                            \
                          .buff = {0x48, 0x8d, (uint8_t)(((reg) << 3) | 5),    \
                                   (uint8_t)((disp) & 0xFF),                   \
                                   (uint8_t)(((disp) >> 8) & 0xFF),            \
                                   (uint8_t)(((disp) >> 16) & 0xFF),           \
                                   (uint8_t)(((disp) >> 24) & 0xFF)}})

#define XOR_REG_REG(dst, src)                                                  \
  ((struct x64_raw_instr){                                                     \
      .len = 3, .buff = {0x48, 0x31, (uint8_t)(0xc0 | ((src) << 3) | (dst))}})

#define AND_REG_REG(dst, src)                                                  \
  ((struct x64_raw_instr){                                                     \
      .len = 3, .buff = {0x48, 0x21, (uint8_t)(0xc0 | ((src) << 3) | (dst))}})

#define OR_REG_REG(dst, src)                                                   \
  ((struct x64_raw_instr){                                                     \
      .len = 3, .buff = {0x48, 0x09, (uint8_t)(0xc0 | ((src) << 3) | (dst))}})

#define IMUL_REG_REG(dst, src)                                                 \
  ((struct x64_raw_instr){                                                     \
      .len = 4,                                                                \
      .buff = {0x48, 0x0f, 0xaf, (uint8_t)(0xc0 | ((dst) << 3) | (src))}})

#endif
