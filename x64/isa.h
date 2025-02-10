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

// #define ADD_REG()

#define MOD_REG ((size_t)0b11)

#define NOP ((struct x64_raw_instr){.len = 1, .buff = {0x90}})

#define IMM(imm, bitc)                                                         \
  (DEBUG_ASSERT(FITS_IN_BITS(imm, bitc), "immediate did not fit!"),            \
   CLAMP_BITS(imm, bitc))

#define MODRM(mod, reg, rm)                                                    \
  U8((IMM(mod, 2) << 6) | (IMM(reg, 3) << 3) | IMM(rm, 3))

#define REX_W_64_OP ((size_t)0b1)
#define REX_W_32_OP ((size_t)0b0)

#define REX_R_4B_REG ((size_t)0b1)
#define REX_R_2B_REG ((size_t)0b0)

#define REX_B_4B_RM ((size_t)0b1)
#define REX_B_2B_RM ((size_t)0b0)

#define REX_X_4B_RM ((size_t)0b1)
#define REX_X_2B_RM ((size_t)0b0)


#define NBYTE(imm, n) (uint8_t)(((imm) >> ((n) * 8)) & 0xFF)

#define IMM_BYTES32(imm) (IMM((imm), 32), NBYTE((imm), 0)), NBYTE((imm), 1), NBYTE((imm), 2), NBYTE((imm), 3)


#define REX(W, R, X, B)                                                        \
  U8((0b0100 << 4) | (IMM((W), 1) << 3) | (IMM((R), 1) << 2) |                 \
     (IMM((X), 1) << 1) | IMM((B), 1))

#define ALU_RM32(opc, dest, rhs)                                               \
  ((struct x64_raw_instr){                                                     \
      .len = 2, .buff = {(opc), MODRM(MOD_REG, (rhs).idx, (dest).idx)}})

#define ALU_RM32_REX(opc, dest, rhs)                                           \
  ((struct x64_raw_instr){                                                     \
      .len = 3,                                                                \
      .buff = {                                                                \
          REX((size_t)0, (size_t)((dest).ty == X64_REG_TY_RD), (size_t)0, (size_t)((rhs).ty == X64_REG_TY_RD)),    \
          (opc), \
          MODRM(MOD_REG, (rhs).idx % 8, (dest).idx % 8)}})

#define ALU_RM64_REX(opc, dest, rhs)                                           \
  ((struct x64_raw_instr){                                                     \
      .len = 3,                                                                \
      .buff = {REX((size_t)1, (size_t)((dest).idx > 7), (size_t)0, (size_t)((rhs).idx > 7)), (opc),                \
               MODRM(MOD_REG, (rhs).idx % 8, (dest).idx % 8)}})

#define ALU_RM(opc, dest, rhs)                                                 \
  (NEEDS_REX(dest) || NEEDS_REX(rhs)                                                   \
       ? ALU_RM64_REX((opc), (dest), (rhs))                                    \
       : ((dest).ty == X64_REG_TY_RD ? ALU_RM32_REX((opc), (dest), (rhs))      \
                                     : ALU_RM32((opc), (dest), (rhs))))

#define ADD_REG(dest, rhs) ALU_RM(0x1, (dest), (rhs))
#define SUB_REG(dest, rhs) ALU_RM(0x29, (dest), (rhs))
#define MOV_REG(dest, rhs) ALU_RM(0x89, (dest), (rhs))

#define ALU_IMM32(opc, dest, imm)                                               \
  ((struct x64_raw_instr){                                                     \
      .len = 2, .buff = {(opc), MODRM(MOD_REG, (rhs).idx, (dest).idx)}, IMM_BYTES32((imm))}})

#define ALU_IMM32_REX(opc, dest, imm)                                           \
  ((struct x64_raw_instr){                                                     \
      .len = 3,                                                                \
      .buff = {                                                                \
          REX((size_t)0, (size_t)0, (size_t)0, (size_t)((dest).ty == X64_REG_TY_RD)),    \
          (opc), \
          MODRM(MOD_REG, (rhs).idx % 8, (dest).idx % 8)}, IMM_BYTES32(imm)}})

#define ALU_IMM64_REX(opc, dest, imm)                                           \
  ((struct x64_raw_instr){                                                     \
      .len = 3,                                                                \
      .buff = { \
          REX((size_t)0, (size_t)0, (size_t)0, ((dest).ty == X64_REG_TY_RD)),    \
               MODRM(MOD_REG, (rhs).idx % 8, (dest).idx % 8), IMM_BYTES32(imm)}})

#define ALU_IMM(opc, dest, imm)                                                 \
  (NEEDS_REX(dest)                                                   \
       ? ALU_IMM64_REX((opc), (dest), (imm))                                    \
       : ((dest).ty == X64_REG_TY_RD ? ALU_IMM32_REX((opc), (dest), (imm))      \
                                     : ALU_IMM32((opc), (dest), (imm))))

#define MOV_IMM32(dest, imm)                                               \
  ((struct x64_raw_instr){                                                     \
      .len = 5, .buff = {U8(0xB8 + (dest.idx)), IMM_BYTES32((imm))}})

#define MOV_IMM32_REX(dest, imm)                                           \
  ((struct x64_raw_instr){                                                     \
      .len = 6,                                                                \
      .buff = {                                                                \
          REX((size_t)0, (size_t)0, (size_t)0, (size_t)((dest).ty == X64_REG_TY_RD)),    \
          U8(0xB8 + (dest.idx) % 8), \
          IMM_BYTES32((imm))}})

#define MOV_IMM64_REX(dest, imm)                                           \
  ((struct x64_raw_instr){                                                     \
      .len = 6,                                                                \
      .buff = {REX((size_t)1, (size_t)0, (size_t)0, (size_t)((dest).idx > 7)), U8(0xB8 + (dest.idx) % 8),                \
               IMM_BYTES32((imm))}})

#define MOV_IMM(dest, imm)                                                 \
  (NEEDS_REX(dest)                                                   \
       ? MOV_IMM64_REX((dest), (imm))                                    \
       : ((dest).ty == X64_REG_TY_RD ? MOV_IMM32_REX((dest), (imm))      \
                                     : MOV_IMM32((dest), (imm))))
                  
#define NEEDS_REX(reg) ((reg).ty == X64_REG_TY_R || (reg).ty == X64_REG_TY_RD)

#define MOV_REG_IMM32(reg, imm)                                                \
  ((struct x64_raw_instr){                                                     \
      .len = 5,                                                                \
      .buff = {                                                                \
          (uint8_t)(0xB8 + (reg).idx), (uint8_t)((imm) & 0xFF),                \
          (uint8_t)(((imm) >> 8) & 0xFF), (uint8_t)(((imm) >> 16) & 0xFF),     \
          (uint8_t)(((imm) >> 24) & 0xFF), (uint8_t)(((imm) >> 32) & 0xFF),    \
          (uint8_t)(((imm) >> 40) & 0xFF), (uint8_t)(((imm) >> 48) & 0xFF),    \
          (uint8_t)(((imm) >> 56) & 0xFF)}})


#define MOV_REG_IMM64(reg, imm)                                                \
  ((struct x64_raw_instr){                                                     \
      .len = 10,                                                               \
      .buff = {                                                                \
          0x48, (uint8_t)(0xb8 + (reg)), (uint8_t)((imm) & 0xFF),              \
          (uint8_t)(((imm) >> 8) & 0xFF), (uint8_t)(((imm) >> 16) & 0xFF),     \
          (uint8_t)(((imm) >> 24) & 0xFF), (uint8_t)(((imm) >> 32) & 0xFF),    \
          (uint8_t)(((imm) >> 40) & 0xFF), (uint8_t)(((imm) >> 48) & 0xFF),    \
          (uint8_t)(((imm) >> 56) & 0xFF)}})

#define ADD_REG_IMM32(reg, imm)                                                \
  ((struct x64_raw_instr){.len = 7,                                            \
                          .buff = {0x48, 0x81, (uint8_t)(0xc0 | (reg)),        \
                                   (uint8_t)((imm) & 0xFF),                    \
                                   (uint8_t)(((imm) >> 8) & 0xFF),             \
                                   (uint8_t)(((imm) >> 16) & 0xFF),            \
                                   (uint8_t)(((imm) >> 24) & 0xFF)}})

#define SUB_REG_IMM32(reg, imm)                                                \
  ((struct x64_raw_instr){.len = 7,                                            \
                          .buff = {0x48, 0x81, (uint8_t)(0xe8 | (reg)),        \
                                   (uint8_t)((imm) & 0xFF),                    \
                                   (uint8_t)(((imm) >> 8) & 0xFF),             \
                                   (uint8_t)(((imm) >> 16) & 0xFF),            \
                                   (uint8_t)(((imm) >> 24) & 0xFF)}})

#define JMP_REL32(disp)                                                        \
  ((struct x64_raw_instr){.len = 5,                                            \
                          .buff = {0xe9, (uint8_t)((disp) & 0xFF),             \
                                   (uint8_t)(((disp) >> 8) & 0xFF),            \
                                   (uint8_t)(((disp) >> 16) & 0xFF),           \
                                   (uint8_t)(((disp) >> 24) & 0xFF)}})

#define RET ((struct x64_raw_instr){.len = 1, .buff = {0xc3}})

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
