#ifndef X64_ISA_H
#define X64_ISA_H

#include <stddef.h>
#include <stdint.h>

#define FITS_IN_BITS(value, bitc)                                              \
  _Generic((value),                                                            \
      size_t: (((value) & ~((1ull << (bitc + 1)) - 1ull)) == 0),                \
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

#define MOD_REG (size_t)0b11

#define NOP ((struct x64_raw_instr){.len = 1, .buff = {0x90}})

#define IMM(imm, bitc)                                                         \
  (DEBUG_ASSERT(FITS_IN_BITS(imm, bitc), "immediate did not fit!"),            \
   CLAMP_BITS(imm, bitc))

#define MODRM(mod, reg, rm) U8((IMM(mod, 2) << 6) | (IMM(reg, 3) << 3) | IMM(rm, 3))

#define ADD_REG(dest, rhs)                                            \
  ((struct x64_raw_instr){                                                     \
      .len = 2,                                                                \
      .buff = {                                                                \
          0x01, \
          MODRM(MOD_REG, rhs.idx, dest.idx)}})

#define MOV_REG_IMM32(reg, imm)                                            \
  ((struct x64_raw_instr){                                                     \
      .len = 5,                                                                \
      .buff = {                                                                \
          (uint8_t)(0xB8 + (reg).idx), (uint8_t)((imm) & 0xff),                \
          (uint8_t)(((imm) >> 8) & 0xff), (uint8_t)(((imm) >> 16) & 0xff),     \
          (uint8_t)(((imm) >> 24) & 0xff), (uint8_t)(((imm) >> 32) & 0xff),    \
          (uint8_t)(((imm) >> 40) & 0xff), (uint8_t)(((imm) >> 48) & 0xff),    \
          (uint8_t)(((imm) >> 56) & 0xff)}})

#define MOV_REG_IMM64(reg, imm)                                            \
  ((struct x64_raw_instr){                                                     \
      .len = 10,                                                               \
      .buff = {                                                                \
          0x48, (uint8_t)(0xb8 + (reg)), (uint8_t)((imm) & 0xff),              \
          (uint8_t)(((imm) >> 8) & 0xff), (uint8_t)(((imm) >> 16) & 0xff),     \
          (uint8_t)(((imm) >> 24) & 0xff), (uint8_t)(((imm) >> 32) & 0xff),    \
          (uint8_t)(((imm) >> 40) & 0xff), (uint8_t)(((imm) >> 48) & 0xff),    \
          (uint8_t)(((imm) >> 56) & 0xff)}})

#define MOV_REG_REG32(dest, src)                                              \
  ((struct x64_raw_instr){                                                     \
      .len = 2, .buff = {0x89, MODRM(MOD_REG, src.idx, dest.idx)}})

#define ADD_REG_IMM32(reg, imm)                                            \
  ((struct x64_raw_instr){.len = 7,                                            \
                          .buff = {0x48, 0x81, (uint8_t)(0xc0 | (reg)),        \
                                   (uint8_t)((imm) & 0xff),                    \
                                   (uint8_t)(((imm) >> 8) & 0xff),             \
                                   (uint8_t)(((imm) >> 16) & 0xff),            \
                                   (uint8_t)(((imm) >> 24) & 0xff)}})

#define SUB_REG_IMM32(reg, imm)                                            \
  ((struct x64_raw_instr){.len = 7,                                            \
                          .buff = {0x48, 0x81, (uint8_t)(0xe8 | (reg)),        \
                                   (uint8_t)((imm) & 0xff),                    \
                                   (uint8_t)(((imm) >> 8) & 0xff),             \
                                   (uint8_t)(((imm) >> 16) & 0xff),            \
                                   (uint8_t)(((imm) >> 24) & 0xff)}})

#define JMP_REL32(disp)                                                    \
  ((struct x64_raw_instr){.len = 5,                                            \
                          .buff = {0xe9, (uint8_t)((disp) & 0xff),             \
                                   (uint8_t)(((disp) >> 8) & 0xff),            \
                                   (uint8_t)(((disp) >> 16) & 0xff),           \
                                   (uint8_t)(((disp) >> 24) & 0xff)}})

#define RET ((struct x64_raw_instr){.len = 1, .buff = {0xc3}})

#define CALL_REL32(disp)                                                   \
  ((struct x64_raw_instr){.len = 5,                                            \
                          .buff = {0xe8, (uint8_t)((disp) & 0xff),             \
                                   (uint8_t)(((disp) >> 8) & 0xff),            \
                                   (uint8_t)(((disp) >> 16) & 0xff),           \
                                   (uint8_t)(((disp) >> 24) & 0xff)}})

#define CMP_REG_IMM32(reg, imm)                                            \
  ((struct x64_raw_instr){.len = 7,                                            \
                          .buff = {0x48, 0x81, (uint8_t)(0xf8 | (reg)),        \
                                   (uint8_t)((imm) & 0xff),                    \
                                   (uint8_t)(((imm) >> 8) & 0xff),             \
                                   (uint8_t)(((imm) >> 16) & 0xff),            \
                                   (uint8_t)(((imm) >> 24) & 0xff)}})

#define CMP_REG_REG(dst, src)                                              \
  ((struct x64_raw_instr){                                                     \
      .len = 3, .buff = {0x48, 0x39, (uint8_t)(0xc0 | ((src) << 3) | (dst))}})

#define JMP_REG(reg)                                                       \
  ((struct x64_raw_instr){.len = 3,                                            \
                          .buff = {0x48, 0xff, (uint8_t)(0xe0 | (reg))}})

#define JCC_REL8(cc, disp)                                                 \
  ((struct x64_raw_instr){                                                     \
      .len = 2, .buff = {(uint8_t)(0x70 + (cc)), (uint8_t)((disp) & 0xff)}})

#define JCC_REL32(cc, disp)                                                \
  ((struct x64_raw_instr){.len = 6,                                            \
                          .buff = {0x0f, (uint8_t)(0x80 + (cc)),               \
                                   (uint8_t)((disp) & 0xff),                   \
                                   (uint8_t)(((disp) >> 8) & 0xff),            \
                                   (uint8_t)(((disp) >> 16) & 0xff),           \
                                   (uint8_t)(((disp) >> 24) & 0xff)}})

#define LEA_RIP_REL32(reg, disp)                                           \
  ((struct x64_raw_instr){.len = 7,                                            \
                          .buff = {0x48, 0x8d, (uint8_t)(((reg) << 3) | 5),    \
                                   (uint8_t)((disp) & 0xff),                   \
                                   (uint8_t)(((disp) >> 8) & 0xff),            \
                                   (uint8_t)(((disp) >> 16) & 0xff),           \
                                   (uint8_t)(((disp) >> 24) & 0xff)}})

#define XOR_REG_REG(dst, src)                                              \
  ((struct x64_raw_instr){                                                     \
      .len = 3, .buff = {0x48, 0x31, (uint8_t)(0xc0 | ((src) << 3) | (dst))}})

#define AND_REG_REG(dst, src)                                              \
  ((struct x64_raw_instr){                                                     \
      .len = 3, .buff = {0x48, 0x21, (uint8_t)(0xc0 | ((src) << 3) | (dst))}})

#define OR_REG_REG(dst, src)                                               \
  ((struct x64_raw_instr){                                                     \
      .len = 3, .buff = {0x48, 0x09, (uint8_t)(0xc0 | ((src) << 3) | (dst))}})

#define IMUL_REG_REG(dst, src)                                             \
  ((struct x64_raw_instr){                                                     \
      .len = 4,                                                                \
      .buff = {0x48, 0x0f, 0xaf, (uint8_t)(0xc0 | ((dst) << 3) | (src))}})

#endif
