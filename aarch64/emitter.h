#ifndef AARCH64_EMITTER_H
#define AARCH64_EMITTER_H

#include <stdlib.h>

struct aarch64_emitter;

// enum aarch64_instr_type {
//     aarch64_INSTR_TYPE_RESERVED = 0b0000,
//     aarch64_INSTR_TYPE_UNALLOCATED0 = 0b0001,
//     aarch64_INSTR_TYPE_SVE = 0b0010,
    // aarch64_INSTR_TYPE_UNALLOCATED1 = "0011"
    // aarch64_INSTR_TYPE_IMM_DATA_PROCESSING = "100x"
    // aarch64_INSTR_TYPE_BRANCH_EX_SYS = "101x"
    // aarch64_INSTR_TYPE_LOAD_STORE = "x1x0"
    // aarch64_INSTR_TYPE_REG_DATA_PROCESSING = "x101"
    // aarch64_INSTR_TYPE_FP_SIMD_DATA_PROCESSING = "x111"
    // aarch64_INSTR_TYPE_UNKNOWN = None
// };

// class ImmDataProcessingType(Enum):
//     RelAddressing = "00x"
//     ImmArith = "010"
//     ImmArithTag = "011"
//     ImmLogic = "100"
//     ImmMovWide = "101"
//     Bitfield = "110"
//     Extract = "111"


void create_aarch64_emitter(struct aarch64_emitter **emitter);
void free_aarch64_emitter(struct aarch64_emitter **emitter);

size_t aarch64_emit_bytesize(struct aarch64_emitter *emitter);
void aarch64_emit_copy_to(struct aarch64_emitter *emitter, void* dest);

void aarch64_emit_sub_32(struct aarch64_emitter *emitter, size_t reg_lhs, size_t reg_rhs, size_t reg_to);
void aarch64_emit_add_32(struct aarch64_emitter *emitter, size_t reg_lhs, size_t reg_rhs, size_t reg_to);
void aarch64_emit_mul_32(struct aarch64_emitter *emitter, size_t reg_lhs, size_t reg_rhs, size_t reg_to);
void aarch64_emit_sdiv_32(struct aarch64_emitter *emitter, size_t reg_lhs, size_t reg_rhs, size_t reg_to);
void aarch64_emit_udiv_32(struct aarch64_emitter *emitter, size_t reg_lhs, size_t reg_rhs, size_t reg_to);

void aarch64_emit_load_cnst_32(struct aarch64_emitter *emitter, size_t reg_idx, uint32_t cnst);
void aarch64_emit_load_cnst_64(struct aarch64_emitter *emitter, size_t reg_idx, uint64_t cnst);
void aarch64_emit_mov_32(struct aarch64_emitter *emitter, size_t reg_from, size_t reg_to);
void aarch64_emit_mov_64(struct aarch64_emitter *emitter, size_t reg_from, size_t reg_to);
void aarch64_emit_ret(struct aarch64_emitter *emitter);

#endif
