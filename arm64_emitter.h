#ifndef __ARM64_EMITTER_H__
#define __ARM64_EMITTER_H__

#include <stdlib.h>

struct arm64_emitter;

// enum arm64_instr_type {
//     ARM64_INSTR_TYPE_RESERVED = 0b0000,
//     ARM64_INSTR_TYPE_UNALLOCATED0 = 0b0001,
//     ARM64_INSTR_TYPE_SVE = 0b0010,
    // ARM64_INSTR_TYPE_UNALLOCATED1 = "0011"
    // ARM64_INSTR_TYPE_IMM_DATA_PROCESSING = "100x"
    // ARM64_INSTR_TYPE_BRANCH_EX_SYS = "101x"
    // ARM64_INSTR_TYPE_LOAD_STORE = "x1x0"
    // ARM64_INSTR_TYPE_REG_DATA_PROCESSING = "x101"
    // ARM64_INSTR_TYPE_FP_SIMD_DATA_PROCESSING = "x111"
    // ARM64_INSTR_TYPE_UNKNOWN = None
// };

// class ImmDataProcessingType(Enum):
//     RelAddressing = "00x"
//     ImmArith = "010"
//     ImmArithTag = "011"
//     ImmLogic = "100"
//     ImmMovWide = "101"
//     Bitfield = "110"
//     Extract = "111"


void create_arm64_emitter(struct arm64_emitter **emitter);
void free_arm64_emitter(struct arm64_emitter **emitter);

size_t arm64_emit_bytesize(struct arm64_emitter *emitter);
void arm64_emit_copy_to(struct arm64_emitter *emitter, void* dest);

void arm64_emit_load_cnst_32(struct arm64_emitter *emitter, size_t reg_idx, uint32_t cnst);
void arm64_emit_load_cnst_64(struct arm64_emitter *emitter, size_t reg_idx, uint64_t cnst);
void arm64_emit_mov_32(struct arm64_emitter *emitter, size_t reg_from, size_t reg_to);
void arm64_emit_mov_64(struct arm64_emitter *emitter, size_t reg_from, size_t reg_to);
void arm64_emit_ret(struct arm64_emitter *emitter);

#endif
