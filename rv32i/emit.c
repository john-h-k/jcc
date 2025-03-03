#include "emit.h"

#include "../rv32i.h"
#include "../vector.h"
#include "emitter.h"

#include <stdio.h>

#define WORD_SIZE (8)

struct emit_state {
  const struct codegen_function *func;
  struct arena_allocator *arena;
  struct rv32i_emitter *emitter;
};

static void emit_instr(const struct emit_state *state,
                       const struct instr *instr) {

#define EMIT(up, lo)                                                           \
  case RV32I_INSTR_TY_##up:                                                    \
    rv32i_emit_##lo(state->emitter, instr->rv32i->lo);                         \
    break;

#define EMIT_FP(up, lo)                                                           \
  case RV32I_INSTR_TY_##up##_S:                                                    \
    rv32i_emit_##lo##_s(state->emitter, instr->rv32i->lo);                         \
    break; \
  case RV32I_INSTR_TY_##up##_D:                                                    \
    rv32i_emit_##lo##_d(state->emitter, instr->rv32i->lo);                         \
    break;
  switch (instr->rv32i->ty) {
    EMIT(ADDI, addi);
    EMIT(XORI, xori);
    EMIT(ADD, add);
    EMIT(SUB, sub);
    EMIT(MUL, mul);
    EMIT(DIV, div);
    EMIT(REM, rem);
    EMIT(DIVU, divu);
    EMIT(REMU, remu);
    EMIT(LUI, lui);
    EMIT(AUIPC, auipc);
    EMIT(JALR, jalr);
    EMIT(SB, sb);
    EMIT(SH, sh);
    EMIT(SW, sw);
    EMIT(LB, lb);
    EMIT(LBU, lbu);
    EMIT(LH, lh);
    EMIT(LHU, lhu);
    EMIT(LW, lw);
    EMIT(JAL, jal);
    EMIT(BEQ, beq);
    EMIT(BNE, bne);
    EMIT(BLT, blt);
    EMIT(BGE, bge);
    EMIT(BLTU, bltu);
    EMIT(BGEU, bgeu);
    EMIT(OR, or);
    EMIT(AND, and);
    EMIT(XOR, xor);
    EMIT(SLL, sll);
    EMIT(SRL, srl);
    EMIT(SRA, sra);
    EMIT(FSW, fsw);
    EMIT(FLW, flw);
    EMIT(FSD, fsd);
    EMIT(FLD, fld);
    EMIT_FP(FADD, fadd);
    EMIT_FP(FSUB, fsub);
    EMIT_FP(FMUL, fmul);
    EMIT_FP(FDIV, fdiv);
    EMIT_FP(FSGNJ, fsgnj);
    EMIT_FP(FSGNJN, fsgnjn);
    EMIT_FP(FSGNJX, fsgnjx);
    EMIT_FP(FMAX, fmax);
    EMIT_FP(FMIN, fmin);
    EMIT_FP(FSQRT, fsqrt);
    EMIT(ORI, ori);
    EMIT(ANDI, andi);
    EMIT(SLLI, slli);
    EMIT(SRLI, srli);
    EMIT(SRAI, srai);
    EMIT(SLT, slt);
    EMIT(SLTU, sltu);
    EMIT(SLTI, slti);
    EMIT(SLTIU, sltiu);
    EMIT(MULH, mulh);
    EMIT(MULHU, mulhu);
    EMIT(MULHSU, mulhsu);
    EMIT_FP(FCVT, fcvt);
    EMIT_FP(FCVTU, fcvtu);
    EMIT_FP(FEQ, feq);
    EMIT_FP(FLT, flt);
    EMIT_FP(FLE, fle);
    EMIT_FP(FMV, fmv);
    // EMIT(FCLASS, fclass);
    // EMIT(FENCE, fence);
    // EMIT(FENCE_TSO, fence_tso);
    // EMIT(PAUSE, pause);
    case RV32I_INSTR_TY_EBREAK:
      rv32i_emit_ebreak(state->emitter);
      break;
    case RV32I_INSTR_TY_ECALL:
      rv32i_emit_ecall(state->emitter);
      break;
    default:
      TODO("impl fclass/misc instrs");
  }

#undef EMIT
}

struct emitted_unit rv32i_emit(const struct codegen_unit *unit) {
  size_t num_entries = unit->num_entries;
  struct object_entry *entries =
      arena_alloc(unit->arena, num_entries * sizeof(*entries));

  for (size_t i = 0; i < unit->num_entries; i++) {
    struct codegen_entry *entry = &unit->entries[i];

    switch (entry->ty) {
    case CODEGEN_ENTRY_TY_STRING:
      entries[i] = (struct object_entry){
          .ty = OBJECT_ENTRY_TY_C_STRING,
          .alignment = entry->alignment,
          .data = entry->data.data,
          .len_data = entry->data.len_data,
          .num_relocations = 0,
          .relocations = NULL,
          .symbol = entry->symbol};
      break;
    case CODEGEN_ENTRY_TY_CONST_DATA:
      // TODO: relocations
      entries[i] = (struct object_entry){
          .ty = OBJECT_ENTRY_TY_CONST_DATA,
          .alignment = entry->alignment,
          .data = entry->data.data,
          .len_data = entry->data.len_data,
          .num_relocations = entry->data.num_relocs,
          .relocations = entry->data.relocs,
          .symbol = entry->symbol};
      break;
    case CODEGEN_ENTRY_TY_DATA:
      entries[i] = (struct object_entry){
          .ty = OBJECT_ENTRY_TY_MUT_DATA,
          .alignment = entry->alignment,
          .data = entry->data.data,
          .len_data = entry->data.len_data,
          .num_relocations = entry->data.num_relocs,
          .relocations = entry->data.relocs,
          .symbol = entry->symbol};
      break;
    case CODEGEN_ENTRY_TY_DECL:
      entries[i] = (struct object_entry){
          .ty = OBJECT_ENTRY_TY_DECL,
          .alignment = entry->alignment,
          .data = NULL,
          .len_data = 0,
          .num_relocations = entry->data.num_relocs,
          .relocations = entry->data.relocs,
          .symbol = entry->symbol};
      break;
    case CODEGEN_ENTRY_TY_FUNC: {
      struct codegen_function *func = &entry->func;

      struct rv32i_emitter *emitter;
      create_rv32i_emitter(&emitter);

      struct emit_state state = {
          .func = func, .arena = unit->arena, .emitter = emitter};

      struct vector *relocs = vector_create(sizeof(struct relocation));

      struct instr *instr = func->first;
      while (instr) {
        size_t pos = rv32i_emit_bytesize(state.emitter);

        size_t emitted = rv32i_emitted_count(state.emitter);
        emit_instr(&state, instr);

        size_t generated_instrs = rv32i_emitted_count(state.emitter) - emitted;

        DEBUG_ASSERT(
            generated_instrs == 1,
            "expected instr %zu to generate exactly 1 instruction but it "
            "generated %zu",
            instr->id, generated_instrs);

        if (instr->reloc) {
          instr->reloc->address = pos;
          instr->reloc->size = 2;
          vector_push_back(relocs, instr->reloc);
        }

        instr = instr->succ;
      }

      size_t len = rv32i_emit_bytesize(emitter);
      void *data = arena_alloc(unit->arena, len);
      rv32i_emit_copy_to(emitter, data);

      free_rv32i_emitter(&emitter);

      // FIXME: some vector leaks here (and probably other places)
      // should really alloc in arena
      entries[i] = (struct object_entry){
          .ty = OBJECT_ENTRY_TY_FUNC,
          .data = data,
          .len_data = len,
          .symbol = entry->symbol,
          .alignment = RV32I_FUNCTION_ALIGNMENT,
      };

      CLONE_AND_FREE_VECTOR(unit->arena, relocs, entries[i].num_relocations, entries[i].relocations);
      break;
    }
    }
  }

  struct emitted_unit result = {.entries = entries, .num_entries = num_entries};

  return result;
}
