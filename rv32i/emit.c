#include "emit.h"

#include "../ir/prettyprint.h"
#include "../rv32i.h"
#include "../vector.h"
#include "emitter.h"
#include "isa.h"

#include <mach/message.h>
#include <stdio.h>

#define WORD_SIZE (8)

struct emit_state {
  const struct codegen_function *func;
  struct arena_allocator *arena;
  struct rv32i_emitter *emitter;
};

static void emit_instr(const struct emit_state *state,
                       const struct instr *instr) {
  switch (instr->rv32i->ty) {
  case RV32I_INSTR_TY_ADDI:
    rv32i_emit_addi(state->emitter, instr->rv32i->addi);
    break;
  case RV32I_INSTR_TY_XORI:
    rv32i_emit_xori(state->emitter, instr->rv32i->xori);
    break;
  case RV32I_INSTR_TY_ADD:
    rv32i_emit_add(state->emitter, instr->rv32i->add);
    break;
  case RV32I_INSTR_TY_SUB:
    rv32i_emit_sub(state->emitter, instr->rv32i->sub);
    break;
  case RV32I_INSTR_TY_MUL:
    rv32i_emit_mul(state->emitter, instr->rv32i->mul);
    break;
  case RV32I_INSTR_TY_DIV:
    rv32i_emit_div(state->emitter, instr->rv32i->div);
    break;
  case RV32I_INSTR_TY_REM:
    rv32i_emit_rem(state->emitter, instr->rv32i->rem);
    break;
  case RV32I_INSTR_TY_DIVU:
    rv32i_emit_divu(state->emitter, instr->rv32i->divu);
    break;
  case RV32I_INSTR_TY_REMU:
    rv32i_emit_remu(state->emitter, instr->rv32i->remu);
    break;
  case RV32I_INSTR_TY_LUI:
    rv32i_emit_lui(state->emitter, instr->rv32i->lui);
    break;
  case RV32I_INSTR_TY_JALR:
    rv32i_emit_jalr(state->emitter, instr->rv32i->jalr);
    break;
  case RV32I_INSTR_TY_SB:
    rv32i_emit_sb(state->emitter, instr->rv32i->sb);
    break;
  case RV32I_INSTR_TY_SH:
    rv32i_emit_sh(state->emitter, instr->rv32i->sh);
    break;
  case RV32I_INSTR_TY_SW:
    rv32i_emit_sw(state->emitter, instr->rv32i->sw);
    break;
  case RV32I_INSTR_TY_LB:
    rv32i_emit_lb(state->emitter, instr->rv32i->lb);
    break;
  case RV32I_INSTR_TY_LBU:
    rv32i_emit_lbu(state->emitter, instr->rv32i->lbu);
    break;
  case RV32I_INSTR_TY_LH:
    rv32i_emit_lh(state->emitter, instr->rv32i->lh);
    break;
  case RV32I_INSTR_TY_LHU:
    rv32i_emit_lhu(state->emitter, instr->rv32i->lhu);
    break;
  case RV32I_INSTR_TY_LW:
    rv32i_emit_lw(state->emitter, instr->rv32i->lw);
    break;
  case RV32I_INSTR_TY_JAL:
    rv32i_emit_jal(state->emitter, instr->rv32i->jal);
    break;
  case RV32I_INSTR_TY_BEQ:
    rv32i_emit_beq(state->emitter, instr->rv32i->beq);
    break;
  case RV32I_INSTR_TY_BNE:
    rv32i_emit_bne(state->emitter, instr->rv32i->bne);
    break;
  case RV32I_INSTR_TY_BLT:
    rv32i_emit_blt(state->emitter, instr->rv32i->blt);
    break;
  case RV32I_INSTR_TY_BGE:
    rv32i_emit_bge(state->emitter, instr->rv32i->bge);
    break;
  case RV32I_INSTR_TY_BLTU:
    rv32i_emit_bltu(state->emitter, instr->rv32i->bltu);
    break;
  case RV32I_INSTR_TY_BGEU:
    rv32i_emit_bgeu(state->emitter, instr->rv32i->bgeu);
    break;
  case RV32I_INSTR_TY_OR:
    rv32i_emit_or(state->emitter, instr->rv32i->or);
    break;
  case RV32I_INSTR_TY_AND:
    rv32i_emit_and(state->emitter, instr->rv32i->and);
    break;
  case RV32I_INSTR_TY_XOR:
    rv32i_emit_xor(state->emitter, instr->rv32i->xor);
    break;
  case RV32I_INSTR_TY_SLL:
    rv32i_emit_sll(state->emitter, instr->rv32i->sll);
    break;
  case RV32I_INSTR_TY_SRL:
    rv32i_emit_srl(state->emitter, instr->rv32i->srl);
    break;
  case RV32I_INSTR_TY_SRA:
    rv32i_emit_sra(state->emitter, instr->rv32i->sra);
    break;
  }
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
          .alignment = 0,
          .data = entry->str,
          .len_data = strlen(entry->str) + 1,
          .num_relocations = 0,
          .relocations = NULL,
          .symbol =
              (struct symbol){.ty = SYMBOL_TY_STRING,
                              .visibility = SYMBOL_VISIBILITY_GLOBAL, // FIXME:
                              .name = entry->name}};
      break;
    case CODEGEN_ENTRY_TY_CONST_DATA:
      // TODO: relocations
      entries[i] = (struct object_entry){
          .ty = OBJECT_ENTRY_TY_CONST_DATA,
          .alignment = 0,
          .data = entry->data.data,
          .len_data = entry->data.len_data,
          .num_relocations = 0,
          .relocations = NULL,
          .symbol =
              (struct symbol){.ty = SYMBOL_TY_CONST_DATA,
                              .visibility = SYMBOL_VISIBILITY_GLOBAL, // FIXME:
                              .name = entry->name}};
      break;
    case CODEGEN_ENTRY_TY_DATA:
      entries[i] = (struct object_entry){
          .ty = OBJECT_ENTRY_TY_MUT_DATA,
          .alignment = 0,
          .data = entry->data.data,
          .len_data = entry->data.len_data,
          .num_relocations = 0,
          .relocations = NULL,
          .symbol =
              (struct symbol){.ty = SYMBOL_TY_DATA,
                              .visibility = SYMBOL_VISIBILITY_GLOBAL, // FIXME:
                              .name = entry->name}};
      break;
    case CODEGEN_ENTRY_TY_DECL:
      entries[i] = (struct object_entry){
          .ty = OBJECT_ENTRY_TY_DECL,
          .alignment = 0,
          .data = NULL,
          .len_data = 0,
          .num_relocations = 0,
          .relocations = NULL,
          .symbol = (struct symbol){.ty = SYMBOL_TY_DECL,
                                    .visibility = SYMBOL_VISIBILITY_UNDEF,
                                    .name = entry->name}};
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

        debug_assert(
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

      struct symbol symbol = {
          .ty = SYMBOL_TY_FUNC,
          .name = entry->name,
          .visibility = SYMBOL_VISIBILITY_GLOBAL // FIXME: symbol vis
      };

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
          .symbol = symbol,
          .relocations = vector_head(relocs),
          .num_relocations = vector_length(relocs),
          .alignment = RV32I_FUNCTION_ALIGNMENT,
      };
      break;
    }
    }
  }

  struct emitted_unit result = {.entries = entries, .num_entries = num_entries};

  return result;
}
