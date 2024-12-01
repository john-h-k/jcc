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
  case RV32I_INSTR_TY_ADD:
    rv32i_emit_add(state->emitter, instr->rv32i->add);
    break;
  case RV32I_INSTR_TY_LUI:
    rv32i_emit_lui(state->emitter, instr->rv32i->lui);
    break;
  case RV32I_INSTR_TY_JALR:
    rv32i_emit_jalr(state->emitter, instr->rv32i->jalr);
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
