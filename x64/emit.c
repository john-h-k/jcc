#include "emit.h"

#include "../x64.h"
#include "../vector.h"
#include "emitter.h"

#include <stdio.h>

#define WORD_SIZE (8)

const char *x64_macos_mangle(struct arena_allocator *arena, const char *name) {
  char *dest =
      arena_alloc(arena, strlen(name) + /* null terminator + '_' char */ 2);

  dest[0] = '_';
  strcpy(dest + 1, name);

  return dest;
}

const char *x64_linux_mangle(struct arena_allocator *arena, const char *name) {
  return name;
}

struct emit_state {
  const struct codegen_function *func;
  struct arena_allocator *arena;
  struct x64_emitter *emitter;
};

static void emit_instr(const struct emit_state *state,
                       const struct instr *instr) {
  #define EMIT(up, lo) \
  case X64_INSTR_TY_ ## up: \
    x64_emit_ ## lo (state->emitter, instr->x64->lo); \
    break;
  switch (instr->x64->ty) {
    EMIT(OR, or);
    EMIT(EOR, eor);
    EMIT(AND, and);
    EMIT(ADD, add);
    EMIT(SUB, sub);

    EMIT(ADD_IMM, add_imm);
    EMIT(SUB_IMM, sub_imm);
    EMIT(OR_IMM, or_imm);
    EMIT(AND_IMM, and_imm);
    EMIT(EOR_IMM, eor_imm);

    EMIT(SHR, shr);
    EMIT(SHL, shl);
    EMIT(SAR, sar);

    EMIT(NOT, not);
    EMIT(NEG, neg);

    EMIT(MOVSX, movsx);

    EMIT(PUSH, push);
    EMIT(POP, pop);

    EMIT(MOV_LOAD_IMM, mov_load_imm);
    EMIT(MOV_STORE_IMM, mov_store_imm);

    EMIT(MOV_IMM, mov_imm);
    EMIT(MOV_REG, mov_reg);
  case X64_INSTR_TY_RET:
    x64_emit_ret(state->emitter);
    break;
  }
  #undef EMIT
}

struct emitted_unit x64_emit(const struct codegen_unit *unit) {
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
          .alignment = entry->alignment,
          .data = entry->data.data,
          .len_data = entry->data.len_data,
          // TODO: reloc lifetimes
          .num_relocations = entry->data.num_relocs,
          .relocations = entry->data.relocs,
          .symbol =
              (struct symbol){.ty = SYMBOL_TY_CONST_DATA,
                              .visibility = SYMBOL_VISIBILITY_GLOBAL, // FIXME:
                              .name = entry->name}};
      break;
    case CODEGEN_ENTRY_TY_DATA:
      entries[i] = (struct object_entry){
          .ty = OBJECT_ENTRY_TY_MUT_DATA,
          .alignment = entry->alignment,
          .data = entry->data.data,
          .len_data = entry->data.len_data,
          // TODO: reloc lifetimes
          .num_relocations = entry->data.num_relocs,
          .relocations = entry->data.relocs,
          .symbol =
              (struct symbol){.ty = SYMBOL_TY_DATA,
                              .visibility = SYMBOL_VISIBILITY_GLOBAL, // FIXME:
                              .name = entry->name}};
      break;
    case CODEGEN_ENTRY_TY_DECL:
      entries[i] = (struct object_entry){
          .ty = OBJECT_ENTRY_TY_DECL,
          .alignment = entry->alignment,
          .data = NULL,
          .len_data = 0,
          // TODO: reloc lifetimes
          .num_relocations = entry->data.num_relocs,
          .relocations = entry->data.relocs,
          .symbol = (struct symbol){.ty = SYMBOL_TY_DECL,
                                    .visibility = SYMBOL_VISIBILITY_UNDEF,
                                    .name = entry->name}};
      break;
    case CODEGEN_ENTRY_TY_FUNC: {
      struct codegen_function *func = &entry->func;

      struct x64_emitter *emitter;
      create_x64_emitter(&emitter);

      struct emit_state state = {
          .func = func, .arena = unit->arena, .emitter = emitter};

      struct vector *relocs = vector_create(sizeof(struct relocation));

      struct instr *instr = func->first;
      while (instr) {
        size_t pos = x64_emit_bytesize(state.emitter);

        size_t emitted = x64_emitted_count(state.emitter);
        emit_instr(&state, instr);

        size_t generated_instrs = x64_emitted_count(state.emitter) - emitted;

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

      size_t len = x64_emit_bytesize(emitter);

      struct symbol symbol = {
          .ty = SYMBOL_TY_FUNC,
          .name = entry->name,
          .visibility = SYMBOL_VISIBILITY_GLOBAL // FIXME: symbol vis
      };

      void *data = arena_alloc(unit->arena, len);
      x64_emit_copy_to(emitter, data);

      free_x64_emitter(&emitter);

      // FIXME: some vector leaks here (and probably other places)
      // should really alloc in arena
      entries[i] = (struct object_entry){
          .ty = OBJECT_ENTRY_TY_FUNC,
          .alignment = entry->alignment,
          .data = data,
          .len_data = len,
          .symbol = symbol,
          .relocations = vector_head(relocs),
          .num_relocations = vector_length(relocs),
      };
      break;
    }
    }
  }

  struct emitted_unit result = {.entries = entries, .num_entries = num_entries};

  return result;
}
