#include "emit.h"

#include "../log.h"
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

const char *x64_linux_mangle(UNUSED struct arena_allocator *arena,
                             const char *name) {
  return name;
}

struct emit_state {
  const struct cg_func *func;
  struct arena_allocator *arena;
  struct x64_emitter *emitter;
  struct vector *local_relocs;
};

struct local_reloc_info {
  struct instr *target;
  struct x64_target_reloc reloc;
};

static void emit_instr(const struct emit_state *state,
                       const struct instr *instr) {
#define EMIT(up, lo)                                                           \
  case X64_INSTR_TY_##up:                                                      \
    x64_emit_##lo(state->emitter, instr->x64->lo);                             \
    break;
  switch (instr->x64->ty) {
    EMIT(OR, or);
    EMIT(XOR, xor);
    EMIT(AND, and);
    EMIT(ADD, add);
    EMIT(SUB, sub);

    EMIT(DIV, div);
    EMIT(IDIV, idiv);

    EMIT(IMUL, imul);

    EMIT(LEA, lea);
    EMIT(LEA_PCREL, lea_pcrel);

    EMIT(ADD_IMM, add_imm);
    EMIT(SUB_IMM, sub_imm);
    EMIT(OR_IMM, or_imm);
    EMIT(AND_IMM, and_imm);
    EMIT(XOR_IMM, xor_imm);

    EMIT(SHR, shr);
    EMIT(SHL, shl);
    EMIT(SAR, sar);

    EMIT(NOT, not);
    EMIT(NEG, neg);

    EMIT(TEST, test);
    EMIT(CMP, cmp);

    EMIT(SETCC, setcc);

    EMIT(CALL, call);
    EMIT(JMP_REG, jmp_reg);
    EMIT(CALL_REG, call_reg);

    EMIT(TEST_IMM, test_imm);
    EMIT(CMP_IMM, cmp_imm);

    EMIT(MOVSX, movsx);

    EMIT(PUSH, push);
    EMIT(POP, pop);

    EMIT(MOV_LOAD_IMM, mov_load_imm);

    EMIT(MOVZX_LOAD_HALF_IMM, movzx_load_half_imm);
    EMIT(MOVZX_LOAD_BYTE_IMM, movzx_load_byte_imm);

    EMIT(MOV_STORE_IMM, mov_store_imm);
    EMIT(MOV_STORE_HALF_IMM, mov_store_half_imm);
    EMIT(MOV_STORE_BYTE_IMM, mov_store_byte_imm);

    EMIT(MOV_IMM, mov_imm);
    EMIT(MOV_REG, mov_reg);
    EMIT(MOVQ, movq);
    EMIT(MOVD, movd);

    EMIT(MOV_STORE_SS_IMM, mov_store_ss_imm);
    EMIT(MOV_STORE_SD_IMM, mov_store_sd_imm);
    EMIT(MOV_LOAD_SS_IMM, mov_load_ss_imm);
    EMIT(MOV_LOAD_SD_IMM, mov_load_sd_imm);

    EMIT(MOVAPS, movaps);
    EMIT(MOVAPD, movapd);

    EMIT(UCOMISS, ucomiss);
    EMIT(UCOMISD, ucomisd);

    EMIT(SQRTSS, sqrtss);
    EMIT(SQRTSD, sqrtsd);
    EMIT(ANDPS, andps);
    EMIT(ANDPD, andpd);
    EMIT(XORPS, xorps);
    EMIT(XORPD, xorpd);
    EMIT(ORPS, orps);
    EMIT(ORPD, orpd);

    EMIT(ADDSS, addss);
    EMIT(ADDSD, addsd);
    EMIT(SUBSS, subss);
    EMIT(SUBSD, subsd);
    EMIT(MULSS, mulss);
    EMIT(MULSD, mulsd);
    EMIT(DIVSS, divss);
    EMIT(DIVSD, divsd);

    EMIT(CVTSI2SS, cvtsi2ss);
    EMIT(CVTSI2SD, cvtsi2sd);

    EMIT(CVTTSS2SI, cvttss2si);
    EMIT(CVTTSD2SI, cvttsd2si);

    EMIT(CVTSS2SI, cvtss2si);
    EMIT(CVTSD2SI, cvtsd2si);

    EMIT(CVTSS2SD, cvtss2sd);
    EMIT(CVTSD2SS, cvtsd2ss);
  case X64_INSTR_TY_JMP: {
    struct x64_target_reloc reloc =
        x64_emit_jmp(state->emitter, instr->x64->jmp);
    struct local_reloc_info info = {
        .target = cg_get_next_instr(instr->x64->jmp.target->cg_basicblock),
        .reloc = reloc};
    vector_push_back(state->local_relocs, &info);
    break;
  }
  case X64_INSTR_TY_JCC: {
    struct x64_target_reloc reloc =
        x64_emit_jcc(state->emitter, instr->x64->jcc);
    struct local_reloc_info info = {
        .target = cg_get_next_instr(instr->x64->jcc.target->cg_basicblock),
        .reloc = reloc};
    vector_push_back(state->local_relocs, &info);
    break;
  }
  case X64_INSTR_TY_RET:
    x64_emit_ret(state->emitter);
    break;
  }

#undef EMIT
}

struct emitted_unit x64_emit(const struct cg_unit *unit) {
  size_t num_entries = unit->num_entries;
  struct object_entry *entries =
      arena_alloc(unit->arena, num_entries * sizeof(*entries));

  for (size_t i = 0; i < unit->num_entries; i++) {
    struct cg_entry *entry = &unit->entries[i];

    switch (entry->ty) {
    case CG_ENTRY_TY_STRING:
      entries[i] = (struct object_entry){.ty = OBJECT_ENTRY_TY_C_STRING,
                                         .alignment = entry->alignment,
                                         .data = entry->data.data,
                                         .len_data = entry->data.len_data,
                                         .num_relocations = 0,
                                         .relocations = NULL,
                                         .symbol = entry->symbol};
      break;
    case CG_ENTRY_TY_CONST_DATA:
      // TODO: relocations
      entries[i] =
          (struct object_entry){.ty = OBJECT_ENTRY_TY_CONST_DATA,
                                .alignment = entry->alignment,
                                .data = entry->data.data,
                                .len_data = entry->data.len_data,
                                // TODO: reloc lifetimes
                                .num_relocations = entry->data.num_relocs,
                                .relocations = entry->data.relocs,
                                .symbol = entry->symbol};
      break;
    case CG_ENTRY_TY_DATA:
      entries[i] =
          (struct object_entry){.ty = OBJECT_ENTRY_TY_MUT_DATA,
                                .alignment = entry->alignment,
                                .data = entry->data.data,
                                .len_data = entry->data.len_data,
                                // TODO: reloc lifetimes
                                .num_relocations = entry->data.num_relocs,
                                .relocations = entry->data.relocs,
                                .symbol = entry->symbol};
      break;
    case CG_ENTRY_TY_DECL:
      entries[i] =
          (struct object_entry){.ty = OBJECT_ENTRY_TY_DECL,
                                .alignment = entry->alignment,
                                .data = NULL,
                                .len_data = 0,
                                // TODO: reloc lifetimes
                                .num_relocations = entry->data.num_relocs,
                                .relocations = entry->data.relocs,
                                .symbol = entry->symbol};
      break;
    case CG_ENTRY_TY_FUNC: {
      struct cg_func *func = &entry->func;

      struct x64_emitter *emitter;
      create_x64_emitter(&emitter);

      struct vector *local_relocs =
          vector_create(sizeof(struct local_reloc_info));

      struct emit_state state = {.func = func,
                                 .arena = unit->arena,
                                 .emitter = emitter,
                                 .local_relocs = local_relocs};

      struct vector *relocs = vector_create(sizeof(struct relocation));
      struct vector *instr_offsets = vector_create(sizeof(size_t));

      struct cg_basicblock *basicblock = func->first;
      while (basicblock) {
        struct instr *instr = basicblock->first;
        while (instr) {
          DEBUG_ASSERT(x64_emitted_count(emitter) == instr->id,
                       "expected emitted count to be same as instr id");

          size_t pos = x64_emit_bytesize(state.emitter);

          vector_push_back(instr_offsets, &pos);

          size_t emitted = x64_emitted_count(state.emitter);
          emit_instr(&state, instr);

          size_t generated_instrs = x64_emitted_count(state.emitter) - emitted;

          size_t new_pos = x64_emit_bytesize(state.emitter);
          size_t len = new_pos - pos;
          invariant_assert(len <= 16, "instr too big");
          char buff[16];
          x64_get_bytes(state.emitter, pos, len, buff);

          if (log_enabled()) {
            fprintf(stderr, "Emitted instruction: ");
            x64_debug_print_instr(stderr, func, instr);
            fprintf(stderr, "\nValue: ");

            fprintf(stderr, "{ ");
            for (size_t j = 0; j < len; j++) {
              fprintf(stderr, "%02hhX", buff[j]);
              if (j + 1 != len) {
                fprintf(stderr, ", ");
              }
            }
            fprintf(stderr, " }\n");
          }

          DEBUG_ASSERT(
              generated_instrs == 1,
              "expected instr %zu to generate exactly 1 instruction but it "
              "generated %zu",
              instr->id, generated_instrs);

          if (instr->reloc) {
            instr->reloc->address += pos;
            instr->reloc->size = 2;
            vector_push_back(relocs, instr->reloc);
          }

          instr = instr->succ;
        }

        basicblock = basicblock->succ;
      }

      size_t num_local_relocs = vector_length(state.local_relocs);
      for (size_t j = 0; j < num_local_relocs; j++) {
        struct local_reloc_info *info = vector_get(state.local_relocs, j);

        size_t instr_pos =
            *(size_t *)vector_get(instr_offsets, info->target->id);

        x64_reloc(state.emitter, info->reloc, instr_pos);
      }

      size_t len = x64_emit_bytesize(emitter);

      void *data = arena_alloc(unit->arena, len);
      x64_emit_copy_to(emitter, data);

      free_x64_emitter(&emitter);

      vector_free(&instr_offsets);
      vector_free(&local_relocs);

      // FIXME: some vector leaks here (and probably other places)
      // should really alloc in arena
      entries[i] = (struct object_entry){
          .ty = OBJECT_ENTRY_TY_FUNC,
          .alignment = entry->alignment,
          .data = data,
          .len_data = len,
          .symbol = entry->symbol,
      };

      CLONE_AND_FREE_VECTOR(unit->arena, relocs, entries[i].num_relocations,
                            entries[i].relocations);
      break;
    }
    }
  }

  struct emitted_unit result = {.entries = entries, .num_entries = num_entries};

  return result;
}
