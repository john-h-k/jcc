#if 0 // NOLINT(readability-avoid-unconditional-preprocessor-if)

#include "emit.h"

#include "../aarch64.h"
#include "../bit_twiddle.h"
#include "../vector.h"
#include "emitter.h"
#include "isa.h"

struct emit_state {
  const struct codegen_function *func;
  struct arena_allocator *arena;
  struct eep_emitter *emitter;
};

static enum eep_cond get_cond_for_op(struct ir_op *op) {
  invariant_assert(op->ty == IR_OP_TY_BINARY_OP,
                   "`get_cond_for_op` expects a binary op");

  switch (op->binary_op.ty) {
  case IR_OP_BINARY_OP_TY_EQ:
    return EEP_COND_JEQ;
  case IR_OP_BINARY_OP_TY_NEQ:
    return EEP_COND_JNE;
  case IR_OP_BINARY_OP_TY_UGT:
    return EEP_COND_JHI;
  case IR_OP_BINARY_OP_TY_SGT:
    return EEP_COND_JGT;
  case IR_OP_BINARY_OP_TY_UGTEQ:
    return EEP_COND_JCS;
  case IR_OP_BINARY_OP_TY_SGTEQ:
    return EEP_COND_JGE;
  case IR_OP_BINARY_OP_TY_ULT:
    return EEP_COND_JCC;
  case IR_OP_BINARY_OP_TY_SLT:
    return EEP_COND_JLT;
  case IR_OP_BINARY_OP_TY_ULTEQ:
    return EEP_COND_JLS;
  case IR_OP_BINARY_OP_TY_SLTEQ:
    return EEP_COND_JLE;
  default:
    bug("op was not a comparison");
  }
}

static void emit_instr(const struct emit_state *state,
                       const struct instr *instr) {

  switch (instr->eep->ty) {
  case EEP_INSTR_TY_MOV:
    eep_emit_mov(state->emitter, instr->eep->mov);
    break;
  case EEP_INSTR_TY_MOV_IMM:
    eep_emit_mov_imm(state->emitter, instr->eep->mov_imm);
    break;
  case EEP_INSTR_TY_ADD:
    eep_emit_add(state->emitter, instr->eep->add);
    break;
  case EEP_INSTR_TY_SUB:
    eep_emit_sub(state->emitter, instr->eep->sub);
    break;
  case EEP_INSTR_TY_ADC:
    eep_emit_adc(state->emitter, instr->eep->adc);
    break;
  case EEP_INSTR_TY_SBC:
    eep_emit_sbc(state->emitter, instr->eep->sbc);
    break;
  case EEP_INSTR_TY_AND:
    eep_emit_and(state->emitter, instr->eep->and);
    break;
  case EEP_INSTR_TY_CMP:
    eep_emit_cmp(state->emitter, instr->eep->cmp);
    break;
  case EEP_INSTR_TY_ADD_IMM:
    eep_emit_add_imm(state->emitter, instr->eep->add_imm);
    break;
  case EEP_INSTR_TY_SUB_IMM:
    eep_emit_sub_imm(state->emitter, instr->eep->sub_imm);
    break;
  case EEP_INSTR_TY_ADC_IMM:
    eep_emit_adc_imm(state->emitter, instr->eep->adc_imm);
    break;
  case EEP_INSTR_TY_SBC_IMM:
    eep_emit_sbc_imm(state->emitter, instr->eep->sbc_imm);
    break;
  case EEP_INSTR_TY_AND_IMM:
    eep_emit_and_imm(state->emitter, instr->eep->and_imm);
    break;
  case EEP_INSTR_TY_CMP_IMM:
    eep_emit_cmp_imm(state->emitter, instr->eep->cmp_imm);
    break;
  case EEP_INSTR_TY_LSL:
    eep_emit_lsl(state->emitter, instr->eep->lsl);
    break;
  case EEP_INSTR_TY_LSR:
    eep_emit_lsr(state->emitter, instr->eep->lsr);
    break;
  case EEP_INSTR_TY_ASR:
    eep_emit_asr(state->emitter, instr->eep->asr);
    break;
  case EEP_INSTR_TY_XSR:
    eep_emit_xsr(state->emitter, instr->eep->xsr);
    break;
  case EEP_INSTR_TY_LDR_DIRECT:
    eep_emit_load_direct(state->emitter, instr->eep->ldr_direct);
    break;
  case EEP_INSTR_TY_STR_DIRECT:
    eep_emit_store_direct(state->emitter, instr->eep->str_direct);
    break;
  case EEP_INSTR_TY_LDR_OFFSET:
    eep_emit_load_offset(state->emitter, instr->eep->ldr_offset);
    break;
  case EEP_INSTR_TY_STR_OFFSET:
    eep_emit_store_offset(state->emitter, instr->eep->str_offset);
    break;
  case EEP_INSTR_TY_JMP:
    eep_emit_jump(state->emitter, instr->eep->jmp);
    break;
  case EEP_INSTR_TY_EXT:
    eep_emit_ext(state->emitter, instr->eep->ext);
    break;
  }
}

struct emitted_unit eep_emit(const struct codegen_unit *unit) {
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

      struct eep_emitter *emitter;
      create_eep_emitter(&emitter);

      struct emit_state state = {
          .func = func, .arena = unit->arena, .emitter = emitter};

      struct vector *relocs = vector_create(sizeof(struct relocation));

      struct instr *instr = func->first;
      while (instr) {
        size_t pos = eep_emit_bytesize(state.emitter);

        size_t emitted = eep_emitted_count(state.emitter);
        emit_instr(&state, instr);

        size_t generated_instrs = eep_emitted_count(state.emitter) - emitted;

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

      size_t len = eep_emit_bytesize(emitter);
      void *data = arena_alloc(unit->arena, len);
      eep_emit_copy_to(emitter, data);

      free_eep_emitter(&emitter);

      // FIXME: some vector leaks here (and probably other places)
      // should really alloc in arena
      entries[i] = (struct object_entry){
          .ty = OBJECT_ENTRY_TY_FUNC,
          .data = data,
          .len_data = len,
          .symbol = symbol,
          .relocations = vector_head(relocs),
          .num_relocations = vector_length(relocs),
          .alignment = AARCH64_FUNCTION_ALIGNMENT,
      };
      break;
    }
    }
  }

  struct emitted_unit result = {.entries = entries, .num_entries = num_entries};

  return result;
}

#endif
