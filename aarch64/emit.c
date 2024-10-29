#include "emit.h"

#include "../aarch64.h"
#include "../ir/prettyprint.h"
#include "../ir/var_refs.h"
#include "../vector.h"
#include "emitter.h"
#include "isa.h"

#include <mach/message.h>
#include <stdio.h>

#define WORD_SIZE (8)

const char *aarch64_mangle(struct arena_allocator *arena, const char *name) {
  char *dest =
      arena_alloc(arena, strlen(name) + /* null terminator + '_' char */ 2);

  dest[0] = '_';
  strcpy(dest + 1, name);

  return dest;
}

struct emit_state {
  const struct codegen_function *func;
  struct arena_allocator *arena;
  struct aarch64_emitter *emitter;
};


static void emit_instr(const struct emit_state *state,
                       const struct instr *instr) {
  switch (instr->aarch64->ty) {
  case AARCH64_INSTR_TY_ADDS:
    aarch64_emit_adds(state->emitter, instr->aarch64->adds);
    break;
  case AARCH64_INSTR_TY_ADDS_IMM:
    aarch64_emit_adds_imm(state->emitter, instr->aarch64->adds_imm);
    break;
  case AARCH64_INSTR_TY_ADD:
    aarch64_emit_add(state->emitter, instr->aarch64->add);
    break;
  case AARCH64_INSTR_TY_ADD_IMM:
    aarch64_emit_add_imm(state->emitter, instr->aarch64->add_imm);
    break;
  case AARCH64_INSTR_TY_ADR:
    aarch64_emit_adr(state->emitter, instr->aarch64->adr);
    break;
  case AARCH64_INSTR_TY_ADRP:
    aarch64_emit_adrp(state->emitter, instr->aarch64->adrp);
    break;
  case AARCH64_INSTR_TY_ANDS:
    aarch64_emit_ands(state->emitter, instr->aarch64->ands);
    break;
  case AARCH64_INSTR_TY_ANDS_IMM:
    aarch64_emit_ands_imm(state->emitter, instr->aarch64->ands_imm);
    break;
  case AARCH64_INSTR_TY_AND:
    aarch64_emit_and(state->emitter, instr->aarch64->and);
    break;
  case AARCH64_INSTR_TY_AND_IMM:
    aarch64_emit_and_imm(state->emitter, instr->aarch64->and_imm);
    break;
  case AARCH64_INSTR_TY_ASRV:
    aarch64_emit_asrv(state->emitter, instr->aarch64->asrv);
    break;
  case AARCH64_INSTR_TY_B:
    aarch64_emit_b(state->emitter, instr->aarch64->b);
    break;
  case AARCH64_INSTR_TY_BL:
    aarch64_emit_bl(state->emitter, instr->aarch64->bl);
    break;
  case AARCH64_INSTR_TY_B_COND:
    aarch64_emit_b_cond(state->emitter, instr->aarch64->b_cond);
    break;
  case AARCH64_INSTR_TY_BC_COND:
    aarch64_emit_bc_cond(state->emitter, instr->aarch64->bc_cond);
    break;
  case AARCH64_INSTR_TY_BFM:
    aarch64_emit_bfm(state->emitter, instr->aarch64->bfm);
    break;
  case AARCH64_INSTR_TY_CBZ:
    aarch64_emit_cbz(state->emitter, instr->aarch64->cbz);
    break;
  case AARCH64_INSTR_TY_CBNZ:
    aarch64_emit_cbnz(state->emitter, instr->aarch64->cbnz);
    break;
  case AARCH64_INSTR_TY_CSEL:
    aarch64_emit_csel(state->emitter, instr->aarch64->csel);
    break;
  case AARCH64_INSTR_TY_CSINC:
    aarch64_emit_csinc(state->emitter, instr->aarch64->csinc);
    break;
  case AARCH64_INSTR_TY_CSINV:
    aarch64_emit_csinv(state->emitter, instr->aarch64->csinv);
    break;
  case AARCH64_INSTR_TY_CSNEG:
    aarch64_emit_csneg(state->emitter, instr->aarch64->csneg);
    break;
  case AARCH64_INSTR_TY_EON:
    aarch64_emit_eon(state->emitter, instr->aarch64->eon);
    break;
  case AARCH64_INSTR_TY_EOR:
    aarch64_emit_eor(state->emitter, instr->aarch64->eor);
    break;
  case AARCH64_INSTR_TY_EOR_IMM:
    aarch64_emit_eor_imm(state->emitter, instr->aarch64->eor_imm);
    break;
  case AARCH64_INSTR_TY_LOAD_IMM:
    aarch64_emit_load_imm(state->emitter, instr->aarch64->ldr_imm);
    break;
  case AARCH64_INSTR_TY_LOAD_BYTE_IMM:
    aarch64_emit_load_byte_imm(state->emitter, instr->aarch64->ldrb_imm);
    break;
  case AARCH64_INSTR_TY_LOAD_HALF_IMM:
    aarch64_emit_load_half_imm(state->emitter, instr->aarch64->ldrh_imm);
    break;
  case AARCH64_INSTR_TY_LOAD_PAIR_IMM:
    aarch64_emit_load_pair_imm(state->emitter, instr->aarch64->ldp_imm);
    break;
  case AARCH64_INSTR_TY_LSLV:
    aarch64_emit_lslv(state->emitter, instr->aarch64->lslv);
    break;
  case AARCH64_INSTR_TY_LSRV:
    aarch64_emit_lsrv(state->emitter, instr->aarch64->lsrv);
    break;
  case AARCH64_INSTR_TY_MADD:
    aarch64_emit_madd(state->emitter, instr->aarch64->madd);
    break;
  case AARCH64_INSTR_TY_MOVN:
    aarch64_emit_movn(state->emitter, instr->aarch64->movn);
    break;
  case AARCH64_INSTR_TY_MOVZ:
    aarch64_emit_movz(state->emitter, instr->aarch64->movz);
    break;
  case AARCH64_INSTR_TY_MOVK:
    aarch64_emit_movk(state->emitter, instr->aarch64->movk);
    break;
  case AARCH64_INSTR_TY_FMOV:
    aarch64_emit_fmov(state->emitter, instr->aarch64->fmov);
    break;
  case AARCH64_INSTR_TY_FCVT:
    aarch64_emit_fcvt(state->emitter, instr->aarch64->fcvt);
    break;
  case AARCH64_INSTR_TY_UCVTF:
    aarch64_emit_ucvtf(state->emitter, instr->aarch64->ucvtf);
    break;
  case AARCH64_INSTR_TY_SCVTF:
    aarch64_emit_scvtf(state->emitter, instr->aarch64->scvtf);
    break;
  case AARCH64_INSTR_TY_MSUB:
    aarch64_emit_msub(state->emitter, instr->aarch64->msub);
    break;
  case AARCH64_INSTR_TY_NOP:
    aarch64_emit_nop(state->emitter);
    break;
  case AARCH64_INSTR_TY_ORN:
    aarch64_emit_orn(state->emitter, instr->aarch64->orn);
    break;
  case AARCH64_INSTR_TY_ORR:
    aarch64_emit_orr(state->emitter, instr->aarch64->orr);
    break;
  case AARCH64_INSTR_TY_ORR_IMM:
    aarch64_emit_orr_imm(state->emitter, instr->aarch64->orr_imm);
    break;
  case AARCH64_INSTR_TY_BR:
    aarch64_emit_br(state->emitter, instr->aarch64->br);
    break;
  case AARCH64_INSTR_TY_BLR:
    aarch64_emit_blr(state->emitter, instr->aarch64->blr);
    break;
  case AARCH64_INSTR_TY_RET:
    aarch64_emit_ret(state->emitter, instr->aarch64->ret);
    break;
  case AARCH64_INSTR_TY_RORV:
    aarch64_emit_rorv(state->emitter, instr->aarch64->rorv);
    break;
  case AARCH64_INSTR_TY_SBFM:
    aarch64_emit_sbfm(state->emitter, instr->aarch64->sbfm);
    break;
  case AARCH64_INSTR_TY_SDIV:
    aarch64_emit_sdiv(state->emitter, instr->aarch64->sdiv);
    break;
  case AARCH64_INSTR_TY_STORE_IMM:
    aarch64_emit_store_imm(state->emitter, instr->aarch64->str_imm);
    break;
  case AARCH64_INSTR_TY_STORE_BYTE_IMM:
    aarch64_emit_store_byte_imm(state->emitter, instr->aarch64->strb_imm);
    break;
  case AARCH64_INSTR_TY_STORE_HALF_IMM:
    aarch64_emit_store_half_imm(state->emitter, instr->aarch64->strh_imm);
    break;
  case AARCH64_INSTR_TY_STORE_PAIR_IMM:
    aarch64_emit_store_pair_imm(state->emitter, instr->aarch64->stp_imm);
    break;
  case AARCH64_INSTR_TY_SUBS:
    aarch64_emit_subs(state->emitter, instr->aarch64->subs);
    break;
  case AARCH64_INSTR_TY_SUB:
    aarch64_emit_sub(state->emitter, instr->aarch64->sub);
    break;
  case AARCH64_INSTR_TY_SUB_IMM:
    aarch64_emit_sub_imm(state->emitter, instr->aarch64->sub_imm);
    break;
  case AARCH64_INSTR_TY_SUBS_IMM:
    aarch64_emit_subs_imm(state->emitter, instr->aarch64->subs_imm);
    break;
  case AARCH64_INSTR_TY_UBFM:
    aarch64_emit_ubfm(state->emitter, instr->aarch64->ubfm);
    break;
  case AARCH64_INSTR_TY_UDIV:
    aarch64_emit_udiv(state->emitter, instr->aarch64->udiv);
    break;
  case AARCH64_INSTR_TY_FADD:
    aarch64_emit_fadd(state->emitter, instr->aarch64->fadd);
    break;
  case AARCH64_INSTR_TY_FMUL:
    aarch64_emit_fmul(state->emitter, instr->aarch64->fmul);
    break;
  case AARCH64_INSTR_TY_FDIV:
    aarch64_emit_fdiv(state->emitter, instr->aarch64->fdiv);
    break;
  case AARCH64_INSTR_TY_FSUB:
    aarch64_emit_fsub(state->emitter, instr->aarch64->fsub);
    break;
  }
}

struct emitted_unit aarch64_emit(const struct codegen_unit *unit) {
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
        .symbol = (struct symbol){
          .ty = SYMBOL_TY_STRING,
          .visibility = SYMBOL_VISIBILITY_GLOBAL, // FIXME:
          .name = entry->name
        }
      };
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
        .symbol = (struct symbol){
          .ty = SYMBOL_TY_CONST_DATA,
          .visibility = SYMBOL_VISIBILITY_GLOBAL, // FIXME:
          .name = entry->name
        }
      };
      break;
    case CODEGEN_ENTRY_TY_DATA:
      entries[i] = (struct object_entry){
        .ty = OBJECT_ENTRY_TY_MUT_DATA,
        .alignment = 0,
        .data = entry->data.data,
        .len_data = entry->data.len_data,
        .num_relocations = 0,
        .relocations = NULL,
        .symbol = (struct symbol){
          .ty = SYMBOL_TY_DATA,
          .visibility = SYMBOL_VISIBILITY_GLOBAL, // FIXME:
          .name = entry->name
        }
      };
      break;
    case CODEGEN_ENTRY_TY_DECL:
      entries[i] = (struct object_entry){
        .ty = OBJECT_ENTRY_TY_DECL,
        .alignment = 0,
        .data = NULL,
        .len_data = 0,
        .num_relocations = 0,
        .relocations = NULL,
        .symbol = (struct symbol){
          .ty = SYMBOL_TY_DECL,
          .visibility = SYMBOL_VISIBILITY_UNDEF,
          .name = entry->name
        }
      };
      break;
    case CODEGEN_ENTRY_TY_FUNC: {
      struct codegen_function *func = &entry->func;

      struct aarch64_emitter *emitter;
      create_aarch64_emitter(&emitter);

      struct emit_state state = {
          .func = func, .arena = unit->arena, .emitter = emitter};

      struct vector *relocs = vector_create(sizeof(struct relocation));

      struct instr *instr = func->first;
      while (instr) {
        size_t pos = aarch64_emit_bytesize(state.emitter);

        size_t emitted = aarch64_emitted_count(state.emitter);
        emit_instr(&state, instr);

        size_t generated_instrs =
            aarch64_emitted_count(state.emitter) - emitted;

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

      size_t len = aarch64_emit_bytesize(emitter);
      void *data = arena_alloc(unit->arena, len);
      aarch64_emit_copy_to(emitter, data);

      free_aarch64_emitter(&emitter);

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
