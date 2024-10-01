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

  struct vector *strings;
  size_t total_str_len;

  size_t num_extra_stack_slots;

  // the maximum number of variadics used in this function
  // we offset all stack vars beneath this as it is easier than worrying about
  // lcl lifetimes
  size_t max_variadic_args;

  // registers that need to be reloaded
  unsigned long need_reload_registers;
};

// static struct aarch64_reg get_reg_for_idx(struct ir_reg reg) {
//   // [w|x]18 not available
//   switch (reg.ty) {
//   case IR_REG_TY_NONE:
//   case IR_REG_TY_SPILLED:
//   case IR_REG_TY_FLAGS:
//     bug("reg type invalid");
//   case IR_REG_TY_INTEGRAL: {
//     struct aarch64_reg reg = {.idx = reg.idx < 18 ? reg.idx : reg.idx + 1};
//     invariant_assert(reg.idx <= 31, "invalid reg!");
//     return reg;
//   }
//   case IR_REG_TY_FP: {
//     struct aarch64_reg reg = {.idx = reg.idx < 18 ? reg.idx : reg.idx + 1};
//     invariant_assert(reg.idx <= 31, "invalid reg!");
//     return reg;
//   }
//   }
// }


// static unsigned get_lcl_stack_offset(struct emit_state *state,
//                                      const struct ir_lcl *lcl) {
//   // FIXME: wrongly assumes everything is 8 byte
//   return state->max_variadic_args * 8 + lcl->offset;
// }

// static unsigned get_lcl_stack_offset_32(struct emit_state *state,
//                                         const struct ir_lcl *lcl) {
//   unsigned abs_offset = get_lcl_stack_offset(state, lcl);
//   debug_assert(abs_offset % 4 == 0, "stack offset not divisible by 4");
//   return abs_offset / 4;
// }

// static unsigned get_lcl_stack_offset_64(struct emit_state *state,
//                                         const struct ir_lcl *lcl) {
//   unsigned abs_offset = get_lcl_stack_offset(state, lcl);
//   debug_assert(abs_offset % 8 == 0, "stack offset not divisible by 8");
//   return abs_offset / 8;
// }

enum aarch64_relocation_ty {
  AARCH64_RELOCATION_TY_B,
  AARCH64_RELOCATION_TY_ADRP_ADD
};

struct aarch64_relocation {
  enum aarch64_relocation_ty ty;

  struct relocation reloc;
};

// static void emit_call(struct emit_state *state, struct ir_op *op) {
//   switch (op->call.target->ty) {
//   case IR_OP_TY_GLB_REF: {
//     // this uses relocs instead of actually calculating it
//     struct ir_op_glb_ref *glb_ref = &op->call.target->glb_ref;
//     invariant_assert(glb_ref->ty == IR_OP_GLB_REF_TY_SYM,
//                      "only symbols make sense for call targets");

//     glb_ref->metadata =
//         arena_alloc(state->arena, sizeof(struct aarch64_relocation));
//     struct aarch64_relocation *reloc =
//         (struct aarch64_relocation *)glb_ref->metadata;
//     reloc->ty = AARCH64_RELOCATION_TY_B;
//     reloc->reloc = (struct relocation){
//         // this is not actually the address!!
//         // this is the offset WITHIN the function
//         // we let `compiler.c` fix up the address
//         .ty = RELOCATION_TY_SINGLE,
//         .address = aarch64_emit_bytesize(state->emitter),
//         .size = 2,
//         .sym = (struct sym_relocation){
//             .symbol_name =
//                 aarch64_mangle(state->arena, op->call.target->glb_ref.sym_name),
//         }};

//     break;
//   }
//   default:
//     todo("non GLB calls");
//     break;
//   }
// }



void emit_br_op(struct emit_state *state, struct ir_op *op) {
  if (op->stmt->basicblock->ty == IR_BASICBLOCK_TY_MERGE) {
    struct ir_basicblock *target = op->stmt->basicblock->merge.target;
    ssize_t offset = (ssize_t)target->function_offset -
                     (ssize_t)aarch64_emitted_count(state->emitter);
    (void)offset;
    // aarch64_emit_b(state->emitter, offset);
  } else {
    // otherwise, this is the false branch of a SPLIT
    struct ir_basicblock *false_target =
        op->stmt->basicblock->split.false_target;

    ssize_t false_offset = (ssize_t)false_target->function_offset -
                           (ssize_t)aarch64_emitted_count(state->emitter);
    (void)false_offset;
    // aarch64_emit_b(state->emitter, false_offset);
  }
}

// static void emit_page(struct emit_state *state, struct ir_op *op) {

//   struct ir_op *value = op->custom.aarch64->page.glb_ref;
//   debug_assert(value->ty == IR_OP_TY_GLB_REF,
//                "received non glb_ref op in emit_mov_glb");

//   value->glb_ref.metadata =
//       arena_alloc(state->arena, sizeof(struct aarch64_relocation));
//   struct aarch64_relocation *reloc =
//       (struct aarch64_relocation *)value->glb_ref.metadata;

//   reloc->ty = AARCH64_RELOCATION_TY_ADRP_ADD;
//   reloc->reloc = (struct relocation){
//       // this is not actually the address!!
//       // this is the offset WITHIN the function
//       // we let `compiler.c` fix up the address
//       .ty = RELOCATION_TY_PAIR,
//       .address = aarch64_emit_bytesize(state->emitter),
//       .size = 2,
//       .str = (struct str_relocation){
//           .str_index = value->glb_ref.string->index_from_back}};

//   // aarch64_emit_adrp(state->emitter, 0, get_reg_for_idx(op->reg));
// }


static void emit_instr(const struct emit_state *state,
                       const struct instr *instr) {
  switch (instr->aarch64->ty) {
  case AARCH64_INSTR_TY_ADDS:
    aarch64_emit_adds(state->emitter, instr->aarch64->adds);
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
    aarch64_emit_bc_cond(state->emitter, instr->aarch64->b_cond);
    break;
  case AARCH64_INSTR_TY_BC_COND:
    aarch64_emit_bc_cond(state->emitter, instr->aarch64->bc_cond);
    break;
  case AARCH64_INSTR_TY_BFM_IMM:
    aarch64_emit_bfm_imm(state->emitter, instr->aarch64->bfm);
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
  case AARCH64_INSTR_TY_MOVN_IMM:
    todo("movn imm");
    break;
  case AARCH64_INSTR_TY_MOV_IMM:
    aarch64_emit_mov_imm(state->emitter, instr->aarch64->mov_imm);
    break;
  case AARCH64_INSTR_TY_MVN:
    aarch64_emit_mvn(state->emitter, instr->aarch64->mvn);
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
  case AARCH64_INSTR_TY_RET:
    aarch64_emit_ret(state->emitter, instr->aarch64->ret);
    break;
  case AARCH64_INSTR_TY_RORV:
    aarch64_emit_rorv(state->emitter, instr->aarch64->rorv);
    break;
  case AARCH64_INSTR_TY_SBFM_IMM:
    aarch64_emit_sbfm_imm(state->emitter, instr->aarch64->sbfm);
    break;
  case AARCH64_INSTR_TY_SDIV:
    aarch64_emit_sdiv(state->emitter, instr->aarch64->sdiv);
    break;
  case AARCH64_INSTR_TY_STORE_IMM:
    aarch64_emit_store_imm(state->emitter, instr->aarch64->str_imm);
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
  case AARCH64_INSTR_TY_UBFM_IMM:
    aarch64_emit_ubfm_imm(state->emitter, instr->aarch64->ubfm);
    break;
  case AARCH64_INSTR_TY_UDIV:
    aarch64_emit_udiv(state->emitter, instr->aarch64->udiv);
    break;
  }
}

struct compiled_function
aarch64_emit_function(const struct codegen_function *func) {
  size_t max_variadic_args = 0;

  struct aarch64_emitter *emitter;
  create_aarch64_emitter(&emitter);

  struct emit_state state = {.func = func,
                             .arena = func->arena,
                             .emitter = emitter,
                             .strings = vector_create(sizeof(const char *)),
                             .total_str_len = 0,
                             .num_extra_stack_slots = 0,
                             .max_variadic_args = max_variadic_args};

  struct instr *instr = func->first;
  while (instr) {
    emit_instr(&state, instr);

    instr = instr->succ;
  }

  size_t len = aarch64_emit_bytesize(emitter);
  void *data = arena_alloc(func->arena, len);
  aarch64_emit_copy_to(emitter, data);

  // now deal with all the relocs which are hidden in the globals
  // FIXME:
  struct ir_op_glb_ref *global_refs = NULL;//func->global_refs;

  // first pass to get number
  size_t num_relocations = 0;
  size_t num_relocation_instrs = 0;
  while (global_refs) {
    struct aarch64_relocation *reloc =
        (struct aarch64_relocation *)global_refs->metadata;

    num_relocations++;

    switch (reloc->ty) {
    case AARCH64_RELOCATION_TY_B:
      num_relocation_instrs += 1;
      break;
    case AARCH64_RELOCATION_TY_ADRP_ADD:
      num_relocation_instrs += 2;
      break;
    }

    global_refs = global_refs->succ;
  }

  struct relocation *relocations =
      arena_alloc(func->arena, sizeof(*relocations) * num_relocations);

  // FIXME:
  global_refs = NULL;// func->global_refs;
  size_t i = 0;
  while (global_refs) {
    struct aarch64_relocation *reloc =
        (struct aarch64_relocation *)global_refs->metadata;

    relocations[i++] = reloc->reloc;

    global_refs = global_refs->succ;
  }

  // FIXME:
  struct ir_string *strings = NULL;//func->strings;
  while (strings) {
    vector_push_back(state.strings, &strings->data);

    strings = strings->succ;
  }

  // FIXME: the vector was built backwards due to the structure of the
  // linked-list of strings really this should be dealt with elsewhere more
  // generally, but for now just reverse this vector

  // turn the `struct ir_string`s into normal strings
  struct vector *string_vec = vector_create(sizeof(const char *));

  size_t num_strings = vector_length(state.strings);
  for (size_t i = 0; i < num_strings; i++) {
    size_t idx = num_strings - 1 - i;
    const char *string = *(const char **)vector_get(state.strings, idx);

    vector_push_back(string_vec, &string);
  }

  free_aarch64_emitter(&emitter);

  // FIXME: some vector leaks here (and probably other places)
  // should really alloc in arena
  struct compiled_function result = {
      .name = aarch64_mangle(func->arena, func->name),
      .code = data,
      .len_code = len,
      .relocations = relocations,
      .num_relocations = num_relocations,
      .num_relocation_instrs = num_relocation_instrs,
      .strings = vector_head(string_vec),
      .num_strings = vector_length(string_vec)};

  return result;
}
