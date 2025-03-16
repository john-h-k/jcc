#include "codegen.h"

#include "aarch64/isa.h"
#include "alloc.h"
#include "ir/ir.h"
#include "program.h"
#include "target.h"
#include "util.h"
#include "vector.h"

int cg_sort_entries_by_id(const void *a, const void *b) {
  const struct cg_entry *l = a;
  const struct cg_entry *r = b;

  if (l->glb_id > r->glb_id) {
    return 1;
  } else if (l->glb_id == r->glb_id) {
    return 0;
  } else {
    return -1;
  }
}

void cg_rebuild_ids(struct cg_func *func) {
  size_t next_basicblock_id = 0;
  size_t next_instr_id = 0;

  struct cg_basicblock *basicblock = func->first;
  while (basicblock) {
    basicblock->id = next_basicblock_id++;

    struct instr *instr = basicblock->first;

    while (instr) {
      instr->id = next_instr_id++;

      instr = instr->succ;
    }

    basicblock = basicblock->succ;
  }
}

struct instr *cg_get_next_instr(struct cg_basicblock *target) {
  while (!target->first) {
    target = target->succ;
  }

  return target->first;
}

struct cg_basicblock *cg_alloc_basicblock(struct cg_func *func,
                                          struct ir_basicblock *ir_basicblock) {
  struct cg_basicblock *basicblock =
      arena_alloc(func->unit->arena, sizeof(*basicblock));

  if (!func->first) {
    func->first = basicblock;
  }

  *basicblock = (struct cg_basicblock){
      .id = func->basicblock_count++,
      .ir_basicblock = ir_basicblock,
      .func = func,
      .pred = func->last,
      .succ = NULL,
      .first = NULL,
      .last = NULL,
  };

  if (ir_basicblock) {
    ir_basicblock->cg_basicblock = basicblock;
  }

  if (func->last) {
    func->last->succ = basicblock;
  }

  func->last = basicblock;

  return basicblock;
}

struct instr *cg_alloc_instr(struct cg_func *func,
                             struct cg_basicblock *basicblock) {
  struct instr *instr = arena_alloc(func->unit->arena, sizeof(*instr));

  if (!basicblock->first) {
    basicblock->first = instr;
  }

  instr->id = func->instr_count++;
  instr->pred = basicblock->last;
  instr->succ = NULL;
  instr->reloc = NULL;
  instr->op = NULL;
  instr->p = arena_alloc(func->unit->arena, func->unit->instr_size);

  if (basicblock->last) {
    basicblock->last->succ = instr;
  }

  basicblock->last = instr;

  return instr;
}

const char *cg_mangle_str_cnst_name(struct arena_allocator *arena,
                                    const char *func_name, size_t id) {
  // TODO: this should all really be handled by the mach-o file
  func_name = "str";
  size_t func_name_len = strlen(func_name);

  size_t len = 0;
  len += func_name_len;
  len += 2; // strlen("l_"), required for local symbols
  len += 1; // surround function name with `.` so it cannot conflict with real
            // names

  if (id) {
    len += 1; // extra "." before id
  }

  size_t id_len = id ? num_digits(id) : 0;
  len += id_len;

  len += 1; // null char
  char *buff = arena_alloc(arena, len);
  size_t head = 0;

  strcpy(&buff[head], "p_");
  head += strlen("l_");

  buff[head++] = '.';
  strcpy(&buff[head], func_name);
  head += func_name_len;

  if (id) {
    buff[head++] = '.';

    size_t tail = head + id_len - 1;
    while (tail >= head) {
      buff[tail--] = (id % 10) + '0';
      id /= 10;
    }
  }

  head += id_len;
  buff[head++] = 0;

  DEBUG_ASSERT(head == len, "head (%zu) != len (%zu) in mangle_str_cnst_name",
               head, len);

  return buff;
}

static void codegen_write_var_value(struct ir_unit *iru, struct vector *relocs,
                                    size_t offset, struct ir_var_value *value,
                                    char *data) {
  if (!value || value->ty == IR_VAR_VALUE_TY_ZERO) {
    return;
  }

#define COPY(ty, fld)                                                          \
  ty tmp##ty = (ty)value->fld;                                                 \
  memcpy(data, &tmp##ty, sizeof(tmp##ty))
  switch (value->var_ty.ty) {
  case IR_VAR_TY_TY_NONE:
  case IR_VAR_TY_TY_VARIADIC:
    break;
  case IR_VAR_TY_TY_PRIMITIVE: {
    switch (value->var_ty.primitive) {
    case IR_VAR_PRIMITIVE_TY_I1:
    case IR_VAR_PRIMITIVE_TY_I8:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT, "expected int");
      COPY(uint8_t, int_value);
      break;
    case IR_VAR_PRIMITIVE_TY_F16:
    case IR_VAR_PRIMITIVE_TY_I16:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT ||
                       value->ty == IR_VAR_VALUE_TY_FLT,
                   "expected int/flt");
      COPY(uint16_t, int_value);
      break;
    case IR_VAR_PRIMITIVE_TY_I32:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT, "expected int");
      COPY(uint32_t, int_value);
      break;
    case IR_VAR_PRIMITIVE_TY_I64:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT, "expected int");
      COPY(uint64_t, int_value);
      break;
    case IR_VAR_PRIMITIVE_TY_I128:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT, "expected int");
      COPY(uint128_t, int_value);
      break;
    case IR_VAR_PRIMITIVE_TY_F32:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_FLT, "expected flt");
      COPY(float, flt_value);
      break;
    case IR_VAR_PRIMITIVE_TY_F64:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_FLT, "expected flt");
      COPY(double, flt_value);
      break;
    }
    break;
  }
#undef COPY

  case IR_VAR_TY_TY_FUNC:
    BUG("func can not have data as a global var");

    // FIXME: some bugs here around using compiler-ptr-size when we mean to use
    // target-ptr-size

  case IR_VAR_TY_TY_POINTER:
  case IR_VAR_TY_TY_ARRAY:
    switch (value->ty) {
    case IR_VAR_VALUE_TY_ZERO:
    case IR_VAR_VALUE_TY_FLT:
      BUG("doesn't make sense");
    case IR_VAR_VALUE_TY_ADDR: {
      struct relocation reloc = {.ty = RELOCATION_TY_POINTER,
                                 .size = 3,
                                 .address = offset,
                                 .offset = value->addr.offset,
                                 .symbol_index = value->addr.glb->id};

      vector_push_back(relocs, &reloc);

      memcpy(data, &value->addr.offset, sizeof(void *));
      break;
    }
    case IR_VAR_VALUE_TY_INT:
      memcpy(data, &value->int_value, sizeof(void *));
      break;
    case IR_VAR_VALUE_TY_STR: {
      struct ir_var_ty var_ty = value->var_ty;
      DEBUG_ASSERT(var_ty.ty == IR_VAR_TY_TY_ARRAY,
                   "expected IR_VAR_VALUE_TY_STR to be an array");

      struct ir_var_ty ch_ty = *var_ty.array.underlying;

      // can't copy whole array as the string may be shorter and part of it
      // so copy sizeof(ch) * num_chr
      size_t len = value->str_value.len * ir_var_ty_info(iru, &ch_ty).size;
      memcpy(data, value->str_value.value, len);
      break;
    }
    case IR_VAR_VALUE_TY_VALUE_LIST:
      for (size_t i = 0; i < value->value_list.num_values; i++) {
        size_t value_offset = value->value_list.offsets[i];
        codegen_write_var_value(iru, relocs, offset + value_offset,
                                &value->value_list.values[i],
                                &data[value_offset]);
      }
      break;
    }
    break;

  case IR_VAR_TY_TY_STRUCT:
  case IR_VAR_TY_TY_UNION:
    DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_VALUE_LIST,
                 "expected value list");
    for (size_t i = 0; i < value->value_list.num_values; i++) {
      size_t field_offset = value->value_list.offsets[i];
      codegen_write_var_value(iru, relocs, offset + field_offset,
                              &value->value_list.values[i],
                              &data[field_offset]);
    }
  }
}

static struct cg_entry codegen_var_data(struct ir_unit *ir, size_t id,
                                        const char *name, struct ir_glb *glb) {
  enum symbol_ty symbol_ty;
  enum cg_entry_ty entry_ty;

  switch (glb->var->ty) {
  case IR_VAR_TY_STRING_LITERAL: {
    BUG("str literal should have been lowered seperately");
  }
  case IR_VAR_TY_CONST_DATA:
    symbol_ty = SYMBOL_TY_CONST_DATA;
    entry_ty = CG_ENTRY_TY_CONST_DATA;
    goto mk_symbol;
  case IR_VAR_TY_DATA:
    symbol_ty = SYMBOL_TY_DATA;
    entry_ty = CG_ENTRY_TY_DATA;
    goto mk_symbol;

  mk_symbol: {
    struct ir_var_ty_info info = ir_var_ty_info(ir, &glb->var->var_ty);

    // TODO: this leak
    struct vector *relocs = vector_create(sizeof(struct relocation));

    size_t len = info.size;

    char *data = arena_alloc(ir->arena, len);
    memset(data, 0, len);

    codegen_write_var_value(ir, relocs, 0, &glb->var->value, data);

    struct cg_data codegen_data = {.data = data, .len_data = len};

    CLONE_AND_FREE_VECTOR(ir->arena, relocs, codegen_data.num_relocs,
                          codegen_data.relocs);

    struct symbol symbol = {.ty = symbol_ty,
                            .name = name,
                            .visibility = glb->linkage == IR_LINKAGE_EXTERNAL
                                              ? SYMBOL_VISIBILITY_GLOBAL
                                              : SYMBOL_VISIBILITY_PRIVATE};

    return (struct cg_entry){.ty = entry_ty,
                             .glb_id = id,
                             .alignment = info.alignment,
                             .name = name,
                             .data = codegen_data,
                             .symbol = symbol};
  }
  }
}

void cg_detach_basicblock(struct cg_func *func,
                          struct cg_basicblock *basicblock) {
  if (basicblock->id == DETACHED_BASICBLOCK) {
    return;
  }

  invariant_assert(func->basicblock_count,
                   "`detach_cg_basicblock` would underflow basicblock count "
                   "for `cg_builder`");

  size_t instr_count = 0;
  struct instr *instr = basicblock->first;
  while (instr) {
    instr_count++;

    instr = instr->succ;
  }

  func->basicblock_count--;
  func->instr_count -= instr_count;

  basicblock->id = DETACHED_BASICBLOCK;

  // fix links on either side of basicblock
  if (basicblock->pred) {
    basicblock->pred->succ = basicblock->succ;
  } else {
    func->first = basicblock->succ;
  }

  if (basicblock->succ) {
    basicblock->succ->pred = basicblock->pred;
  } else {
    func->last = basicblock->pred;
  }

  basicblock->func = NULL;
}

static struct cg_entry codegen_func(struct cg_unit *unit, struct ir_glb *glb, enum codegen_flags flags) {
  struct ir_func *ir_func = glb->func;

  ir_clear_metadata(ir_func);

  const char *name = unit->target->mangle(unit->arena, glb->name);

  struct symbol symbol = {.ty = SYMBOL_TY_FUNC,
                          .name = name,
                          .visibility = glb->linkage == IR_LINKAGE_EXTERNAL
                                            ? SYMBOL_VISIBILITY_GLOBAL
                                            : SYMBOL_VISIBILITY_PRIVATE};

  struct cg_entry entry = {.ty = CG_ENTRY_TY_FUNC,
                           .alignment = unit->target->function_alignment,
                           .name = name,
                           .glb_id = glb->id,
                           .func = {.unit = unit,
                                    .first = NULL,
                                    .last = NULL,
                                    .instr_count = 0,
                                    .basicblock_count = 0},
                           .symbol = symbol};

  struct cg_func *func = &entry.func;
  struct cg_state state = {.arena = unit->arena,
    .flags = flags,
                           .target = unit->target,
                           .func = func,
                           .ir = ir_func};

  unit->target->codegen.codegen_start(&state);

  // func->prologue = state.prologue_info.prologue_generated;
  // func->stack_size = state.prologue_info.stack_size;

  // currently, after phi elim, ops using the phis still point to the phis
  // rather than the moves so we can't strip them before codegen but they don't
  // generate instructions and so can lead to empty basicblocks we fix this by
  // stripping empty basicblocks before `codegen_end`

  struct ir_basicblock *basicblock = ir_func->first;
  while (basicblock) {
    unit->target->codegen.codegen_basicblock(&state, basicblock);

    basicblock = basicblock->succ;
  }

#ifndef NDEBUG
  basicblock = ir_func->first ? ir_func->first->succ : ir_func->first;
  while (basicblock) {
    if (!basicblock->cg_basicblock) {
      BUG("BB @ %zu did not have a corresponding `cg_basicblock`",
          basicblock->id);
    }

    basicblock = basicblock->succ;
  }
#endif

  cg_rebuild_ids(func);
  unit->target->codegen.codegen_end(&state);
  cg_rebuild_ids(func);

  return entry;
}

struct cg_unit *codegen(struct ir_unit *unit, enum codegen_flags flags) {
  struct cg_unit *codegen_unit = arena_alloc(unit->arena, sizeof(*unit));

  struct codegen_info info = unit->target->codegen;

  *codegen_unit = (struct cg_unit){
      .ty = info.ty,
      .target = unit->target,
      .instr_size = info.instr_sz,
      .num_entries = unit->num_globals,
      .entries = arena_alloc(unit->arena,
                             unit->num_globals * sizeof(struct cg_entry))};

  arena_allocator_create(&codegen_unit->arena);

  struct ir_glb *glb = unit->first_global;

  {
    size_t i = 0;
    while (glb) {
      if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
        const char *name = unit->target->mangle(unit->arena, glb->name);
        struct symbol symbol = {.ty = SYMBOL_TY_DECL,
                                .name = name,
                                .visibility = SYMBOL_VISIBILITY_UNDEF};

        codegen_unit->entries[i] = (struct cg_entry){.ty = CG_ENTRY_TY_DECL,
                                                     .alignment = 0,
                                                     .glb_id = glb->id,
                                                     .name = name,
                                                     .symbol = symbol};

        i++;
        glb = glb->succ;
        continue;
      }

      switch (glb->ty) {
      case IR_GLB_TY_DATA: {
        // TODO: give names relative to func
        const char *name =
            glb->name ? unit->target->mangle(unit->arena, glb->name)
                      : cg_mangle_str_cnst_name(unit->arena, "todo", glb->id);

        switch (glb->var->ty) {
        case IR_VAR_TY_STRING_LITERAL: {
          size_t len = ir_var_ty_info(unit, &glb->var->value.var_ty).size;
          char *data = arena_alloc(unit->arena, len);
          memcpy(data, glb->var->value.str_value.value, len);

          struct symbol symbol = {.ty = SYMBOL_TY_STRING,
                                  .name = name,
                                  .visibility =
                                      glb->linkage == IR_LINKAGE_EXTERNAL
                                          ? SYMBOL_VISIBILITY_GLOBAL
                                          : SYMBOL_VISIBILITY_PRIVATE};

          codegen_unit->entries[i] =
              (struct cg_entry){.ty = CG_ENTRY_TY_STRING,
                                .alignment = 1,
                                .glb_id = glb->id,
                                .name = name,
                                .data = {.data = data, .len_data = len},
                                .symbol = symbol};
          break;
        }
        case IR_VAR_TY_CONST_DATA:
        case IR_VAR_TY_DATA:
          codegen_unit->entries[i] = codegen_var_data(unit, glb->id, name, glb);
          break;
        }
        break;
      }
      case IR_GLB_TY_FUNC:
        codegen_unit->entries[i] = codegen_func(codegen_unit, glb, flags);
        break;
      }

      i++;
      glb = glb->succ;
    }
  }

  if (codegen_unit->entries) {
    qsort(codegen_unit->entries, codegen_unit->num_entries,
          sizeof(struct cg_entry), cg_sort_entries_by_id);
  }

  return codegen_unit;
}

void codegen_free(struct cg_unit **unit) {
  arena_allocator_free(&(*unit)->arena);

  *unit = NULL;
}
