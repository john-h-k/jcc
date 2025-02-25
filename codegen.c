#include "codegen.h"

#include "alloc.h"
#include "util.h"
#include "vector.h"

int codegen_sort_entries_by_id(const void *a, const void *b) {
  const struct codegen_entry *l = a;
  const struct codegen_entry *r = b;

  if (l->glb_id > r->glb_id) {
    return 1;
  } else if (l->glb_id == r->glb_id) {
    return 0;
  } else {
    return -1;
  }
}

struct instr *alloc_instr(struct codegen_function *func) {
  struct instr *instr = arena_alloc(func->unit->arena, sizeof(*instr));

  if (!func->first) {
    func->first = instr;
  }

  instr->id = func->instr_count++;
  instr->pred = func->last;
  instr->succ = NULL;
  instr->reloc = NULL;
  instr->op = NULL;
  instr->p = arena_alloc(func->unit->arena, func->unit->instr_size);

  if (func->last) {
    func->last->succ = instr;
  }

  func->last = instr;

  return instr;
}

const char *mangle_str_cnst_name(struct arena_allocator *arena,
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
    case IR_VAR_VALUE_TY_STR:
      // FIXME: !!!! doesn't work with string literals containing null char
      strcpy(data, value->str_value);
      break;
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

static struct codegen_entry codegen_var_data(struct ir_unit *ir, size_t id,
                                             const char *name,
                                             struct ir_var *var) {
  switch (var->ty) {
  case IR_VAR_TY_STRING_LITERAL: {
    BUG("str literal should have been lowered seperately");
  }
  case IR_VAR_TY_CONST_DATA:
  case IR_VAR_TY_DATA: {
    struct ir_var_ty_info info = var_ty_info(ir, &var->var_ty);

    // TODO: this leak
    struct vector *relocs = vector_create(sizeof(struct relocation));

    size_t len = info.size;

    char *data = arena_alloc(ir->arena, len);
    memset(data, 0, len);

    codegen_write_var_value(ir, relocs, 0, &var->value, data);

    struct codegen_data codegen_data = {.data = data, .len_data = len};

    CLONE_AND_FREE_VECTOR(ir->arena, relocs, codegen_data.num_relocs,
                          codegen_data.relocs);

    // TODO: handle const data
    return (struct codegen_entry){
        .ty = CODEGEN_ENTRY_TY_DATA,
        .glb_id = id,
        .alignment = info.alignment,
        .name = name,
        .data = codegen_data,
    };
  }
  }
}

static struct codegen_entry codegen_func(struct codegen_unit *unit,
                                         struct ir_glb *glb) {
  struct ir_func *ir_func = glb->func;

  clear_metadata(ir_func);

  struct codegen_entry entry = {
      .ty = CODEGEN_ENTRY_TY_FUNC,
      .alignment = unit->target->function_alignment,
      .glb_id = glb->id,
      .name = unit->target->mangle(unit->arena, ir_func->name),
      .func = {.unit = unit, .first = NULL, .last = NULL, .instr_count = 0}};

  struct codegen_function *func = &entry.func;
  struct codegen_state state = {.arena = unit->arena,
                                .target = unit->target,
                                .func = func,
                                .ir = ir_func};

  unit->target->codegen.codegen_start(&state);

  // func->prologue = state.prologue_info.prologue_generated;
  // func->stack_size = state.prologue_info.stack_size;

  struct ir_basicblock *basicblock = ir_func->first;
  while (basicblock) {
    struct instr *first_pred = func->last;

    unit->target->codegen.codegen_basicblock(&state, basicblock);

    basicblock->first_instr = first_pred ? first_pred->succ : func->first;
    basicblock->last_instr = func->last;

    basicblock = basicblock->succ;
  }

  unit->target->codegen.codegen_end(&state);

  basicblock = glb->func->first;
  while (basicblock) {
    if (!basicblock->first_instr) {
      struct ir_basicblock *succ = basicblock->succ;
      while (!succ->first_instr) {
        succ = succ->succ;
      }

      basicblock->first_instr = succ->first_instr;
    }

    basicblock = basicblock->succ;
  }

  return entry;
}

struct codegen_unit *codegen(struct ir_unit *unit) {
  struct codegen_unit *codegen_unit = arena_alloc(unit->arena, sizeof(*unit));

  struct codegen_info info = unit->target->codegen;

  *codegen_unit = (struct codegen_unit){
      .ty = info.ty,
      .target = unit->target,
      .instr_size = info.instr_sz,
      .num_entries = unit->num_globals,
      .entries = arena_alloc(unit->arena,
                             unit->num_globals * sizeof(struct codegen_entry))};

  arena_allocator_create(&codegen_unit->arena);

  struct ir_glb *glb = unit->first_global;

  {
    size_t i = 0;
    while (glb) {
      if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
        codegen_unit->entries[i] = (struct codegen_entry){
            .ty = CODEGEN_ENTRY_TY_DECL,
            .alignment = 0,
            .glb_id = glb->id,
            .name = unit->target->mangle(unit->arena, glb->name)};

        i++;
        glb = glb->succ;
        continue;
      }

      switch (glb->ty) {
      case IR_GLB_TY_DATA: {
        // TODO: give names relative to func
        const char *name =
            glb->name ? unit->target->mangle(unit->arena, glb->name)
                      : mangle_str_cnst_name(unit->arena, "todo", glb->id);

        switch (glb->var->ty) {
        case IR_VAR_TY_STRING_LITERAL:
          codegen_unit->entries[i] =
              (struct codegen_entry){.ty = CODEGEN_ENTRY_TY_STRING,
                                     .alignment = 1,
                                     .glb_id = glb->id,
                                     .name = name,
                                     .str = glb->var->value.str_value};
          break;
        case IR_VAR_TY_CONST_DATA:
        case IR_VAR_TY_DATA:
          codegen_unit->entries[i] =
              codegen_var_data(unit, glb->id, name, glb->var);
          break;
        }
        break;
      }
      case IR_GLB_TY_FUNC:
        codegen_unit->entries[i] = codegen_func(codegen_unit, glb);
        break;
      }

      i++;
      glb = glb->succ;
    }
  }

  qsort(codegen_unit->entries, codegen_unit->num_entries,
        sizeof(struct codegen_entry), codegen_sort_entries_by_id);

  return codegen_unit;
}

void codegen_free(struct codegen_unit **unit) {
  arena_allocator_free(&(*unit)->arena);

  *unit = NULL;
}
