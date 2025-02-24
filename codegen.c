#include "codegen.h"

#include "alloc.h"
#include "util.h"

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

struct codegen_unit *codegen(struct ir_unit *unit) {
  struct codegen_unit *codegen_unit = arena_alloc(unit->arena, sizeof(*unit));

  struct codegen_info info = unit->target->codegen;

  *codegen_unit = (struct codegen_unit){
      .ty = info.ty,
      .instr_size = info.instr_sz,
      .num_entries = unit->num_globals,
      .entries = arena_alloc(unit->arena,
                             unit->num_globals * sizeof(struct codegen_entry))};

  arena_allocator_create(&codegen_unit->arena);

  info.codegen(codegen_unit, unit);

  struct ir_glb *glb = unit->first_global;

  while (glb) {
    if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
      glb = glb->succ;
      continue;
    }

    switch (glb->ty) {
    case IR_GLB_TY_DATA:
      break;
    case IR_GLB_TY_FUNC: {
      struct ir_basicblock *basicblock = glb->func->first;
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
      break;
    }
    }

    glb = glb->succ;
  }

  return codegen_unit;
}

void codegen_free(struct codegen_unit **unit) {
  arena_allocator_free(&(*unit)->arena);

  *unit = NULL;
}
