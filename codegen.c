#include "codegen.h"

struct instr *alloc_instr(struct codegen_function *func) {
  struct instr *instr = arena_alloc(func->unit->arena, sizeof(*instr));

  if (!func->first) {
    func->first = instr;
  }

  instr->id = func->instr_count++;
  instr->pred = func->last;
  instr->succ = NULL;
  instr->reloc = NULL;
  instr->p = arena_alloc(func->unit->arena, func->unit->instr_size);

  if (func->last) {
    func->last->succ = instr;
  }

  func->last = instr;

  return instr;
}
