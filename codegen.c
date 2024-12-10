#include "codegen.h"

#include "alloc.h"
#include "util.h"
#include "vector.h"

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

  strcpy(&buff[head], "l_");
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

  debug_assert(head == len, "head (%zu) != len (%zu) in mangle_str_cnst_name",
               head, len);

  return buff;
}
