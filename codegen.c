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

struct move_set gen_move_order(struct arena_allocator *arena,
                                      size_t *from, size_t *to, size_t num,
                                      size_t tmp_index) {
  size_t max = 0;

  for (size_t i = 0; i < num; i++) {
    max = MAX(max, from[i]);
    max = MAX(max, to[i]);
  }

  max++;

  struct info {
    size_t pred;
    size_t cur;
  } *infos = arena_alloc(arena, sizeof(*infos) * (max));
  memset(infos, 0, sizeof(*infos) * (max));

  struct vector *non_dependent = vector_create(sizeof(size_t));
  struct vector *remainder = vector_create(sizeof(size_t));
  struct vector *moves = vector_create(sizeof(struct move));

#define NO_LOCATION (SIZE_T_MAX)

  for (size_t i = 0; i < num; i++) {
    size_t source = from[i];
    size_t dest = to[i];
    infos[dest].cur = NO_LOCATION;
    infos[dest].pred = NO_LOCATION;
    infos[source].pred = NO_LOCATION;
  }

  for (size_t i = 0; i < num; i++) {
    size_t source = from[i];
    size_t dest = to[i];

    infos[source].cur = source;
    infos[dest].pred = source;

    vector_push_back(remainder, &dest);
  }

  for (size_t i = 0; i < num; i++) {
    size_t dest = to[i];

    if (infos[dest].cur == NO_LOCATION) {
      vector_push_back(non_dependent, &dest);
    }
  }

  while (!vector_empty(remainder)) {
    while (!vector_empty(non_dependent)) {
      size_t dest = *(size_t *)vector_pop(non_dependent);

      size_t source = infos[dest].pred;
      size_t source_loc = infos[source].cur;

      struct move move = {
          .from = source_loc,
          .to = dest,
      };
      vector_push_back(moves, &move);

      // FIXME: inefficient
      for (size_t i = 0; i < vector_length(remainder); i++) {
        size_t el = *(size_t *)vector_get(remainder, i);
        if (el == source_loc) {
          vector_remove_at(remainder, i);
          break;
        }
      }

      infos[source].cur = dest;

      if (source == source_loc && infos[source].pred != NO_LOCATION) {
        vector_push_back(non_dependent, &source);
      }
    }

    if (vector_empty(remainder)) {
      break;
    }

    size_t dest = *(size_t *)vector_pop(remainder);
    if (dest != infos[infos[dest].pred].cur) {
      struct move move = {
          .from = dest,
          .to = tmp_index,
      };
      vector_push_back(moves, &move);

      infos[dest].cur = tmp_index;
      vector_push_back(non_dependent, &dest);
    }
  }

  struct move_set move_set = {.num_moves = vector_length(moves),
                              .moves =
                                  arena_alloc(arena, vector_byte_size(moves))};

  vector_copy_to(moves, move_set.moves);

  vector_free(&non_dependent);
  vector_free(&remainder);
  vector_free(&moves);

  return move_set;

#undef NO_LOCATION
}
