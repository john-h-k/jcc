#include "diagnostics.h"

#include "alloc.h"
#include "hash.h"
#include "hashtbl.h"
#include "vector.h"

#define DIAG_FN(sev, str, _, enum, ty)                                         \
  struct compiler_diagnostic_ty DIAGNOSTIC_PREPROC_##enum = {                  \
      .class = COMPILER_DIAGNOSTIC_CLASS_PREPROC,                              \
      .severity = COMPILER_DIAGNOSTIC_SEVERITY_##sev,                          \
      .name = str,                                                             \
  };

COMPILER_PREPROC_DIAGNOSTIC_LIST

#undef DIAG_FN

#define DIAG_FN(sev, str, _, enum, ty)                                         \
  struct compiler_diagnostic_ty DIAGNOSTIC_PARSER_##enum = {                   \
      .class = COMPILER_DIAGNOSTIC_CLASS_PARSE,                                \
      .severity = COMPILER_DIAGNOSTIC_SEVERITY_##sev,                          \
      .name = str,                                                             \
  };

COMPILER_PARSE_DIAGNOSTIC_LIST

#undef DIAG_FN

#define DIAG_FN(sev, str, _, enum, ty)                                         \
  struct compiler_diagnostic_ty DIAGNOSTIC_SEMANTIC_##enum = {                 \
      .class = COMPILER_DIAGNOSTIC_CLASS_SEMANTIC,                             \
      .severity = COMPILER_DIAGNOSTIC_SEVERITY_##sev,                          \
      .name = str,                                                             \
  };

COMPILER_SEMANTIC_DIAGNOSTIC_LIST

#undef DIAG_FN

struct compiler_diagnostics {
  struct arena_allocator *arena;
  struct compiler_diagnostics_args args;

  bool seen_err;

  struct vector *diagnostics;
  struct hashtbl *seen_diagnostics;
};

static void hash_compiler_diagnostic(struct hasher *hasher, const void *obj) {
  const struct compiler_diagnostic *diag = obj;

  // prevents multiple errors on same line
  // TODO: do better dedupe logic. the aim is to prevent duplicate errors when
  // parser generates errors for the same thing twice due to different parsing
  // paths
  hasher_hash_integer(hasher, diag->span.start.line,
                      sizeof(diag->span.start.line));
  hasher_hash_integer(hasher, diag->ty.class, sizeof(diag->ty.class));
  hasher_hash_integer(hasher, diag->ty.severity, sizeof(diag->ty.severity));
}

static bool eq_compiler_diagnostic(const void *l, const void *r) {
  const struct compiler_diagnostic *ld = l;
  const struct compiler_diagnostic *rd = r;

  return ld->span.start.line == rd->span.start.line &&
         ld->ty.class == rd->ty.class && ld->ty.severity == rd->ty.severity;
}

struct compiler_diagnostics *
compiler_diagnostics_create(struct compiler_diagnostics_args args) {
  struct arena_allocator *arena;
  arena_allocator_create(&arena);

  struct compiler_diagnostics *d = arena_alloc(arena, sizeof(*d));
  *d = (struct compiler_diagnostics){
      .arena = arena,
      .args = args,
      .seen_err = false,
      .diagnostics =
          vector_create_in_arena(sizeof(struct compiler_diagnostic), arena),
      .seen_diagnostics = hashtbl_create_in_arena(
          arena, sizeof(struct compiler_diagnostic), 0,
          hash_compiler_diagnostic, eq_compiler_diagnostic)};

  return d;
}

void compiler_diagnostics_add(struct compiler_diagnostics *diagnostics,
                              struct compiler_diagnostic diagnostic) {
  if (diagnostic.ty.severity == COMPILER_DIAGNOSTIC_SEVERITY_ERROR ||
      (diagnostic.ty.severity == COMPILER_DIAGNOSTIC_SEVERITY_WARN &&
       diagnostics->args.warnings_as_errors)) {
    diagnostics->seen_err = true;

    // do this so LSP also sees error
    // should neaten up how it handles Werror
    diagnostic.ty.severity = COMPILER_DIAGNOSTIC_SEVERITY_ERROR;
  }

  if (hashtbl_lookup(diagnostics->seen_diagnostics, &diagnostic)) {
    return;
  }

  hashtbl_insert(diagnostics->seen_diagnostics, &diagnostic, NULL);

  vector_push_back(diagnostics->diagnostics, &diagnostic);
}

bool compiler_diagnostics_err(struct compiler_diagnostics *diagnostics) {
  return diagnostics->seen_err;
}

void compiler_diagnostics_free(struct compiler_diagnostics **diagnostics) {
  vector_free(&(*diagnostics)->diagnostics);

  // need to copy it out because else it self-frees
  struct arena_allocator *arena = (*diagnostics)->arena;
  arena_allocator_free(&arena);

  *diagnostics = NULL;
}

struct compiler_diagnostics_iter
compiler_diagnostics_iter(struct compiler_diagnostics *diagnostics) {
  return (struct compiler_diagnostics_iter){.diagnostics = diagnostics,
                                            .idx = 0};
}

bool compiler_diagnostics_iter_next(struct compiler_diagnostics_iter *iter,
                                    struct compiler_diagnostic *diagnostic) {
  if (iter->idx >= vector_length(iter->diagnostics->diagnostics)) {
    return false;
  }

  *diagnostic = *(struct compiler_diagnostic *)vector_get(
      iter->diagnostics->diagnostics, iter->idx++);
  return true;
}
