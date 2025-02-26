#include "args.h"

#include "hashtbl.h"
#include "log.h"
#include "util.h"
#include "vector.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

void debug_print_parsed_args(FILE *file, const struct parsed_args *args) {
  fprintf(file, "PARSED ARGS:\n");

  fprintf(stderr, "  OPTS: \n");
  for (size_t i = 0; i < args->num_args; i++) {
    struct arg *arg = &args->args[i];

    fprintf(stderr, "    %s (short=%s, long=%s):\n", arg->name, arg->short_name,
            arg->long_name);

    switch (arg->ty) {
    case ARG_TY_OPTION:
      fprintf(stderr, "      %s\n", arg->enabled ? "true" : "false");
      break;
    case ARG_TY_SINGLE:
      fprintf(stderr, "      %s\n", arg->value);
      break;
    case ARG_TY_MULTIPLE:
      for (size_t j = 0; j < arg->num_values; j++) {
        fprintf(stderr, "      %s\n", arg->values[j]);
      }
      break;
    }

    fprintf(stderr, "\n");
  }

  fprintf(stderr, "  VALUES: \n");
  for (size_t i = 0; i < args->num_values; i++) {
    const char *value = args->values[i];

    fprintf(stderr, "    %s\n", value);
  }
}

void free_args(struct parsed_args *args) {
#define ARG_OPT(ty, name, _1, _2)                                              \
  if (ARG_TY_##ty == ARG_TY_MULTIPLE) {                                        \
    struct arg *arg = &args->name;                                             \
                                                                               \
    free(arg->values);                                                         \
  }

  ARG_OPT_LIST

#undef ARG_OPT

  if (args->values) {
    free(args->values);
  }
}

struct parsed_args parse_args(int argc, char **argv) {
  struct hashtbl *opts =
      hashtbl_create_sized_str_keyed(sizeof(struct arg *));

  struct parsed_args parsed = {
    .values = NULL
  };

#define ARG_OPT(arg_ty, arg_name, sh, lo)                                      \
  static_assert(sh[0] || lo[0], "must have short or long option");             \
  static_assert(!sh[0] || (sh[0] == '-' && (bool)sh[1] && !(bool)sh[2]),       \
                "short option must begin '-' and be exactly two chars");       \
  static_assert(!lo[0] || (lo[0] == '-' && (bool)lo[1] && (bool)lo[2]),        \
                "long option must begin '-' and be at least three chars");     \
  do {                                                                         \
                                                                               \
    struct arg *arg = &parsed.arg_name; \
    *arg = (struct arg){.ty = ARG_TY_##arg_ty,                                   \
                      .name = #arg_name,                                       \
                      .short_name = sh,                                        \
                      .long_name = lo};                                        \
    switch (ARG_TY_##arg_ty) {                                                 \
    case ARG_TY_OPTION:                                                        \
      arg->enabled = false;                                                     \
      break;                                                                   \
    case ARG_TY_SINGLE:                                                        \
      arg->value = NULL;                                                        \
      break;                                                                   \
    case ARG_TY_MULTIPLE:                                                      \
      arg->num_values = 0;                                                      \
      arg->values = NULL;                                                       \
      break;                                                                   \
    }                                                                          \
    if (sh[0]) {                                                               \
      struct sized_str full_sh = {.str = sh, .len = strlen(sh)};               \
      hashtbl_insert(opts, &full_sh, arg);                                   \
    }                                                                          \
    if (lo[0]) {                                                               \
      struct sized_str full_lo = {.str = lo, .len = strlen(lo)};               \
      hashtbl_insert(opts, &full_lo, arg);                                   \
    }                                                                          \
  } while (0);

  ARG_OPT_LIST;

#undef ARG_OPT

  bool values_only = false;

  struct vector *values = vector_create(sizeof(char *));

  for (size_t i = 1; i < (size_t)argc; i++) {
    char *s = argv[i];
    size_t len = strlen(s);

    if (!strcmp(s, "--")) {
      values_only = true;
      continue;
    }

    if (!values_only && len && s[0] == '-') {
      const char *value = NULL;

      struct sized_str lookup_str = {.len = MIN(len, 2), .str = s};

      struct arg **data = hashtbl_lookup(opts, &lookup_str);
      if (data) {
        value = len > 2 ? &s[2] : NULL;
      } else {
        value = strchr(s, '=');

        lookup_str =
            (struct sized_str){.len = value ? value - s : len, .str = s};

        data = hashtbl_lookup(opts, &lookup_str);
      }

      if (!data) {
        err("Unrecognised option '%.*s'\n", (int)lookup_str.len,
            lookup_str.str);
        exit(-1);
      }

#define GET_ARGUMENT(v)                                                        \
  v = ((v) && (v)[0] ? ((v) && (v)[0] == '=' ? (v) + 1 : v)                    \
                     : (i + 1 == (size_t)argc ? NULL : argv[++i]))

       struct arg *arg = *data;

      switch (arg->ty) {
      case ARG_TY_OPTION:
        if (arg->enabled) {
          err("Duplicate option '%.*s'\n", (int)lookup_str.len, lookup_str.str);
          exit(-1);
        }

        arg->enabled = true;
        break;
      case ARG_TY_SINGLE:
        GET_ARGUMENT(value);
        if (arg->value) {
          err("Duplicate option '%.*s'\n", (int)lookup_str.len, lookup_str.str);
          exit(-1);
        }

        arg->value = value;
        break;
      case ARG_TY_MULTIPLE:
        GET_ARGUMENT(value);

        arg->values = realloc(arg->values, sizeof(*arg->values) * ++arg->num_values);
        arg->values[arg->num_values - 1] = value;
        break;
      }

      continue;
#undef GET_ARGUMENT
    }

    vector_push_back(values, &s);
  }

#undef ARG_OPT

  size_t num_values = vector_length(values);

  parsed.values = nonnull_malloc(sizeof(*parsed.values) * num_values);
  parsed.num_values = num_values;

  vector_copy_to(values, parsed.values);
  vector_free(&values);

  return parsed;
}
