#include "args.h"

#include "bit_twiddle.h"
#include "hashtbl.h"
#include "log.h"
#include "util.h"
#include "vector.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

static void debug_print_arg_bool(FILE *file, const struct arg_bool *arg_bool,
                                 const char *(*string_fn)(int value)) {
  DEBUG_ASSERT(!string_fn, "string fn does not make sense for bool");
  fprintf(file, "      %s\n", arg_bool->enabled ? "true" : "false");
}

static void debug_print_arg_string(FILE *file,
                                   const struct arg_string *arg_string,
                                   const char *(*string_fn)(int value)) {
  DEBUG_ASSERT(!string_fn, "string fn does not make sense for string");
  fprintf(file, "      %s\n", arg_string->value);
}

static void
debug_print_arg_string_list(FILE *file,
                            const struct arg_string_list *arg_string_list,
                            const char *(*string_fn)(int value)) {
  DEBUG_ASSERT(!string_fn, "string fn does not make sense for string list");
  fprintf(file, "     ");
  if (!arg_string_list->num_values) {
    fprintf(file, "(none)\n");
    return;
  }

  for (size_t j = 0; j < arg_string_list->num_values; j++) {
    fprintf(file, "'%s'", arg_string_list->values[j]);

    if (j + 1 != arg_string_list->num_values) {
      fprintf(file, ", ");
    }
  }

  fprintf(file, "\n");
}

static void debug_print_arg_option(FILE *file,
                                   const struct arg_option *arg_option,
                                   const char *(*string_fn)(int value)) {
  DEBUG_ASSERT(string_fn, "string fn required for option");
  fprintf(file, "      %s\n", string_fn(arg_option->value));
}

static void debug_print_arg_flags(FILE *file, const struct arg_flags *arg_flags,
                                  const char *(*string_fn)(int value)) {
  DEBUG_ASSERT(string_fn, "string fn required for flags");
  fprintf(file, "      ");

  int value = arg_flags->value;

  if (!value || value == -1) {
    fprintf(file, "%s", string_fn(value));
  } else {
    while (value) {
      int idx = tzcnt(value);

      int flag = NTH_BIT(value, idx);
      value &= ~flag;

      fprintf(file, "%s", string_fn(flag));

      if (value) {
        fprintf(file, " | ");
      }
    }
  }

  fprintf(file, "\n");
}

#define DEBUG_PRINT_ARG(file, arg, string_fn)                                  \
  _Generic((arg),                                                              \
      const struct arg_bool *: debug_print_arg_bool,                           \
      const struct arg_string *: debug_print_arg_string,                       \
      const struct arg_string_list *: debug_print_arg_string_list,             \
      const struct arg_option *: debug_print_arg_option,                       \
      const struct arg_flags *: debug_print_arg_flags)(file, (arg), string_fn)

void debug_print_parsed_args(FILE *file, const struct parsed_args *args) {
  fprintf(file, "PARSED ARGS:\n");

  unsigned long longest_name = 0;

#define ARG_OPT(ty, struct_ty, name, _0, _1, _2, string_fn)                    \
  longest_name = MAX(longest_name, strlen(#name));

  ARG_OPT_LIST;

#undef ARG_OPT

  fprintf(file, "OPTS: \n");

#define ARG_OPT(ty, struct_ty, name, _0, _1, _2, string_fn)                    \
  fprintf(file, "  " #name ": %*s", (int)(longest_name - strlen(#name)), " "); \
  DEBUG_PRINT_ARG(file, &args->name, string_fn);

  ARG_OPT_LIST;

#undef ARG_OPT

  fprintf(file, "\nVALUES: \n");
  for (size_t i = 0; i < args->num_values; i++) {
    const char *value = args->values[i];

    fprintf(file, "  %s\n", value);
  }

  fprintf(file, "\n");
}

void free_args(struct parsed_args *args) {
#define ARG_OPT(ty, struct_ty, name, ...)                                      \
  if (ARG_TY_##ty == ARG_TY_STRING_LIST) {                                     \
    /* this is sort of dodgy as we are doing an illegal cast */                \
    /* however, when it is an illegal cast, it is not executed */              \
    PUSH_NO_WARN("-Wcast-align");                                              \
    struct arg_string_list *string_list =                                      \
        (struct arg_string_list *)&args->name;                                 \
    POP_NO_WARN;                                                               \
                                                                               \
    free(string_list->values);                                                 \
  }

  ARG_OPT_LIST

#undef ARG_OPT

  if (args->values) {
    free(args->values);
  }
}

struct parsed_args parse_args(int argc, char **argv) {
  struct hashtbl *convert =
      hashtbl_create_str_keyed(sizeof(bool (*)(const char *, int *)));

#define ARG_OPT(arg_ty, struct_ty, arg_name, sh, lo, func, ...)                \
  static_assert(!func || ARG_TY_##arg_ty == ARG_TY_OPTION ||                   \
                    ARG_TY_##arg_ty == ARG_TY_FLAGS,                           \
                "should only have func for option/flags");                     \
  static_assert(func || (ARG_TY_##arg_ty != ARG_TY_OPTION &&                   \
                         ARG_TY_##arg_ty != ARG_TY_FLAGS),                     \
                "should have func for option/flags");                          \
  if (func) {                                                                  \
    const char *name = #arg_name;                                              \
    bool (*f)(const char *, int *) = func;                                     \
    hashtbl_insert(convert, &name, &f);                                        \
  }

  ARG_OPT_LIST;

#undef ARG_OPT

  struct hashtbl *opts = hashtbl_create_sized_str_keyed(sizeof(struct arg));

  struct parsed_args parsed = {.values = NULL};

#define ARG_OPT(arg_ty, struct_ty, arg_name, sh, lo, func, ...)                \
  static_assert(sh[0] || lo[0], "must have short or long option");             \
  static_assert(!sh[0] || (sh[0] == '-' && (bool)sh[1] && !(bool)sh[2]),       \
                "short option must begin '-' and be exactly two chars");       \
  static_assert(!lo[0] || (lo[0] == '-' && (bool)lo[1] && (bool)lo[2]),        \
                "long option must begin '-' and be at least three chars");     \
  do {                                                                         \
                                                                               \
    struct arg arg = {.ty = ARG_TY_##arg_ty,                                   \
                      .name = #arg_name,                                       \
                      .short_name = sh,                                        \
                      .long_name = lo};                                        \
                                                                               \
    arg.arg_##struct_ty = &parsed.arg_name;                                    \
                                                                               \
    if (sh[0]) {                                                               \
      struct sized_str full_sh = {.str = sh, .len = strlen(sh)};               \
      hashtbl_insert(opts, &full_sh, &arg);                                    \
    }                                                                          \
    if (lo[0]) {                                                               \
      struct sized_str full_lo = {.str = lo, .len = strlen(lo)};               \
      hashtbl_insert(opts, &full_lo, &arg);                                    \
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

      struct arg *arg = hashtbl_lookup(opts, &lookup_str);
      if (arg) {
        value = len > 2 ? &s[2] : NULL;
      } else {
        value = strchr(s, '=');

        lookup_str =
            (struct sized_str){.len = value ? value - s : len, .str = s};

        arg = hashtbl_lookup(opts, &lookup_str);
      }

      if (!arg) {
        err("Unrecognised option '%.*s'\n", (int)lookup_str.len,
            lookup_str.str);
        exit(-1);
      }

#define GET_ARGUMENT(v)                                                        \
  v = ((v) && (v)[0] ? ((v) && (v)[0] == '=' ? (v) + 1 : v)                    \
                     : (i + 1 == (size_t)argc ? NULL : argv[++i]))

      switch (arg->ty) {
      case ARG_TY_BOOL:
        if (arg->arg_bool->enabled) {
          err("Duplicate option '%.*s'\n", (int)lookup_str.len, lookup_str.str);
          exit(-1);
        }

        arg->arg_bool->enabled = true;
        break;
      case ARG_TY_OPTION:
        if (arg->arg_option->value) {
          err("Duplicate option '%.*s'\n", (int)lookup_str.len, lookup_str.str);
          exit(-1);
        }

        arg->arg_option->value = true;
        break;
      case ARG_TY_FLAGS:
        if (arg->arg_flags->value) {
          err("Duplicate options '%.*s'\n", (int)lookup_str.len,
              lookup_str.str);
          exit(-1);
        }

        arg->arg_flags->value = true;
        break;
      case ARG_TY_STRING:
        GET_ARGUMENT(value);
        if (arg->arg_string->value) {
          err("Duplicate option '%.*s'\n", (int)lookup_str.len, lookup_str.str);
          exit(-1);
        }

        arg->arg_string->value = value;
        break;
      case ARG_TY_STRING_LIST:
        GET_ARGUMENT(value);

        arg->arg_string_list->values =
            realloc(arg->arg_string_list->values,
                    sizeof(*arg->arg_string_list->values) *
                        ++arg->arg_string_list->num_values);
        arg->arg_string_list->values[arg->arg_string_list->num_values - 1] =
            value;
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
