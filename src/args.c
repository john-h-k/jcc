#include "args.h"

#include "compiler.h"
#include "hashtbl.h"
#include "log.h"
#include "util.h"
#include "vector.h"

#include <stdio.h>
#include <string.h>

static void debug_print_arg_bool(FILE *file, const bool *arg_bool,
                                 const char *(*string_fn)(int value)) {
  DEBUG_ASSERT(!string_fn, "string fn does not make sense for bool");
  fprintf(file, "      %s\n", *arg_bool ? "true" : "false");
}

static void debug_print_arg_string(FILE *file, const char *const *arg_string,
                                   const char *(*string_fn)(int value)) {
  DEBUG_ASSERT(!string_fn, "string fn does not make sense for string");
  fprintf(file, "      %s\n", *arg_string);
}

static void
debug_print_arg_string_list(FILE *file,
                            const struct arg_string_list *arg_string_list,
                            const char *(*string_fn)(int value)) {
  DEBUG_ASSERT(!string_fn, "string fn does not make sense for string list");
  fprintf(file, "      ");

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

static void debug_print_arg_option(FILE *file, const int *arg_option,
                                   const char *(*string_fn)(int value)) {
  DEBUG_ASSERT(string_fn, "string fn required for option");
  fprintf(file, "      %s\n",
          *arg_option == 0 ? "(none)" : string_fn(*arg_option));
}

static void debug_print_arg_flags(FILE *file, const int *arg_flags,
                                  const char *(*string_fn)(int value)) {
  DEBUG_ASSERT(string_fn, "string fn required for flags");
  fprintf(file, "      ");

  int value = *arg_flags;

  if (!value || value == -1) {
    fprintf(file, "%s", string_fn(value));
  } else {
    while (value) {
      int idx = tzcnt(value);

      int flag = value & (1 << idx);
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
  fprintf(file, "RAW ARGS:\n");
  for (size_t i = 0; i < (size_t)args->argc; i++) {
    fprintf(file, "    '%s'\n", args->argv[i]);
  }

  fprintf(file, "\nPARSED ARGS:\n");

  unsigned long longest_name = 0;

#define ARG_OPT(ty, struct_ty, name, _0, _1, _2, _3, string_fn, ...)           \
  longest_name = MAX(longest_name, strlen(#name));

  ARG_OPT_LIST

#undef ARG_OPT

  fprintf(file, "OPTS: \n");

#define ARG_OPT(ty, struct_ty, name, _0, _1, _2, _3, string_fn, ...)           \
  fprintf(file, "  " #name ": %*s", (int)(longest_name - strlen(#name) + 1),   \
          " ");                                                                \
  switch (ARG_TY_##ty) {                                                       \
  /* more invalid pointer casting here, but again, only valid casts are        \
   * possible paths */                                                         \
  case ARG_TY_BOOL:                                                            \
    debug_print_arg_bool(file, (const void *)&args->name, string_fn);          \
    break;                                                                     \
  case ARG_TY_OPTION:                                                          \
    debug_print_arg_option(file, (const void *)&args->name, string_fn);        \
    break;                                                                     \
  case ARG_TY_FLAGS:                                                           \
    debug_print_arg_flags(file, (const void *)&args->name, string_fn);         \
    break;                                                                     \
  case ARG_TY_STRING:                                                          \
    debug_print_arg_string(file, (const void *)&args->name, string_fn);        \
    break;                                                                     \
  case ARG_TY_STRING_LIST:                                                     \
    debug_print_arg_string_list(file, (const void *)&args->name, string_fn);   \
    break;                                                                     \
  }

  ARG_OPT_LIST

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
    PUSH_NO_WARN("-Wcast-qual");                                               \
    struct arg_string_list *string_list =                                      \
        (struct arg_string_list *)&args->name;                                 \
    POP_NO_WARN();                                                             \
    POP_NO_WARN();                                                             \
                                                                               \
    for (size_t i = 0; i < string_list->num_values; i++) {                     \
      free((char *)string_list->values[i]);                                    \
    }                                                                          \
                                                                               \
    free(string_list->values);                                                 \
  }

  ARG_OPT_LIST

#undef ARG_OPT

  if (args->values) {
    free(args->values);
  }
}

static void print_opt(const char *sh, const char *lo, const char *desc) {
#ifdef NDEBUG
  if (!strncmp(desc, "[DEBUG]", strlen("[DEBUG]"))) {
    return;
  }
#endif

  if (sh[0] && lo[0]) {
    printf("    %s, %s:\n", sh, lo);
  } else if (sh[0]) {
    printf("    %s:\n", sh);
  } else {
    printf("    %s:\n", lo);
  }

  printf("        %s\n\n", desc);
}

static void print_help(void) {
  printf("JCC\n");
  printf("John Kelly <johnharrykelly@gmail.com>\n");
  printf("\n");
  printf("jcc [OPTIONS] [--] [SOURCES]\n\n");

  printf("SOURCES:\n");
  printf("    - Use '-' to read from stdin, else provide files");
  printf("\n\n");

  printf("OPTIONS:\n");

#define ARG_OPT(_0, _1, name, sh, lo, desc, _2, _3, ...)                       \
  print_opt(sh, lo, desc);

  ARG_OPT_LIST

#undef ARG_OPT
}

static void bad_value(struct arg *arg, ustr_t lookup_str,
                      const char *value) {
  errsl("Invalid value '%s' for option '%.*s'. Valid values: ", value,
        (int)lookup_str.len, lookup_str.str);

  const char **values;
  size_t num_values;
  arg->values(&values, &num_values);

  for (size_t i = 0; i < num_values; i++) {
    printf("%s", values[i]);
    if (i + 1 != num_values) {
      printf(", ");
    }
  }

  printf("\n");
}

enum parse_args_result parse_args(int argc, char **argv,
                                  struct parsed_args *parsed) {
  for (int i = 0; i < argc; i++) {
    DEBUG_ASSERT(argv[i], "arg %d was null!", i);
  }

  struct hashtbl *opts = hashtbl_create_ustr_keyed(sizeof(struct arg));

  *parsed = (struct parsed_args){.argc = argc, .argv = argv, .values = NULL};

#define ARG_OPT(arg_ty, struct_ty, arg_name, sh, lo, desc, parse_fn,           \
                string_fn, values_fn)                                          \
  DEBUG_ASSERT(sh[0] || lo[0], "must have short or long option");              \
  DEBUG_ASSERT(!sh[0] || (sh[0] == '-' && (bool)sh[1] && !(bool)sh[2]),        \
               "short option must begin '-' and be exactly two chars");        \
  DEBUG_ASSERT(!lo[0] || (lo[0] == '-' && (bool)lo[1] && (bool)lo[2]),         \
               "long option must begin '-' and be at least three chars");      \
  do {                                                                         \
    struct arg arg = {.ty = ARG_TY_##arg_ty,                                   \
                      .name = #arg_name,                                       \
                      .try_parse = parse_fn,                                   \
                      .string = string_fn,                                     \
                      .values = values_fn,                                     \
                      .short_name = sh,                                        \
                      .long_name = lo};                                        \
                                                                               \
    switch (ARG_TY_##arg_ty) {                                                 \
      /* more invalid pointer casting here, but again, only valid casts are    \
       * possible paths */                                                     \
      PUSH_NO_WARN("-Wcast-align");                                            \
    case ARG_TY_BOOL:                                                          \
      arg.arg_bool = (bool *)&parsed->arg_name;                                \
      break;                                                                   \
    case ARG_TY_OPTION:                                                        \
      arg.arg_option = (int *)&parsed->arg_name;                               \
      break;                                                                   \
    case ARG_TY_FLAGS:                                                         \
      arg.arg_flags = (int *)&parsed->arg_name;                                \
      break;                                                                   \
    case ARG_TY_STRING:                                                        \
      arg.arg_string = (const char **)&parsed->arg_name;                       \
      break;                                                                   \
    case ARG_TY_STRING_LIST:                                                   \
      arg.arg_string_list = (struct arg_string_list *)&parsed->arg_name;       \
      break;                                                                   \
    }                                                                          \
    if (sh[0]) {                                                               \
      ustr_t full_sh = {.str = sh, .len = strlen(sh)};               \
      hashtbl_insert(opts, &full_sh, &arg);                                    \
    }                                                                          \
    if (lo[0]) {                                                               \
      ustr_t full_lo = {.str = lo, .len = strlen(lo)};               \
      hashtbl_insert(opts, &full_lo, &arg);                                    \
    }                                                                          \
    POP_NO_WARN();                                                             \
  } while (0);

  ARG_OPT_LIST

#undef ARG_OPT

  bool values_only = false;

  struct vector *values = vector_create(sizeof(char *));

  parsed->jcc = argc ? argv[0] : NULL;

  size_t i = 1;

  // default to compile driver
  parsed->driver = JCC_DRIVER_COMPILER;

  if (argc > 1) {
    if (!strcmp(argv[1], "-lsp")) {
      parsed->driver = JCC_DRIVER_LSP;
      i++;
    } else if (!strcmp(argv[1], "-jcc")) {
      parsed->driver = JCC_DRIVER_COMPILER;
      i++;
    }
  }

  for (; i < (size_t)argc; i++) {
    char *s = argv[i];
    size_t len = strlen(s);

    if (!len) {
      continue;
    }

    if (!strcmp(s, "-")) {
      // means read from stdin. handle it seperately to avoid it getting
      // confused for a flag
      vector_push_back(values, &s);
      continue;
    }

    if (!strcmp(s, "--")) {
      values_only = true;
      continue;
    }

    if (!strcmp(s, "-h") || !strcmp(s, "--help") || !strcmp(s, "help")) {
      print_help();
      goto help;
    }

    if (!values_only && len && s[0] == '-') {
      char *value = NULL;

      value = strchr(s, '=');

      ustr_t lookup_str = {.len = value ? value - s : len, .str = s};

      struct arg *arg = hashtbl_lookup(opts, &lookup_str);

      if (!arg) {
        size_t lookup_len = MIN(len, 2);

        const char *comma = strchr(s, ',');
        if (comma) {
          lookup_len = comma - s + 1;
        }

        lookup_str = (ustr_t){.len = lookup_len, .str = s};

        arg = hashtbl_lookup(opts, &lookup_str);
        if (arg) {
          value = len > lookup_len ? &s[lookup_len] : NULL;
        }
      }

      if (!arg) {
        err("Unrecognised option '%.*s'\n", (int)lookup_str.len,
            lookup_str.str);
        goto fail;
      }

#define GET_ARGUMENT(v)                                                        \
  v = ((v) && (v)[0] ? ((v) && (v)[0] == '=' ? (v) + 1 : v)                    \
                     : (i + 1 == (size_t)argc ? NULL : argv[++i]))

      switch (arg->ty) {
      case ARG_TY_BOOL:
        if (*arg->arg_bool) {
          err("Duplicate option '%.*s'\n", (int)lookup_str.len, lookup_str.str);
          goto fail;
        }

        *arg->arg_bool = true;
        break;
      case ARG_TY_OPTION:
        GET_ARGUMENT(value);

        if (!value) {
          err("Value required for option '%.*s'\n", (int)lookup_str.len,
              lookup_str.str);
          goto fail;
        }

        if (*arg->arg_option) {
          err("Duplicate option '%.*s'\n", (int)lookup_str.len, lookup_str.str);
          goto fail;
        }

        if (!arg->try_parse(value, arg->arg_option)) {
          bad_value(arg, lookup_str, value);
          goto fail;
        }
        break;
      case ARG_TY_FLAGS:
        GET_ARGUMENT(value);

        if (!value) {
          err("Value required for option '%.*s'\n", (int)lookup_str.len,
              lookup_str.str);
          goto fail;
        }

        do {
          char *next = strchr(value, ',');

          char *buf = NULL;
          if (next) {
            size_t val_len = next - value;
            // FIXME: strdup could return null via malloc?
            buf = strndup(value, val_len);

            value = buf;
          }

          int flag = 0;
          if (!arg->try_parse(value, &flag)) {
            bad_value(arg, lookup_str, value);
            goto fail;
          }

          if (buf) {
            free(buf);
          }

          // allow `-1` as a duplicate flag as it just means "all"
          // will allow duplicate flags if _all_ have been given, but this is
          // okay
          if (((flag != -1) == (*arg->arg_flags != -1)) &&
              *arg->arg_flags & flag) {
            err("Duplicate options '%.*s'\n", (int)lookup_str.len,
                lookup_str.str);
            goto fail;
          }

          *arg->arg_flags |= flag;

          value = next;
        } while (value ? value++ : 0);
        break;
      case ARG_TY_STRING:
        GET_ARGUMENT(value);

        if (!value) {
          err("Value required for option '%.*s'\n", (int)lookup_str.len,
              lookup_str.str);
          goto fail;
        }

        if (*arg->arg_string) {
          err("Duplicate option '%.*s'\n", (int)lookup_str.len, lookup_str.str);
          goto fail;
        }

        *arg->arg_string = value;
        break;
      case ARG_TY_STRING_LIST:
        GET_ARGUMENT(value);

        if (!value) {
          err("Value required for option '%.*s'\n", (int)lookup_str.len,
              lookup_str.str);
          goto fail;
        }

        do {
          char *next = strchr(value, ',');

          if (next) {
            size_t val_len = next - value;

            value = strndup(value, val_len);
          } else {
            value = strdup(value);
          }

          arg->arg_string_list->values =
              realloc(arg->arg_string_list->values,
                      sizeof(*arg->arg_string_list->values) *
                          ++arg->arg_string_list->num_values);
          arg->arg_string_list->values[arg->arg_string_list->num_values - 1] =
              value;

          value = next;
        } while (value ? value++ : 0);
        break;
      }

      continue;
#undef GET_ARGUMENT
    }

    vector_push_back(values, &s);
  }

#undef ARG_OPT

#ifdef JCC_DEFAULT_TARGET
#define MKSTR_INNER(x) #x
#define MKSTR(x) MKSTR_INNER(x)
  if (parsed->target == 0) {
    invariant_assert(
        parse_target(MKSTR(JCC_DEFAULT_TARGET), (int *)&parsed->target),
        "JCC_DEFAULT_TARGET '%s' was invalid", MKSTR(JCC_DEFAULT_TARGET));
  }
#undef MKSTR
#endif

  size_t num_values = vector_length(values);

  parsed->values = nonnull_malloc(sizeof(*parsed->values) * num_values);
  parsed->num_values = num_values;

  vector_copy_to(values, parsed->values);

  vector_free(&values);

  hashtbl_free(&opts);

  return PARSE_ARGS_RESULT_SUCCESS;

help:
  vector_free(&values);

  hashtbl_free(&opts);

  return PARSE_ARGS_RESULT_HELP;

fail:
  vector_free(&values);

  hashtbl_free(&opts);

  return PARSE_ARGS_RESULT_FAIL;
}
