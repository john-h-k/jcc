#ifndef ARGS_H
#define ARGS_H

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

enum arg_ty {
  // e.g `-O3`
  ARG_TY_OPTION,

  // e.g `-arch x86_64`
  ARG_TY_SINGLE,

  // e.g `-I foo -I ../bar`
  ARG_TY_MULTIPLE,
};

struct arg {
  enum arg_ty ty;

  const char *name;

  const char *short_name;
  const char *long_name;

  union {
    bool enabled;
    const char *value;

    struct {
      size_t num_values;
      const char **values;
    };
  };
};

#define ARG_OPT_LIST                                                           \
  ARG_OPT(OPTION, preprocess, "-E", "--preprocess")                            \
  ARG_OPT(OPTION, assembly, "-S", "--assemble")                                \
  ARG_OPT(OPTION, object, "-c", "--compile")                                   \
                                                                               \
  ARG_OPT(SINGLE, arch, "", "-arch")                                           \
  ARG_OPT(SINGLE, target, "", "-target")                                       \
  ARG_OPT(SINGLE, output, "-o", "")                                            \
                                                                               \
  ARG_OPT(SINGLE, c_standard, "", "-std")                                      \
                                                                               \
  ARG_OPT(SINGLE, timestamp, "", "-tm")                                        \
                                                                               \
  ARG_OPT(MULTIPLE, include_paths, "-I", "")

struct parsed_args {
#define ARG_OPT(_0, name, _1, _2) struct arg name;

  ARG_OPT_LIST

#undef ARG_OPT

  size_t num_values;
  const char **values;
};

void debug_print_parsed_args(FILE *file, const struct parsed_args *args);

struct parsed_args parse_args(int argc, char **argv);
void free_args(struct parsed_args *args);

#endif
