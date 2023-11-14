#include "compiler.h"
#include "lex.h"
#include "link.h"
#include "log.h"
#include "macos/mach-o.h"
#include "parse.h"
#include "util.h"
#include "vector.h"
#include <stdio.h>
#include <stdlib.h>

enum parse_args_result {
  PARSE_ARGS_RESULT_SUCCESS = 0,
  PARSE_ARGS_RESULT_NO_SOURCES = 1
};

enum parse_args_result parse_args(int argc, char **argv,
                                  struct compile_args *args,
                                  const char ***sources, size_t *num_sources);

char *readfile(const char *path) {
  FILE *f = fopen(path, "r");

  if (!f) {
    return NULL;
  }

  fseek(f, 0, SEEK_END);
  long fsize = ftell(f);
  rewind(f);

  char *content = nonnull_malloc(fsize + 1);
  fread(content, fsize, 1, f);
  fclose(f);

  content[fsize] = '\0';
  return content;
}

int main(int argc, char **argv) {
  info("parsing command line args");

  struct compile_args args;
  const char **sources;
  size_t num_sources;
  if (parse_args(argc, argv, &args, &sources, &num_sources) !=
      PARSE_ARGS_RESULT_SUCCESS) {
    fprintf(stderr, "%s\n", "parsing command line args failed");
    return -1;
  }

  char **objects = nonnull_malloc(sizeof(*objects) * num_sources);

  info("beginning compilation stage...");
  for (size_t i = 0; i < num_sources; i++) {
    info("compiling source file \"%s\"", sources[i]);

    const char *source = readfile(sources[i]);

    if (!source) {
      err("source file \"%s\" could not be read!", sources[i]);
      return COMPILE_RESULT_BAD_FILE;
    }

    char *output = nonnull_malloc(strlen(sources[i]) + strlen(".obj"));
    strcpy(output, sources[i]);
    strcat(output, ".obj");

    objects[i] = output;

    struct compiler *compiler;
    if (create_compiler(source, output, &args, &compiler) !=
        COMPILER_CREATE_RESULT_SUCCESS) {
      err("failed to create compiler");
      return -1;
    }

    if (compile(compiler) != COMPILE_RESULT_SUCCESS) {
      err("compilation failed!");
      return -1;
    }
  }

  struct link_args link_args = {.objects = (const char **)objects,
                                .num_objects = num_sources,
                                .output = "a.out"};

  if (link_objects(&link_args) != LINK_RESULT_SUCCESS) {
    err("link failed");
    exit(-1);
  }

  for (size_t i = 0; i < num_sources; i++) {
    free(objects[i]);
  }
}

enum parse_args_result parse_args(int argc, char **argv,
                                  struct compile_args *args,
                                  const char ***sources, size_t *num_sources) {

  // should always be true as argv[0] is program namr
  invariant_assert(argc > 0, "argc must be >0");

  *num_sources = (size_t)(argc - 1);

  if (num_sources == 0) {
    return PARSE_ARGS_RESULT_NO_SOURCES;
  }

  *sources = nonnull_malloc(sizeof(*sources) * *num_sources);

  for (size_t i = 0; i < *num_sources; i++) {
    (*sources)[i] = argv[i + 1];
  }

  args->target_arch = COMPILE_TARGET_ARCH_MACOS_ARM64;
  return PARSE_ARGS_RESULT_SUCCESS;
}
