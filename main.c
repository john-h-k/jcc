#include "lex.h"
#include "log.h"
#include "parse.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>

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

struct compile_args {
  int num_sources;
  const char **sources;
};

enum parse_args_result { PARSE_ARGS_RESULT_SUCCESS = 0 };

enum parse_args_result parse_args(int argc, char **argv,
                                  struct compile_args *args) {
  args->num_sources = argc - 1;
  args->sources = nonnull_malloc(sizeof(*args->sources) * args->num_sources);

  for (int i = 1; i < argc; i++) {
    args->sources[i - 1] = argv[i];
  }

  return PARSE_ARGS_RESULT_SUCCESS;
}

enum compile_source_result {
  COMPILE_SOURCE_RESULT_SUCCESS = 0,
  COMPILE_SOURCE_RESULT_BAD_FILE
};

enum compile_source_result compile_source(struct compile_args *args,
                                          int source_idx);

int main(int argc, char **argv) {
  info("parsing command line args");

  struct compile_args args;
  if (parse_args(argc, argv, &args) != PARSE_ARGS_RESULT_SUCCESS) {
    fprintf(stderr, "%s\n", "parsing command line args failed");
    return -1;
  }

  info("beginning compilation stage...");
  for (int i = 0; i < args.num_sources; i++) {
    if (compile_source(&args, i) != COMPILE_SOURCE_RESULT_SUCCESS) {
    }
  }

  info("finished compilation!");
}

enum compile_source_result compile_source(struct compile_args *args,
                                          int source_idx) {
  const char *source_path = args->sources[source_idx];

  info("compiling source file \"%s\"", source_path);

  const char *source = readfile(source_path);

  if (!source) {
    err("source file \"%s\" could not be read!", source_path);
    return COMPILE_SOURCE_RESULT_BAD_FILE;
  }
  
  struct lexer *lexer;
  if (create_lexer(source, &lexer) != LEX_STATUS_SUCCESS) {
    fprintf(stderr, "%s\n", "lexing failed");
    exit(-1);
  }

  struct parser *parser;
  if (create_parser(lexer, &parser) != PARSER_CREATE_RESULT_SUCCESS) {
    fprintf(stderr, "%s\n", "lexing failed");
    exit(-1);
  }

  struct parse_result result = parse(parser);
  (void)result;

  return COMPILE_SOURCE_RESULT_SUCCESS;
}
