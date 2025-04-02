#include "lsp.h"

#include "../alloc.h"
#include "../vector.h"
#include "../hashtbl.h"
#include "../json.h"
#include "lsp_types.h"

#include <stdbool.h>
#include <stdio.h>

struct lsp_headers {
  size_t content_length;
};

struct lsp_msg {
  int _foo;
};

struct lsp_context {
  struct arena_allocator *arena;
  struct vector *read_buf;
  struct hashtbl *obj_props;

  FILE *in;
  FILE *out;
  FILE *log;
};

static struct lsp_headers lsp_read_headers(struct lsp_context *context) {
  FILE *in = context->in;
  FILE *log = context->log;

  struct lsp_headers headers = {.content_length = 0};

  while (true) {
    int c0 = fgetc(in);
    int c1 = fgetc(in);

    if (c0 == '\r' && c1 == '\n') {
      return headers;
    }

    // header can't be more than 40 long i don't think
    char buffer[64] = { (char)c0, (char)c1 };
    if (!fscanf(in, "%61[^:]: ", &buffer[2])) {
      BUG("handle bad lsp message");
    }

    fprintf(log, "Header %s\n", buffer);

    // FIXME: should be case insensitive
    if (!strcmp(buffer, "Content-Length")) {
      // for some reason adding `\r\n` fails?
      // either helix LSP isn't sending the second one or we are doing logic wrong
      invariant_assert(fscanf(in, "%zu", &headers.content_length),
                       "'Content-Length' had invalid value");
    } else if (!strcmp(buffer, "Content-Type")) {
      // ignore
      fprintf(log, "Content-Type: ");

      int c;
      while ((c = fgetc(in)) != EOF) {
        fprintf(log, "%c", c);

        if (c == '\r') {
          int next = fgetc(in);
          if (next == '\n') {
            break;
          }

          ungetc(next, in);
        }
      }

      fprintf(log, "\n");
    } else {
      BUG("unknown header '%s'", buffer);
    }
  }
}

static struct lsp_msg lsp_read_msg(struct lsp_context *context) {
  vector_clear(context->read_buf);

  FILE *in = context->in;

  int c;

  // find open bracket
  while ((c = fgetc(in)) != EOF) {
    if (c == '{') {
      ungetc(c, in);
      break;
    }
  }

  int depth = 0;

  // find match
  while ((c = fgetc(in)) != EOF) {
    vector_push_back(context->read_buf, &(char){(char)c});

    switch (c) {
      case '}':
        depth--;
        break;
      case '{':
        depth++;
        break;
      default:
        break;
    }

    if (!depth) {
      break;
    }
  }

  fprintf(context->log, "object: \n");
  fprintf(context->log, "%.*s\n", (int)vector_length(context->read_buf), (char *)vector_head(context->read_buf));
  fprintf(context->log, "\n");

  struct sized_str obj = {
    .len = vector_length(context->read_buf),
    .str = vector_head(context->read_buf)
  };

  struct json_result result = json_parse(obj);
  json_print(context->log, &result);

  return (struct lsp_msg){0};
}

void lsp_run(void) {
  fprintf(stderr, "JCC LSP mode\n");
  fprintf(stderr, "This is intended for consumption by an editor\n\n");

  struct arena_allocator *arena;
  arena_allocator_create(&arena);

  struct lsp_context context = {.arena = arena,
                                .read_buf =
                                    vector_create_in_arena(sizeof(char), arena),
                                .obj_props =
                                    hashtbl_create_sized_str_keyed_in_arena(arena, sizeof(struct sized_str)),
                                .in = stdin,
                                .out = stdout,
                                .log = stderr};

  while (true) {
    UNUSED struct lsp_headers headers = lsp_read_headers(&context);
    lsp_read_msg(&context);
    break;
  }
}
