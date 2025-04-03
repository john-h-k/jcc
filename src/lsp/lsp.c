#include "lsp.h"

#include "../alloc.h"
#include "../hashtbl.h"
#include "../json.h"
#include "../log.h"
#include "../vector.h"
#include "lsp_types.h"

#include <stdbool.h>
#include <stdio.h>
#include <unistd.h>

struct lsp_headers {
  size_t content_length;
};

struct lsp_context {
  struct arena_allocator *arena;
  struct vector *read_buf;
  struct hashtbl *obj_props;
  struct json_writer *writer;

  bool shutdown_recv;

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
    char buffer[64] = {(char)c0, (char)c1};
    if (!fscanf(in, "%61[^:]: ", &buffer[2])) {
      BUG("handle bad lsp message");
    }

    fprintf(log, "Header %s\n", buffer);

    // FIXME: should be case insensitive
    if (!strcmp(buffer, "Content-Length")) {
      // for some reason adding `\r\n` fails?
      // either helix LSP isn't sending the second one or we are doing logic
      // wrong
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

          invariant_assert(ungetc(next, in) != EOF, "ungetc failed");
        }
      }

      fprintf(log, "\n");
    } else {
      BUG("unknown header '%s'", buffer);
    }
  }
}

static void lsp_handle_msg(struct lsp_context *context,
                           const struct req_msg *msg) {
  switch (msg->method) {
  case REQ_MSG_METHOD_INITIALIZE:
    BUG("duplicate initialize message");
  case REQ_MSG_METHOD_SHUTDOWN:
  case REQ_MSG_METHOD_EXIT:
    BUG("shutdown/exit methods should have been handled");
  case REQ_MSG_METHOD_INITIALIZED:
    break;
  case REQ_MSG_METHOD_TEXTDOCUMENT_DIDOPEN:
  case REQ_MSG_METHOD_TEXTDOCUMENT_DIDCLOSE:
    (void)context;
    break;
  }
}

static struct req_msg lsp_read_msg(struct lsp_context *context) {
  vector_clear(context->read_buf);

  FILE *in = context->in;

  int c;

  // FIXME: use Content-Length header to read in one go

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
  fprintf(context->log, "%.*s\n", (int)vector_length(context->read_buf),
          (char *)vector_head(context->read_buf));
  fprintf(context->log, "\n");

  struct sized_str obj = {.len = vector_length(context->read_buf),
                          .str = vector_head(context->read_buf)};

  struct json_result result = json_parse(obj);

  if (result.ty != JSON_RESULT_TY_VALUE) {
    BUG("handle bad message format");
  }

  struct req_msg msg;
  if (!try_de_req_msg(&result.value, &msg)) {
    BUG("handle message deserialize failure");
  }

  return msg;
}

static void lsp_write_buf(struct lsp_context *ctx) {
  struct sized_str res = json_writer_get_buf(ctx->writer);

  fprintf(ctx->log, "%.*s\n", (int)res.len, res.str);

  fprintf(ctx->out, "Content-Length: %zu\r\n", res.len);
  fprintf(ctx->out, "\r\n");

  fprintf(ctx->out, "%.*s", (int)res.len, res.str);
  fflush(ctx->out);
}

static void lsp_write_server_caps(struct lsp_context *ctx,
                                  const struct req_msg *msg) {
  struct json_writer *writer = ctx->writer;

  json_writer_write_begin_obj(writer);
  {
    // ResponseMessage

    JSON_WRITE_FIELD(writer, "id", msg->id);

    json_writer_write_field_name(writer, MK_SIZED("result"));
    json_writer_write_begin_obj(writer);
    {
      // InitializeResult

      json_writer_write_field_name(writer, MK_SIZED("capabilities"));
      json_writer_write_begin_obj(writer);
      {
        // ServerInfo

        // FIXME: need to check this is available
        JSON_WRITE_FIELD(writer, "name", MK_SIZED("jcc-lsp"));
        JSON_WRITE_FIELD(writer, "version", MK_SIZED(JCC_VERSION));
      }
      json_writer_write_end_obj(writer);

      json_writer_write_field_name(writer, MK_SIZED("capabilities"));
      json_writer_write_begin_obj(writer);
      {
        // ServerCapabilities

        // FIXME: need to check this is available
        JSON_WRITE_FIELD(writer, "positionEncoding", MK_SIZED("utf-8"));
      }
      json_writer_write_end_obj(writer);
    }
    json_writer_write_end_obj(writer);
  }
  json_writer_write_end_obj(writer);

  lsp_write_buf(ctx);
}

int lsp_run(void) {
  fprintf(stderr, "JCC LSP mode\n");
  fprintf(stderr, "This is intended for consumption by an editor\n\n");

  struct arena_allocator *arena;
  arena_allocator_create(&arena);

  FILE *in = stdin;
  FILE *out = stdout;
  FILE *log = stderr;

  struct lsp_context context = {
      .arena = arena,
      .read_buf = vector_create_in_arena(sizeof(char), arena),
      .obj_props = hashtbl_create_sized_str_keyed_in_arena(
          arena, sizeof(struct sized_str)),
      .writer = json_writer_create(),
      .shutdown_recv = false,
      .in = in,
      .out = out,
      .log = log};

  struct lsp_headers headers = lsp_read_headers(&context);
  (void)headers;

  struct req_msg msg = lsp_read_msg(&context);

  invariant_assert(msg.method == REQ_MSG_METHOD_INITIALIZE,
                   "expected 'initialize' method as first message from client");

  lsp_write_server_caps(&context, &msg);

  while (true) {
    headers = lsp_read_headers(&context);
    msg = lsp_read_msg(&context);

    switch (msg.method) {
    case REQ_MSG_METHOD_SHUTDOWN:
      context.shutdown_recv = true;

      info("LSP shutting down...");
      fprintf(stderr, "LSP shutting down...");
      break;
    case REQ_MSG_METHOD_EXIT:
      return context.shutdown_recv ? 0 : 1;
    default:
      lsp_handle_msg(&context, &msg);
    }
  }
}
