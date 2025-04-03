#include "lsp.h"

#include "../alloc.h"
#include "../compiler.h"
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
  struct json_writer *writer;
  struct fcache *fcache;
  struct parsed_args args;
  struct compile_args compile_args;
  const struct target *target;

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
    // header can't be more than 40 long i don't think
    // so this _should_ be big enough
    char buffer[128];
    if (!fgets(buffer, sizeof(buffer), context->in)) {
      BUG("handle bad lsp message");
    }

    if (buffer[0] == '\r' && buffer[1] == '\n') {
      return headers;
    }

    size_t len = strlen(buffer);

    char *brk = strchr(buffer, ':');
    if (!brk) {
      BUG("malformed header");
    }

    size_t header_len = brk - buffer;

    fprintf(log, "Header %.*s\n", (int)header_len, buffer);

    // FIXME: should be case insensitive
    if (!strncmp(buffer, "Content-Length", header_len)) {
      unsigned long long length;

      size_t val_len = len - header_len - /* ': ' */ 2 - /* ending crlf */ 2;

      if (!try_parse_integer(brk + 2, val_len, &length)) {
        BUG("malformed Content-Length value");
      }

      headers.content_length = length;
    } else if (!strncmp(buffer, "Content-Type", header_len)) {
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

static void lsp_parse_doc(struct lsp_context *context,
                          const struct didopen_textdoc_params *doc) {
  struct program program = {
      // FIXME: not sized
      .text = arena_alloc_strndup(context->arena, doc->text_doc.text.str,
                                  doc->text_doc.text.len)};

  // feels hacky
  context->compile_args.syntax_only = true;

  struct compiler *compiler;
  compiler_create(&program, context->fcache, context->target,
                  (struct compile_file){.ty = COMPILE_FILE_TY_NONE},
                  context->args.jcc, &context->compile_args,
                  COMPILE_PREPROC_MODE_PREPROC, &compiler);

  compile(compiler);
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
    lsp_parse_doc(context, &msg->didopen_textdoc_params);
    break;
  case REQ_MSG_METHOD_TEXTDOCUMENT_DIDCLOSE:
    break;
  }
}

static struct req_msg lsp_read_msg(struct lsp_context *context,
                                   const struct lsp_headers *headers) {
  vector_clear(context->read_buf);
  vector_extend(context->read_buf, NULL, headers->content_length);

  fread(vector_head(context->read_buf), 1, headers->content_length,
        context->in);

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

        JSON_WRITE_FIELD(writer, "name", MK_SIZED("jcc-lsp"));
        JSON_WRITE_FIELD(writer, "version", MK_SIZED(JCC_VERSION));
      }
      json_writer_write_end_obj(writer);

      json_writer_write_field_name(writer, MK_SIZED("capabilities"));
      json_writer_write_begin_obj(writer);
      {
        // ServerCapabilities

        JSON_WRITE_FIELD(writer, "positionEncoding", MK_SIZED("utf-8"));
      }
      json_writer_write_end_obj(writer);
    }
    json_writer_write_end_obj(writer);
  }
  json_writer_write_end_obj(writer);

  lsp_write_buf(ctx);
}

int lsp_run(struct arena_allocator *arena, struct fcache *fcache,
            struct parsed_args args, struct compile_args compile_args,
            const struct target *target) {
  fprintf(stderr, "JCC LSP mode\n");
  fprintf(stderr, "This is intended for consumption by an editor\n\n");

  struct lsp_context context = {.arena = arena,
                                .read_buf =
                                    vector_create_in_arena(sizeof(char), arena),
                                .writer = json_writer_create(),
                                .compile_args = compile_args,
                                .args = args,
                                .fcache = fcache,
                                .target = target,
                                .shutdown_recv = false,
                                .in = stdin,
                                .out = stdout,
                                .log = stderr};

  struct lsp_headers headers = lsp_read_headers(&context);
  struct req_msg msg = lsp_read_msg(&context, &headers);

  invariant_assert(msg.method == REQ_MSG_METHOD_INITIALIZE,
                   "expected 'initialize' method as first message from client");

  lsp_write_server_caps(&context, &msg);

  while (true) {
    headers = lsp_read_headers(&context);
    msg = lsp_read_msg(&context, &headers);

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
