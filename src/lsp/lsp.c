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

struct lsp_ctx {
  struct arena_allocator *arena;
  struct vector *read_buf;
  struct json_writer *writer;
  struct fcache *fcache;
  struct parsed_args args;
  struct compile_args compile_args;
  const struct target *target;

  struct init_params init_params;

  bool shutdown_recv;

  FILE *in;
  FILE *out;
  FILE *log;
};

struct lsp_doc_ctx {
  struct text_doc doc;
};

static struct lsp_headers lsp_read_headers(struct lsp_ctx *ctx) {
  FILE *in = ctx->in;
  FILE *log = ctx->log;

  struct lsp_headers headers = {.content_length = 0};

  while (true) {
    // header can't be more than 40 long i don't think
    // so this _should_ be big enough
    char buffer[128];
    if (!fgets(buffer, sizeof(buffer), ctx->in)) {
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

static void lsp_write_diagnostics(struct lsp_ctx *ctx,
                                  struct lsp_doc_ctx *doc_ctx,
                                  struct compiler_diagnostics *diagnostics);

static void lsp_parse_doc(struct lsp_ctx *ctx,
                          const struct didopen_textdoc_params *doc) {
  struct program program = {
      // FIXME: not sized
      .text = arena_alloc_strndup(ctx->arena, doc->text_doc.text.str,
                                  doc->text_doc.text.len)};

  struct compiler_create_args comp_args = {
      .program = program,
      .fcache = ctx->fcache,
      .target = ctx->target,
      .args = ctx->compile_args,
      // TODO: working dir
      // .working_dir = ctx->source_path,
      .mode = COMPILE_PREPROC_MODE_PREPROC,
      .output = COMPILE_FILE_NONE,
  };

  struct compiler *compiler;
  if (compiler_create(&comp_args, &compiler) !=
      COMPILER_CREATE_RESULT_SUCCESS) {
    BUG("handle compiler create failure");
  }

  compile(compiler);

  struct compiler_diagnostics *diagnostics = compiler_get_diagnostics(compiler);

  struct lsp_doc_ctx doc_ctx = {.doc = doc->text_doc};

  lsp_write_diagnostics(ctx, &doc_ctx, diagnostics);
}

static void lsp_handle_msg(struct lsp_ctx *ctx, const struct req_msg *msg) {
  switch (msg->method) {
  case REQ_MSG_METHOD_INITIALIZE:
    BUG("duplicate initialize message");
  case REQ_MSG_METHOD_SHUTDOWN:
  case REQ_MSG_METHOD_EXIT:
    BUG("shutdown/exit methods should have been handled");
  case REQ_MSG_METHOD_INITIALIZED:
    break;
  case REQ_MSG_METHOD_TEXTDOCUMENT_DIDOPEN:
    lsp_parse_doc(ctx, &msg->didopen_textdoc_params);
    break;
  case REQ_MSG_METHOD_TEXTDOCUMENT_DIDCLOSE:
    break;
  }
}

static struct req_msg lsp_read_msg(struct lsp_ctx *ctx) {
  struct lsp_headers headers = lsp_read_headers(ctx);

  vector_clear(ctx->read_buf);
  vector_extend(ctx->read_buf, NULL, headers.content_length);

  fread(vector_head(ctx->read_buf), 1, headers.content_length, ctx->in);

  struct sized_str obj = {.len = vector_length(ctx->read_buf),
                          .str = vector_head(ctx->read_buf)};

  struct json_result result = json_parse(obj);

  if (result.ty != JSON_RESULT_TY_VALUE) {
    BUG("handle bad message format");
  }

  struct json_de_ctx de_ctx = {
      .arena = ctx->arena,
  };

  struct req_msg msg;
  if (!try_de_req_msg(&de_ctx, &result.value, &msg)) {
    BUG("handle message deserialize failure");
  }

  return msg;
}

static void lsp_write_buf(struct lsp_ctx *ctx) {
  struct sized_str res = json_writer_get_buf(ctx->writer);

  fprintf(ctx->log, "%.*s\n", (int)res.len, res.str);

  fprintf(ctx->out, "Content-Length: %zu\r\n", res.len);
  fprintf(ctx->out, "\r\n");

  fprintf(ctx->out, "%.*s", (int)res.len, res.str);
  fflush(ctx->out);

  // safe to clear buffer now
  json_writer_clear(ctx->writer);
}

// nice trick:
// you can use `for (int a = 0; !a; a++, <CODE>)` to run code _after_ a block

#define LSP_WRITE_MESSAGE(block)                                               \
  json_writer_write_begin_obj(ctx->writer);                                    \
  JSON_WRITE_FIELD(ctx->writer, "jsonrpc", MK_SIZED("2.0"));                   \
  {block};                                                                     \
  json_writer_write_end_obj(ctx->writer);                                      \
  lsp_write_buf(ctx);

// Writes a `struct text_pos` into a `Position` LSP type
static void lsp_json_write_pos(struct lsp_ctx *ctx, struct text_pos pos) {
  struct json_writer *writer = ctx->writer;

  JSON_OBJECT(writer, NULL, {
    // FIXME: a few things to check here
    //   * are our lines starting at the same line as LSP expects (zero vs one
    //   indexing)
    //   * characters may not line up when UTF text is present as we do byte
    //   columns
    JSON_WRITE_FIELD(writer, "line", pos.line);
    JSON_WRITE_FIELD(writer, "character", pos.col);
  });
}

// Writes a `struct text_span` into a `Range` LSP type
static void lsp_json_write_span(struct lsp_ctx *ctx, struct text_span span) {
  struct json_writer *writer = ctx->writer;

  JSON_OBJECT(writer, NULL, {
    JSON_WRITE_FIELD_NAME(writer, "start");
    lsp_json_write_pos(ctx, span.start);

    JSON_WRITE_FIELD_NAME(writer, "end");
    lsp_json_write_pos(ctx, span.end);
  });
}

static void lsp_write_diagnostics(struct lsp_ctx *ctx,
                                  struct lsp_doc_ctx *doc_ctx,
                                  struct compiler_diagnostics *diagnostics) {
  struct json_writer *writer = ctx->writer;

  LSP_WRITE_MESSAGE({
    JSON_WRITE_FIELD(writer, "method",
                     MK_SIZED("textDocument/publishDiagnostics"));

    JSON_OBJECT(writer, "params", {
      // PublishDiagnosticsParams

      JSON_WRITE_FIELD(writer, "uri", doc_ctx->doc.uri);
      JSON_WRITE_FIELD(writer, "version", doc_ctx->doc.version);

      JSON_ARRAY(writer, "diagnostics", {
        // Diagnostic[]

        struct compiler_diagnostics_iter iter =
            compiler_diagnostics_iter(diagnostics);
        struct compiler_diagnostic diagnostic;
        while (compiler_diagnostics_iter_next(&iter, &diagnostic)) {
          struct text_span span = diagnostic.span;

          if (span.start.file && span.end.file &&
              strcmp(span.start.file, span.end.file)) {
            // diff files, LSP does not support
            continue;
          }

          JSON_OBJECT(writer, NULL, {
            JSON_WRITE_FIELD_NAME(writer, "range");
            lsp_json_write_span(ctx, span);

            JSON_WRITE_FIELD_NAME(writer, "severity");
            switch (diagnostic.ty.severity) {
            case COMPILER_DIAGNOSTIC_SEVERITY_ERROR:
              json_writer_write_integer(writer, DIAGNOSTIC_SEVERITY_ERROR);
              break;
            case COMPILER_DIAGNOSTIC_SEVERITY_WARN:
              json_writer_write_integer(writer, DIAGNOSTIC_SEVERITY_WARNING);
              break;
            case COMPILER_DIAGNOSTIC_SEVERITY_INFO:
              json_writer_write_integer(writer, DIAGNOSTIC_SEVERITY_INFO);
              break;
            }

            // TODO:
            // code?: integer | string
            // codeDescription?: CodeDescription;

            JSON_WRITE_FIELD(writer, "source", MK_SIZED("jcc"));
            JSON_WRITE_FIELD(writer, "message", MK_SIZED(diagnostic.message));

            // tags?: DiagnosticTag[];

            // relatedInformation?: DiagnosticRelatedInformation[];
            // data?: LSPAny;
          });
        }
      });
    });
  })
}

static void lsp_write_server_caps(struct lsp_ctx *ctx,
                                  const struct req_msg *msg) {
  const struct general_caps *caps =
      &ctx->init_params.client_capabilities.general_caps;

  bool found = false;
  for (size_t i = 0; i < caps->num_position_encodings; i++) {
    enum pos_encoding_kind kind = caps->position_encodings[i];

    if (kind == POS_ENCODING_KIND_UTF8) {
      found = true;
      break;
    }
  }

  if (!found) {
    TODO("support clients that dont allow utf-8");
  }

  struct sized_str pos_encoding_kind = MK_SIZED("utf-8");

  struct json_writer *writer = ctx->writer;

  LSP_WRITE_MESSAGE({
    // ResponseMessage

    JSON_WRITE_FIELD(writer, "id", msg->id);

    JSON_OBJECT(writer, "result", {
      // InitializeResult

      JSON_OBJECT(writer, "serverInfo", {
        // ServerInfo

        JSON_WRITE_FIELD(writer, "name", MK_SIZED("jcc-lsp"));
        JSON_WRITE_FIELD(writer, "version", MK_SIZED(JCC_VERSION));
      });

      JSON_OBJECT(writer, "capabilities", {
        // ServerCapabilities

        JSON_WRITE_FIELD(writer, "positionEncoding", pos_encoding_kind);
      });
    });
  })
}

static void lsp_handle_init(struct lsp_ctx *ctx) {
  struct req_msg msg = lsp_read_msg(ctx);

  invariant_assert(msg.method == REQ_MSG_METHOD_INITIALIZE,
                   "expected 'initialize' method as first message from client");

  ctx->init_params = msg.init_params;

  lsp_write_server_caps(ctx, &msg);

  msg = lsp_read_msg(ctx);

  invariant_assert(msg.method == REQ_MSG_METHOD_INITIALIZED,
                   "expected response to be an 'initialized' method");
}

int lsp_run(struct arena_allocator *arena, struct fcache *fcache,
            struct parsed_args args, struct compile_args compile_args,
            const struct target *target) {
  fprintf(stderr, "JCC LSP mode\n");
  fprintf(stderr, "This is intended for consumption by an editor\n\n");

  struct lsp_ctx ctx = {.arena = arena,
                        .read_buf = vector_create_in_arena(sizeof(char), arena),
                        .writer = json_writer_create(),
                        .compile_args = compile_args,
                        .args = args,
                        .fcache = fcache,
                        .target = target,
                        .shutdown_recv = false,
                        .in = stdin,
                        .out = stdout,
                        .log = stderr};

  lsp_handle_init(&ctx);

  while (true) {
    struct req_msg msg = lsp_read_msg(&ctx);

    switch (msg.method) {
    case REQ_MSG_METHOD_SHUTDOWN:
      ctx.shutdown_recv = true;

      info("LSP shutting down...");
      fprintf(stderr, "LSP shutting down...");
      break;
    case REQ_MSG_METHOD_EXIT:
      return ctx.shutdown_recv ? 0 : 1;
    default:
      lsp_handle_msg(&ctx, &msg);
    }
  }
}
