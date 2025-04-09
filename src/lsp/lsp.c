#include "lsp.h"

#include "../alloc.h"
#include "../compiler.h"
#include "../json.h"
#include "../log.h"
#include "../typechk.h"
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

  // hmm, ideally we would use fcache for all the file caching stuff but it
  // doesn't currently have the capabilities for modifying docs
  struct fcache *fcache;
  // sized_str (uri) : lsp_doc
  struct hashtbl *docs;

  struct parsed_args args;
  struct compile_args compile_args;
  const struct target *target;

  struct init_params init_params;

  bool shutdown_recv;

  FILE *in;
  FILE *out;
  FILE *log;
};

struct lsp_doc {
  struct text_doc doc;
  struct compiler *compiler;
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

    size_t header_len = (size_t)(brk - buffer);

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

static void lsp_write_diagnostics(struct lsp_ctx *ctx, struct lsp_doc *doc,
                                  struct compiler_diagnostics *diagnostics);

static void lsp_parse_doc(struct lsp_ctx *ctx, struct lsp_doc *doc) {
  struct program program = {
      // FIXME: not sized
      .text = arena_alloc_strndup(ctx->arena, doc->doc.text.str,
                                  doc->doc.text.len)};

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

  if (doc->compiler) {
    free_compiler(&doc->compiler);
  }

  if (compiler_create(&comp_args, &doc->compiler) !=
      COMPILER_CREATE_RESULT_SUCCESS) {
    BUG("handle compiler create failure");
  }

  compile(doc->compiler);

  struct compiler_diagnostics *diagnostics =
      compiler_get_diagnostics(doc->compiler);

  lsp_write_diagnostics(ctx, doc, diagnostics);
}

static void lsp_parse_opened_doc(struct lsp_ctx *ctx,
                                 const struct didopen_textdoc_params *params) {
  struct lsp_doc doc = {.doc = params->text_doc};
  lsp_parse_doc(ctx, &doc);

  hashtbl_insert(ctx->docs, &params->text_doc.uri, &doc);
}

static struct lsp_doc *lsp_get_doc(struct lsp_ctx *ctx, ustr_t uri) {
  struct lsp_doc *doc = hashtbl_lookup(ctx->docs, &uri);

  invariant_assert(doc, "doc did not exist (should have been opened by "
                        "previous textDocument/didOpen call)");

  return doc;
}

static void
lsp_parse_changed_doc(struct lsp_ctx *ctx,
                      const struct didchange_textdoc_params *params) {
  struct lsp_doc *doc = lsp_get_doc(ctx, params->text_doc.uri);

  invariant_assert(doc, "doc did not exist (should have been opened by "
                        "previous textDocument/didOpen call)");

  if (doc->doc.version > params->text_doc.version) {
    return;
  }

  doc->doc.version = params->text_doc.version;

  // now we have to apply the changes
  for (size_t i = 0; i < params->num_changes; i++) {
    const struct text_doc_change_ev *ev = &params->changes[i];

    switch (ev->ty) {
    case TEXT_DOC_CHANGE_EV_TY_INCREMENTAL:
      TODO("incremental changes");
    case TEXT_DOC_CHANGE_EV_TY_FULL:
      doc->doc.text = ev->text;
      break;
    }
  }

  lsp_parse_doc(ctx, doc);
}

static void lsp_close_doc(struct lsp_ctx *ctx,
                          const struct didclose_textdoc_params *params) {
  hashtbl_remove(ctx->docs, &params->text_doc.uri);
}

#define LSP_WRITE_MESSAGE(block)                                               \
  json_writer_write_begin_obj(ctx->writer);                                    \
  JSON_WRITE_FIELD(ctx->writer, "jsonrpc", MK_USTR("2.0"));                   \
  {block};                                                                     \
  json_writer_write_end_obj(ctx->writer);                                      \
  lsp_write_buf(ctx);

static struct req_msg lsp_read_msg(struct lsp_ctx *ctx) {
  struct lsp_headers headers = lsp_read_headers(ctx);

  vector_clear(ctx->read_buf);
  vector_extend(ctx->read_buf, NULL, headers.content_length);

  fread(vector_head(ctx->read_buf), 1, headers.content_length, ctx->in);

  ustr_t obj = {.len = vector_length(ctx->read_buf),
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
  ustr_t res = json_writer_get_buf(ctx->writer);

  fprintf(ctx->log, "%.*s\n", (int)res.len, res.str);

  fprintf(ctx->out, "Content-Length: %zu\r\n", res.len);
  fprintf(ctx->out, "\r\n");

  fprintf(ctx->out, "%.*s", (int)res.len, res.str);
  fflush(ctx->out);

  // safe to clear buffer now
  json_writer_clear(ctx->writer);
}

// Writes a `struct text_pos` into a `Position` LSP type
static void lsp_json_write_pos(struct lsp_ctx *ctx, struct text_pos pos) {
  struct json_writer *writer = ctx->writer;

  JSON_OBJECT(writer, NULL, {
    // FIXME: a few things to check here
    //   * are our lines starting at the same line as LSP expects (zero vs one
    //   indexing)
    //   * characters may not line up when UTF text is present as we do byte
    //   columns
    JSON_WRITE_FIELD(writer, "line", (int)pos.line);
    JSON_WRITE_FIELD(writer, "character", (int)pos.col);
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

static void lsp_write_diagnostics(struct lsp_ctx *ctx, struct lsp_doc *doc_ctx,
                                  struct compiler_diagnostics *diagnostics) {
  struct json_writer *writer = ctx->writer;

  LSP_WRITE_MESSAGE({
    JSON_WRITE_FIELD(writer, "method",
                     MK_USTR("textDocument/publishDiagnostics"));

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

            JSON_WRITE_FIELD(writer, "source", MK_USTR("jcc"));
            JSON_WRITE_FIELD(writer, "message", MK_USTR(diagnostic.message));

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

  ustr_t pos_encoding_kind = MK_USTR("utf-8");

  struct json_writer *writer = ctx->writer;

  LSP_WRITE_MESSAGE({
    // ResponseMessage

    JSON_WRITE_FIELD(writer, "id", msg->id);

    JSON_OBJECT(writer, "result", {
      // InitializeResult

      JSON_OBJECT(writer, "serverInfo", {
        // ServerInfo

        JSON_WRITE_FIELD(writer, "name", MK_USTR("jcc-lsp"));
        JSON_WRITE_FIELD(writer, "version", MK_USTR(JCC_VERSION));
      });

      JSON_OBJECT(writer, "capabilities", {
        // ServerCapabilities

        JSON_WRITE_FIELD(writer, "positionEncoding", pos_encoding_kind);

        JSON_OBJECT(writer, "textDocumentSync", {
          // TextDocumentSyncOptions

          // in C11 we need to force to bool so _Generic picks the right thing
          // (sigh)
          JSON_WRITE_FIELD(writer, "openClose", (bool)true);
          JSON_WRITE_FIELD(writer, "change", TEXT_DOC_CHANGE_EV_TY_FULL);
        });

        // we don't support dynamic registration or progress, so just write bool
        // JSON_OBJECT(writer, "definitionProvider", {
        //     // DefinitionOptions
        // });

        // JSON_OBJECT(writer, "typeDefinitionProvider", {
        //     // TypeDefinitionRegistrationOptions

        // });

        JSON_WRITE_FIELD(writer, "definitionProvider", (bool)true);
        JSON_WRITE_FIELD(writer, "typeDefinitionProvider", (bool)true);
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

struct build_def_data {
  struct vector *locs;
  struct hashtbl *defs;
};

static void build_def_cb(const struct td_node *node, void *metadata) {
  struct build_def_data *data = metadata;

  // TODO: support decls
  switch (node->ty) {
  case TD_NODE_TY_EXPR: {
    const struct td_expr *expr = node->expr;
    if (expr->ty == TD_EXPR_TY_VAR) {
      fprintf(stderr, "var l %zu %.*s\n", expr->span.start.line, (int)expr->var.identifier.len, expr->var.identifier.str);
      vector_push_back(data->locs, node);
    }
    break;
  }
  case TD_NODE_TY_VAR_DECL: {
    const struct td_var_declaration *var_decl = node->var_decl;
    struct text_span span = var_decl->var.span;
    fprintf(stderr, "add decl %.*s\n", (int)var_decl->var.identifier.len, var_decl->var.identifier.str);
    hashtbl_insert(data->defs, &var_decl->var, &span);
    break;
  }
  default:
    break;
  }
}

static int compare_text_spans(const void *l, const void *r) {
  const struct td_node *ls = l;
  const struct td_node *rs = r;

  const struct text_pos *l_start = &ls->span.start;
  const struct text_pos *r_start = &rs->span.start;

  if (l_start->idx > r_start->idx) {
    return 1;
  } else if (l_start->idx == r_start->idx) {
    return 0;
  } else {
    return -1;
  }
}

static void lsp_goto_def(struct lsp_ctx *ctx, lsp_integer id,
                         const struct definition_textdoc_params *params) {
  struct lsp_doc *doc = lsp_get_doc(ctx, params->text_doc.uri);

  struct compiler *compiler = doc->compiler;

  struct typechk *tchk;
  struct typechk_result result;
  compiler_get_tchk(compiler, &tchk, &result);

  if (!tchk) {
    LSP_WRITE_MESSAGE({
      JSON_WRITE_FIELD(ctx->writer, "id", id);
      JSON_WRITE_FIELD(ctx->writer, "result", JSON_NULL);
    });

    return;
  }

  struct build_def_data data = {
      .locs = vector_create_in_arena(sizeof(struct td_node), ctx->arena),
      .defs = hashtbl_create_in_arena(ctx->arena, sizeof(struct td_var),
                                      sizeof(struct text_span), hash_td_var,
                                      eq_td_var)};
  td_walk(tchk, &result.translation_unit, build_def_cb, &data);

  size_t num_locs = vector_length(data.locs);

  qsort(vector_head(data.locs), num_locs, sizeof(struct td_node),
        compare_text_spans);

  const struct td_var *var = NULL;

  // FIXME: linear search
  for (size_t i = 0; i < num_locs; i++) {
    struct td_node *node = vector_get(data.locs, i);

    struct text_pos start = node->span.start;
    struct text_pos end = node->span.end;

    if ((end.line > params->pos.line || (end.line == params->pos.line &&
        end.col > params->pos.col)) && (start.line < params->pos.line || (start.line == params->pos.line && start.col <= params->pos.col))) {
      var = &node->var_decl->var;
      break;
    }
  }

  struct text_span *def = NULL;
  if (var) {
    fprintf(stderr, "lookup def %.*s\n", (int)var->identifier.len, var->identifier.str);
    def = hashtbl_lookup(data.defs, var);
  }

  if (!def) {
    LSP_WRITE_MESSAGE({
      JSON_WRITE_FIELD(ctx->writer, "id", id);
      JSON_WRITE_FIELD(ctx->writer, "result", JSON_NULL);
    });
    return;
  }

  LSP_WRITE_MESSAGE({
    JSON_WRITE_FIELD(ctx->writer, "id", id);
    JSON_OBJECT(ctx->writer, "result", {
      if (def->start.file) {
        char *uri = arena_alloc_snprintf(ctx->arena, "file://%s", def->start.file);
        JSON_WRITE_FIELD(ctx->writer, "uri", MK_USTR(uri));
      } else {
        JSON_WRITE_FIELD(ctx->writer, "uri", params->text_doc.uri);
      }

      JSON_WRITE_FIELD_NAME(ctx->writer, "range");
      lsp_json_write_span(ctx, *def);
    });
  });
}

START_NO_UNUSED_ARGS

static void
lsp_goto_type_def(struct lsp_ctx *ctx,
                  const struct type_definition_textdoc_params *params) {
  TODO("goto def");
}

END_NO_UNUSED_ARGS

static void lsp_handle_msg(struct lsp_ctx *ctx, const struct req_msg *msg) {
  switch (msg->method) {
  case REQ_MSG_METHOD_INITIALIZE:
    BUG("duplicate initialize message");
  case REQ_MSG_METHOD_SHUTDOWN:
    ctx->shutdown_recv = true;

    LSP_WRITE_MESSAGE({
      JSON_WRITE_FIELD(ctx->writer, "id", msg->id);
      JSON_WRITE_FIELD(ctx->writer, "result", JSON_NULL);

      // apparently we are meant to set code & message?
    });

    info("LSP shutting down...");
    fprintf(stderr, "LSP shutting down...");
    break;

  case REQ_MSG_METHOD_EXIT:
    BUG("exit method should have been handled");
  case REQ_MSG_METHOD_INITIALIZED:
    break;
  case REQ_MSG_METHOD_TEXTDOCUMENT_DIDOPEN:
    lsp_parse_opened_doc(ctx, &msg->didopen_textdoc_params);
    break;
  case REQ_MSG_METHOD_TEXTDOCUMENT_DIDCHANGE:
    lsp_parse_changed_doc(ctx, &msg->didchange_textdoc_params);
    break;
  case REQ_MSG_METHOD_TEXTDOCUMENT_DIDCLOSE:
    lsp_close_doc(ctx, &msg->didclose_textdoc_params);
    break;
  case REQ_MSG_METHOD_TEXTDOCUMENT_DEFINITION:
    lsp_goto_def(ctx, msg->id, &msg->definition_textdoc_params);
    break;
  case REQ_MSG_METHOD_TEXTDOCUMENT_TYPEDEFINITION:
    lsp_goto_type_def(ctx, &msg->type_definition_textdoc_params);
    break;
  }
}
int lsp_run(struct arena_allocator *arena, struct fcache *fcache,
            struct parsed_args args, struct compile_args compile_args,
            const struct target *target) {
  fprintf(stderr, "JCC LSP mode\n");
  fprintf(stderr, "This is intended for consumption by an editor\n\n");

  struct lsp_ctx ctx = {.arena = arena,
                        .read_buf = vector_create_in_arena(sizeof(char), arena),
                        .docs = hashtbl_create_sized_str_keyed_in_arena(
                            arena, sizeof(struct lsp_doc)),
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
    case REQ_MSG_METHOD_EXIT:
      return ctx.shutdown_recv ? 0 : 1;
    default:
      lsp_handle_msg(&ctx, &msg);
    }
  }
}
