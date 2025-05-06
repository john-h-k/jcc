#include "rw.h"

#include "../util.h"
#include "../vector.h"
#include "ir.h"

// FIXME: wip

#if 0

/******************************* ser *******************************/
/* This is the authoritative spec for how serialized               */
/* bytecode looks                                                  */
/*******************************************************************/

// * Header - metadata (TODO)
// * List of fixed-size global records
// *
// * String table

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

// reference
typedef u64 str_t;

// 0 = none
// 1 = first element
typedef u64 id_list_t;

typedef u64 ser_id_t;

struct ir_ser_ctx {
  FILE *file;
  struct ir_unit *unit;

  struct vector *ids;
  struct vector *strings;
};

struct ir_de_ctx {
  FILE *file;
  struct ir_unit *unit;

  struct vector *ids;
  struct vector *strings;
};

struct ir_ser_var {
  u8 ty;
  u8 flags;
  u8 _pad[6];
};

static_assert(sizeof(struct ir_ser_var) == 8);

struct ir_ser_func {
  u8 flags;
  u8 _pad[7];
};

static_assert(sizeof(struct ir_ser_func) == 8);

struct ir_ser_glb {
  ser_id_t id;

  u8 ty;
  u8 def_ty;
  u8 linkage;
  u8 flags;

  str_t name;
  ref_t var_ty;
};

static_assert(sizeof(struct ir_ser_glb) == 32);

static ref_t ir_ser_string(struct ir_ser_ctx *ctx, const char *str) {}

static ref_t ir_ser_var(struct ir_ser_ctx *ctx, struct ir_var *var) {}

static ref_t ir_ser_op(struct ir_ser_ctx *ctx, struct ir_op *op) {}

static void ir_ser_stmt(struct ir_ser_ctx *ctx, struct ir_stmt *stmt) {
  struct ir_op *op = stmt->first;
  while (op) {
    ir_ser_op(ctx, op);

    op = op->succ;
  }
}

struct ir_ser_basicblock {
  u8 ty;
  union {
    ref_t merge;

    struct {
      ref_t true_target;
      ref_t false_target;
    };

    struct ir_basicblock_switch switch_case;
  };

}

static void
ir_ser_basicblock(struct ir_ser_ctx *ctx, struct ir_basicblock *basicblock) {
  struct ir_stmt *stmt = basicblock->first;
  while (stmt) {
    ir_ser_stmt(ctx, stmt);

    stmt = stmt->succ;
  }
}

static void ir_ser_func(struct ir_ser_ctx *ctx, struct ir_func *func) {
  struct ir_ser_func ser = {.flags = (u8)func->flags};

  fwrite(&ser, sizeof(ser), 1, ctx->file);

  struct ir_basicblock *basicblock = func->first;
  while (basicblock) {
    ir_ser_basicblock(ctx, basicblock);

    basicblock = basicblock->succ;
  }
}

static void ir_ser_glb(struct ir_ser_ctx *ctx, struct ir_glb *glb) {
  struct ir_ser_glb ser = {
      .id = glb->id,
      .ty = (u8)glb->ty,
      .def_ty = (u8)glb->def_ty,
      .linkage = (u8)glb->linkage,
      .flags = (u8)glb->flags,
      .name = ir_ser_string(ctx, glb->name),
  };

  fwrite(&ser, sizeof(ser), 1, ctx->file);

  if (glb->def_ty == IR_GLB_DEF_TY_DEFINED) {
    switch (glb->ty) {
    case IR_GLB_TY_DATA:
      ir_ser_var(ctx, glb->var);
      break;
    case IR_GLB_TY_FUNC:
      ir_ser_func(ctx, glb->func);
      break;
    }
  }
}

void ir_unit_ser(FILE *file, struct ir_unit *unit) {
  struct ir_ser_ctx ctx = {.file = file,
                           .unit = unit,
                           .vars = ir_ser_tbl_create(sizeof(struct ir_ser_var)),
                           .funcs =
                               ir_ser_tbl_create(sizeof(struct ir_ser_func)),
                           .strings = vector_create(sizeof(char))};

  struct ir_glb *glb = unit->first_global;

  while (glb) {
    ir_unit_ser_glb(&ctx, glb);

    glb = glb->succ;
  }
}

#endif
