#include "promote.h"

#include "../alloc.h"
#include "../hashtbl.h"
#include "../ir/prettyprint.h"
#include "../log.h"
#include "../vector.h"
#include "opts.h"

struct addr_uses_data {
  struct ir_func *func;
  struct ir_op_use_map *use_map;

  struct vector *lcl_uses;
  struct ir_var_ty var_ty;
  struct ir_var_ty_info info;
  struct ir_op *addr;
  bool *candidate;
};

struct lcl_use {
  struct ir_op *op;
  size_t field_idx;
};

static void check_load(struct ir_op_use_map *use_map, struct vector *lcl_uses,
                       struct ir_op *op, size_t field_idx, bool *candidate) {
  struct ir_op_usage *usage = &use_map->op_use_datas[op->id];
  bool is_ret =
      usage->num_uses == 1 && usage->uses[0].consumer->ty == IR_OP_TY_RET;

  if (!is_ret && (op->var_ty.ty != IR_VAR_TY_TY_PRIMITIVE &&
                  op->var_ty.ty != IR_VAR_TY_TY_POINTER)) {
    *candidate = false;
  } else {
    struct lcl_use lcl_use = {.op = op, .field_idx = field_idx};
    vector_push_back(lcl_uses, &lcl_use);
  }
}

static void check_store(struct ir_op_use_map *use_map, struct vector *lcl_uses,
                        struct ir_op *op, struct ir_op *addr, size_t field_idx,
                        bool *candidate) {
  if (op->store.ty == IR_OP_STORE_TY_ADDR && op->store.addr != addr) {
    // don't consider stores where the address is the value _being stored_
    *candidate = false;
    return;
  }

  struct ir_op_usage *usage = &use_map->op_use_datas[op->id];
  bool is_ret =
      usage->num_uses == 1 && usage->uses[0].consumer->ty == IR_OP_TY_RET;

  if (!is_ret && (op->store.value->var_ty.ty != IR_VAR_TY_TY_PRIMITIVE &&
                  op->store.value->var_ty.ty != IR_VAR_TY_TY_POINTER)) {
    *candidate = false;
  } else {
    struct lcl_use lcl_use = {.op = op, .field_idx = field_idx};
    vector_push_back(lcl_uses, &lcl_use);
  }
}

static void check_addr_uses(struct addr_uses_data *data, struct ir_op *op,
                            size_t field_idx) {
  if (!*data->candidate) {
    return;
  }

  struct ir_op_usage uses = data->use_map->op_use_datas[op->id];

  for (size_t i = 0; i < uses.num_uses; i++) {
    struct ir_op_use use = uses.uses[i];

    struct ir_op *consumer = use.consumer;

    switch (consumer->ty) {
    case IR_OP_TY_LOAD:
      check_load(data->use_map, data->lcl_uses, consumer, field_idx,
                 data->candidate);
      break;
    case IR_OP_TY_STORE:
      check_store(data->use_map, data->lcl_uses, consumer, op, field_idx,
                  data->candidate);
      break;
    case IR_OP_TY_MEM_SET: {
      struct ir_op_mem_set mem_set = consumer->mem_set;

      // TODO: either here or in a seperate pass simplify addr nodes as this
      // won't find `&a[0]`
      struct ir_op *addr = mem_set.addr;
      if (addr->ty != IR_OP_TY_ADDR || addr->addr.ty != IR_OP_ADDR_TY_LCL) {
        *data->candidate = false;
        return;
      }

      // TODO: support partial zeroinit
      struct ir_lcl *lcl = addr->addr.lcl;
      if (field_idx || mem_set.value ||
          mem_set.length < var_ty_info(data->func->unit, &lcl->var_ty).size) {
        *data->candidate = false;
        return;
      }

      lcl->flags |= IR_LCL_FLAG_ZEROED;
      break;
    }

    case IR_OP_TY_ADDR_OFFSET: {
      struct ir_op_addr_offset addr_offset = consumer->addr_offset;
      if (addr_offset.index) {
        *data->candidate = false;
        return;
      }

      bool offset_found = false;
      size_t offset_field_idx = 0;
      size_t offset = addr_offset.offset;

      switch (data->var_ty.ty) {
      case IR_VAR_TY_TY_NONE:
        BUG("makes no sense");
      case IR_VAR_TY_TY_PRIMITIVE:
      case IR_VAR_TY_TY_FUNC:
      case IR_VAR_TY_TY_POINTER:
        offset_found = offset == 0;
        offset_field_idx = 0;
        break;
      case IR_VAR_TY_TY_ARRAY: {
        // TODO: also flatten arrays

        struct ir_var_ty el_ty = *data->var_ty.array.underlying;

        while (el_ty.ty == IR_VAR_TY_TY_ARRAY) {
          el_ty = *el_ty.array.underlying;
        }

        struct ir_var_ty_info el_info = var_ty_info(data->func->unit, &el_ty);

        offset_field_idx = offset / el_info.size;
        offset_found = offset % el_info.size == 0 &&
                       offset_field_idx < el_ty.array.num_elements;
        break;
      }
      case IR_VAR_TY_TY_STRUCT:
        for (size_t j = 0; j < data->info.num_fields; j++) {
          if (offset == data->info.offsets[j]) {
            offset_field_idx = j;
            offset_found = true;
            break;
          }
        }
        break;
      case IR_VAR_TY_TY_UNION:
        // don't promote unions
        *data->candidate = false;
        return;
      case IR_VAR_TY_TY_VARIADIC:
        // does this even make sense?
        *data->candidate = false;
        return;
      }

      if (offset_found) {
        check_addr_uses(data, consumer, field_idx + offset_field_idx);
        break;
      } else {
        *data->candidate = false;
        return;
      }
    }
    default:
      *data->candidate = false;
      return;
    }
  }
}

// FIXME: VERY temporary. should replace with proper phi allocation

struct ir_build_phi_build {
  struct ir_phi_entry *entry;
  struct ir_basicblock *pred;
};

struct lcl_store {
  size_t bb_id;
  size_t field_idx;
};

static void gen_var_phis(struct ir_func *irb, struct hashtbl *stores,
                         struct ir_op **basicblock_ops_for_var,
                         struct vector *preds, size_t field_idx,
                         struct ir_var_ty *var_ty) {
  size_t head = 0;
  while (vector_length(preds) - head) {
    struct ir_build_phi_build *build = vector_get(preds, head++);

    struct ir_basicblock *basicblock = build->pred;

    struct ir_op *op;

    struct lcl_store key = {.bb_id = basicblock->id, .field_idx = field_idx};
    struct ir_op **store = hashtbl_lookup(stores, &key);
    if (store) {
      op = *store;
    } else {
      op = basicblock_ops_for_var[basicblock->id];
    }

    if (op) {
      *build->entry =
          (struct ir_phi_entry){.basicblock = basicblock, .value = op};

      basicblock_ops_for_var[basicblock->id] = op;
      continue;
    }

    DEBUG_ASSERT(basicblock->pred, "can't insert a phi in first bb");

    // var is not in this bb, so gen phi
    struct ir_op *phi = insert_phi(irb, basicblock, *var_ty);

    phi->phi = (struct ir_op_phi){
        .num_values = basicblock->num_preds,
        .values = arena_alloc(irb->arena, sizeof(*phi->phi.values) *
                                              basicblock->num_preds)};
    *build->entry =
        (struct ir_phi_entry){.basicblock = basicblock, .value = phi};

    basicblock_ops_for_var[basicblock->id] = phi;

    hashtbl_insert(stores, &key, &phi);

    for (size_t i = 0; i < basicblock->num_preds; i++) {
      struct ir_basicblock *pred = basicblock->preds[i];

      struct ir_build_phi_build pred_build = {.entry = &phi->phi.values[i],
                                              .pred = pred};

      vector_push_back(preds, &pred_build);
    }
  }
}

static void find_phi_exprs(struct ir_func *irb, struct hashtbl *stores,
                           struct ir_op *phi, size_t field_idx) {
  DEBUG_ASSERT(phi->ty == IR_OP_TY_PHI, "non-phi in `find_phi_exprs`");

  // walk predecessor basic blocks (splitting into seperate walks each time we
  // have multiple predecessors) until we
  // * A) find a write
  // * B) re-reach current bb
  // * or C) reach end (first bb)

  struct ir_basicblock *basicblock = phi->stmt->basicblock;

  struct ir_op **basicblock_ops_for_var =
      arena_alloc(irb->arena, sizeof(struct ir_op *) * irb->basicblock_count);
  memset(basicblock_ops_for_var, 0,
         sizeof(struct ir_op *) * irb->basicblock_count);
  basicblock_ops_for_var[basicblock->id] = phi;

  phi->phi = (struct ir_op_phi){
      .num_values = basicblock->num_preds,
      .values = arena_alloc(irb->arena,
                            sizeof(*phi->phi.values) * basicblock->num_preds)};

  struct vector *phi_builds = vector_create(sizeof(struct ir_build_phi_build));

  for (size_t i = 0; i < basicblock->num_preds; i++) {
    struct ir_build_phi_build build = {.entry = &phi->phi.values[i],
                                       .pred = basicblock->preds[i]};

    vector_push_back(phi_builds, &build);
  }

  gen_var_phis(irb, stores, basicblock_ops_for_var, phi_builds, field_idx,
               &phi->var_ty);

  vector_free(&phi_builds);
}

static void opts_do_promote(struct ir_func *func, struct vector *lcl_uses,
                            struct ir_lcl *lcl) {
  size_t num_uses = vector_length(lcl_uses);

  // used to check if variable has _ever_ been written - else we know it is
  // zeroed
  struct hashtbl *any_stores =
      hashtbl_create(sizeof(size_t), sizeof(struct ir_op *), NULL, NULL);
  // holds the latest `mov` for each (field, basicblock) pair
  struct hashtbl *stores = hashtbl_create(sizeof(struct lcl_store),
                                          sizeof(struct ir_op *), NULL, NULL);

  // TODO: use this here (and in build) to pick better phi locations
  // struct ir_dominance_frontier domf = ir_compute_dominance_frontier(func);

  for (size_t i = 0; i < num_uses; i++) {
    struct lcl_use *use = vector_get(lcl_uses, i);

    struct ir_op *op = use->op;
    if (op->ty != IR_OP_TY_STORE) {
      continue;
    }

    struct ir_op *value = op->store.value;

    op = replace_ir_op(func, op, IR_OP_TY_MOV, value->var_ty);
    op->mov = (struct ir_op_mov){.value = value};

    struct lcl_store key = {.bb_id = op->stmt->basicblock->id,
                            .field_idx = use->field_idx};

    struct ir_op **prev = hashtbl_lookup(stores, &key);

    // relies on sequential ids
    if (!prev || (*prev)->id < op->id) {
      hashtbl_insert(stores, &key, &op);
      hashtbl_insert(any_stores, &use->field_idx, &op);
    }
  }

  for (size_t i = 0; i < num_uses; i++) {
    struct lcl_use *use = vector_get(lcl_uses, i);

    struct ir_op *op = use->op;
    struct ir_gather_value *gather_values;
    if (op->ty == IR_OP_TY_LOAD) {
      size_t first_field, num_fields;
      struct ir_var_ty *fields;
      if (var_ty_is_aggregate(&op->var_ty)) {
        first_field = use->field_idx;
        num_fields = op->var_ty.aggregate.num_fields;
        gather_values = arena_alloc(
            func->arena, num_fields * sizeof(struct ir_gather_value));
        fields = op->var_ty.aggregate.fields;
      } else {
        first_field = use->field_idx;
        num_fields = 1;
        gather_values = NULL;
        fields = &op->var_ty;
      }

      size_t head = 0;
      for (size_t j = first_field; j < first_field + num_fields; j++) {
        struct ir_basicblock *basicblock = op->stmt->basicblock;

        struct lcl_store key = {.bb_id = basicblock->id, .field_idx = j};

        struct ir_var_ty field_ty = fields[head];

        struct ir_op **store = hashtbl_lookup(stores, &key);
        if (store) {
          // same bb, no phi needed
          struct ir_op *mov;
          if (gather_values) {
            mov = insert_before_ir_op(func, op, IR_OP_TY_MOV, field_ty);
            gather_values[head++] =
                (struct ir_gather_value){.value = mov, .field_idx = j};
          } else {
            mov = replace_ir_op(func, op, IR_OP_TY_MOV, op->var_ty);
          }

          mov->mov = (struct ir_op_mov){.value = *store};
          mov->flags |= IR_OP_FLAG_PROMOTED;

          continue;
        }

        struct ir_op **any_store = hashtbl_lookup(any_stores, &key.field_idx);
        if (!any_store) {
          DEBUG_ASSERT(lcl->flags & IR_LCL_FLAG_ZEROED,
                       "expected zeroed because no stores");

          struct ir_op *zero;
          if (gather_values) {
            zero = insert_before_ir_op(func, op, IR_OP_TY_CNST, field_ty);

            gather_values[head++] =
                (struct ir_gather_value){.value = zero, .field_idx = j};
          } else {
            zero = op;
          }

          mk_zero_constant(func->unit, zero, &field_ty);
          zero->flags |= IR_OP_FLAG_PROMOTED;

          continue;
        }

        struct ir_op *phi = insert_phi(func, basicblock, field_ty);
        phi->phi = (struct ir_op_phi){.num_values = 0};

        struct ir_op *mov;
        if (gather_values) {
          mov = insert_before_ir_op(func, op, IR_OP_TY_MOV, field_ty);
          gather_values[head++] =
              (struct ir_gather_value){.value = mov, .field_idx = j};
        } else {
          mov = replace_ir_op(func, op, IR_OP_TY_MOV, op->var_ty);
        }

        mov->mov = (struct ir_op_mov){.value = phi};
        mov->flags |= IR_OP_FLAG_PROMOTED;

        find_phi_exprs(func, stores, phi, j);
      }

      if (gather_values) {
        op = replace_ir_op(func, op, IR_OP_TY_GATHER, op->var_ty);
        op->flags |= IR_OP_FLAG_PROMOTED;
        op->gather = (struct ir_op_gather){.num_values = num_fields,
                                           .values = gather_values};
      }
    }
  }

  // can't reuse map from parent because we have added ops
  struct ir_op_use_map use_map = build_op_uses_map(func);

  struct vector *depends = vector_create(sizeof(struct ir_op *));
  struct ir_lcl_usage *usage = &use_map.lcl_use_datas[lcl->id];

  for (size_t j = 0; j < usage->num_consumers; j++) {
    struct ir_op *consumer = usage->consumers[j];

    vector_push_back(depends, &consumer);

    while (vector_length(depends)) {
      struct ir_op *detach = *(struct ir_op **)vector_pop(depends);

      if (detach->id == DETACHED_OP) {
        continue;
      }

      if (detach->flags & IR_OP_FLAG_PROMOTED) {
        continue;
      }

      struct ir_op_usage *dep_usage = &use_map.op_use_datas[detach->id];

      for (size_t k = 0; k < dep_usage->num_uses; k++) {
        struct ir_op *dep_use = dep_usage->uses[k].consumer;

        vector_push_back(depends, &dep_use);
      }

      DEBUG_ASSERT(!op_is_branch(detach->ty), "should not be detaching branch");
      detach_ir_op(func, detach);
    }
  }

  vector_free(&depends);
  hashtbl_free(&any_stores);
  hashtbl_free(&stores);
}

static void opts_promote_func(struct ir_func *func) {
  // rebuilds op ids, important for later
  struct ir_op_use_map use_map = build_op_uses_map(func);

  // note: this requires contigous lcl ids
  bool *candidates =
      arena_alloc(func->arena, sizeof(*candidates) * func->lcl_count);

  if (!candidates) {
    return;
  }

  memset(candidates, true, sizeof(*candidates) * func->lcl_count);

  struct vector **lcl_uses =
      arena_alloc(func->arena, sizeof(struct vector *) * func->lcl_count);

  for (size_t i = 0; i < func->lcl_count; i++) {
    lcl_uses[i] = vector_create(sizeof(struct lcl_use));
  }

  struct ir_basicblock *basicblock = func->first;

  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;

    while (stmt) {
      struct ir_op *op = stmt->first;

      while (op) {
        switch (op->ty) {
        case IR_OP_TY_LOAD:
          if (op->load.ty == IR_OP_LOAD_TY_LCL) {
            struct ir_lcl *lcl = op->load.lcl;
            check_load(&use_map, lcl_uses[lcl->id], op, 0,
                       &candidates[lcl->id]);
          }
          break;
        case IR_OP_TY_STORE:
          if (op->store.ty == IR_OP_STORE_TY_LCL) {
            struct ir_lcl *lcl = op->store.lcl;
            check_store(&use_map, lcl_uses[lcl->id], op, NULL, 0,
                        &candidates[lcl->id]);

            if (op->var_ty.ty != IR_VAR_TY_TY_PRIMITIVE &&
                op->var_ty.ty != IR_VAR_TY_TY_POINTER) {
              candidates[lcl->id] = false;
            } else {
              struct lcl_use lcl_use = {.op = op, .field_idx = 0};
              vector_push_back(lcl_uses[lcl->id], &lcl_use);
            }
          }
          break;

        // don't promote bitfields because `lower` can't currently handle it
        case IR_OP_TY_LOAD_BITFIELD:
          if (op->load_bitfield.ty == IR_OP_LOAD_TY_LCL) {
            struct ir_lcl *lcl = op->load_bitfield.lcl;
            candidates[lcl->id] = false;
          }
          break;
        case IR_OP_TY_STORE_BITFIELD:
          if (op->store_bitfield.ty == IR_OP_STORE_TY_LCL) {
            struct ir_lcl *lcl = op->load_bitfield.lcl;
            candidates[lcl->id] = false;
          }
          break;
        case IR_OP_TY_ADDR:
          // progressively look forward through uses until we hit a load/store

          if (op->addr.ty == IR_OP_ADDR_TY_LCL) {
            struct ir_lcl *lcl = op->addr.lcl;

            struct ir_var_ty_info info = var_ty_info(func->unit, &lcl->var_ty);

            struct addr_uses_data data = {.func = func,
                                          .lcl_uses = lcl_uses[lcl->id],
                                          .use_map = &use_map,
                                          .addr = op,
                                          .var_ty = lcl->var_ty,
                                          .info = info,
                                          .candidate = &candidates[lcl->id]};

            check_addr_uses(&data, op, 0);
          }
          break;
        default:
          break;
        }

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }

  struct ir_lcl *lcl = func->first_lcl;

  // we have to remove locals _after_ to prevent invalidating `lcl_uses`
  struct vector *lcls_to_remove = vector_create(sizeof(struct ir_lcl *));

  while (lcl) {
    DEBUG_ASSERT(!lcl->succ || lcl->succ->id == lcl->id + 1,
                 "non sequential locals");

    // don't promote params because they don't have moves (they are in a "magic
    // local") and so promoting will fail
    if (candidates[lcl->id] && !(lcl->flags & IR_LCL_FLAG_PARAM)) {
      debug("lcl %zu promotion candidate", lcl->id);
      opts_do_promote(func, lcl_uses[lcl->id], lcl);
      vector_push_back(lcls_to_remove, &lcl);
    }

    lcl = lcl->succ;
  }

  for (size_t i = 0; i < func->lcl_count; i++) {
    vector_free(&lcl_uses[i]);
  }

  size_t num = vector_length(lcls_to_remove);
  for (size_t i = 0; i < num; i++) {
    detach_local(func, *(struct ir_lcl **)vector_get(lcls_to_remove, i));
  }

  vector_free(&lcls_to_remove);
}

void opts_promote(struct ir_unit *unit) {
  struct ir_glb *glb = unit->first_global;

  while (glb) {
    if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
      glb = glb->succ;
      continue;
    }

    switch (glb->ty) {
    case IR_GLB_TY_DATA:
      break;
    case IR_GLB_TY_FUNC:
      opts_promote_func(glb->func);
      break;
    }

    glb = glb->succ;
  }
}
