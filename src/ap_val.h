#ifndef AP_FLOAT_H
#define AP_FLOAT_H

#include "alloc.h"

#include <stdint.h>

/**************************  ap_int  **************************/

struct ap_int {
  size_t num_bits;
  uint64_t chunks[4];
};

struct ap_int ap_int_zero(size_t num_bits);
struct ap_int ap_int_one(size_t num_bits);

size_t ap_int_try_parse(struct arena_allocator *arena, size_t num_bits,
                      struct sized_str str, struct ap_int *ap_int);

void ap_int_set(struct ap_int *ap_int, signed long long value);

struct ap_int ap_int_add(struct ap_int lhs, struct ap_int rhs);
struct ap_int ap_int_sub(struct ap_int lhs, struct ap_int rhs);
struct ap_int ap_int_mul(struct ap_int lhs, struct ap_int rhs);
struct ap_int ap_int_div(struct ap_int lhs, struct ap_int rhs);
struct ap_int ap_int_mod(struct ap_int lhs, struct ap_int rhs);

struct ap_int ap_int_xor(struct ap_int lhs, struct ap_int rhs);
struct ap_int ap_int_or(struct ap_int lhs, struct ap_int rhs);
struct ap_int ap_int_and(struct ap_int lhs, struct ap_int rhs);
struct ap_int ap_int_rshift(struct ap_int lhs, struct ap_int rhs);
struct ap_int ap_int_lshift(struct ap_int lhs, struct ap_int rhs);

bool ap_int_eq(struct ap_int lhs, struct ap_int rhs);
bool ap_int_neq(struct ap_int lhs, struct ap_int rhs);
bool ap_int_gt(struct ap_int lhs, struct ap_int rhs);
bool ap_int_gteq(struct ap_int lhs, struct ap_int rhs);
bool ap_int_lt(struct ap_int lhs, struct ap_int rhs);
bool ap_int_lteq(struct ap_int lhs, struct ap_int rhs);

bool ap_int_nonzero(struct ap_int value);

struct ap_int ap_int_negate(struct ap_int value);
struct ap_int ap_int_not(struct ap_int value);

// TODO: get rid of this so everything uses ap_val
unsigned long long ap_int_as_ull(struct ap_int value);

/************************** ap_float **************************/

enum ap_float_ty {
  AP_FLOAT_TY_F16,
  AP_FLOAT_TY_F32,
  AP_FLOAT_TY_F64,
};

struct ap_float {
  enum ap_float_ty ty;

  union {
    float f16; // FIXME: currently all f16 ops are done via f32

    float f32;
    double f64;
  };
};

struct ap_float ap_float_zero(enum ap_float_ty ty);
struct ap_float ap_float_one(enum ap_float_ty ty);

size_t ap_float_try_parse(struct arena_allocator *arena, enum ap_float_ty ty,
                        struct sized_str str, struct ap_float *ap_float);

struct ap_float ap_float_add(struct ap_float lhs, struct ap_float rhs);
struct ap_float ap_float_sub(struct ap_float lhs, struct ap_float rhs);
struct ap_float ap_float_mul(struct ap_float lhs, struct ap_float rhs);
struct ap_float ap_float_div(struct ap_float lhs, struct ap_float rhs);

bool ap_float_eq(struct ap_float lhs, struct ap_float rhs);
bool ap_float_neq(struct ap_float lhs, struct ap_float rhs);
bool ap_float_gt(struct ap_float lhs, struct ap_float rhs);
bool ap_float_gteq(struct ap_float lhs, struct ap_float rhs);
bool ap_float_lt(struct ap_float lhs, struct ap_float rhs);
bool ap_float_lteq(struct ap_float lhs, struct ap_float rhs);

struct ap_float ap_float_negate(struct ap_float value);

bool ap_float_nonzero(struct ap_float value);

long double ap_float_as_ld(struct ap_float value);

/**************************  ap_val  **************************/

enum ap_val_ty {
  AP_VAL_TY_INVALID,

  AP_VAL_TY_INT,

  // TODO: should really unify uses of float vs flt
  AP_VAL_TY_FLOAT,
};

struct ap_val {
  enum ap_val_ty ty;

  union {
    struct ap_int ap_int;
    struct ap_float ap_float;
  };
};

#define MK_AP_VAL_INVALID()                                                    \
  (struct ap_val) { .ty = AP_VAL_TY_INVALID }
#define MK_AP_VAL_INT(val)                                                     \
  (struct ap_val) { .ty = AP_VAL_TY_INT, .ap_int = (val) }
#define MK_AP_VAL_FLT(val)                                                     \
  (struct ap_val) { .ty = AP_VAL_TY_FLOAT, .ap_float = (val) }

struct ap_val ap_val_from_ull(unsigned long long value, size_t num_bits);

bool ap_val_nonzero(struct ap_val value);
bool ap_val_iszero(struct ap_val value);

size_t ap_val_as_size_t(struct ap_val value);
double ap_val_as_double(struct ap_val value);

struct ap_val ap_val_add(struct ap_val lhs, struct ap_val rhs);
struct ap_val ap_val_sub(struct ap_val lhs, struct ap_val rhs);
struct ap_val ap_val_mul(struct ap_val lhs, struct ap_val rhs);
struct ap_val ap_val_div(struct ap_val lhs, struct ap_val rhs);
struct ap_val ap_val_mod(struct ap_val lhs, struct ap_val rhs);

struct ap_val ap_val_eq(struct ap_val lhs, struct ap_val rhs);
struct ap_val ap_val_neq(struct ap_val lhs, struct ap_val rhs);

struct ap_val ap_val_gt(struct ap_val lhs, struct ap_val rhs);
struct ap_val ap_val_gteq(struct ap_val lhs, struct ap_val rhs);
struct ap_val ap_val_lt(struct ap_val lhs, struct ap_val rhs);
struct ap_val ap_val_lteq(struct ap_val lhs, struct ap_val rhs);

struct ap_val ap_val_rshift(struct ap_val lhs, struct ap_val rhs);
struct ap_val ap_val_lshift(struct ap_val lhs, struct ap_val rhs);

struct ap_val ap_val_or(struct ap_val lhs, struct ap_val rhs);
struct ap_val ap_val_and(struct ap_val lhs, struct ap_val rhs);
struct ap_val ap_val_xor(struct ap_val lhs, struct ap_val rhs);

struct ap_val ap_val_negate(struct ap_val value);
struct ap_val ap_val_not(struct ap_val value);

struct ap_val ap_val_to_int(struct ap_val value, size_t num_bits);
struct ap_val ap_val_to_float(struct ap_val value, enum ap_float_ty ty);

size_t ap_val_try_parse_int(struct arena_allocator *arena, size_t num_bits,
                          struct sized_str str, struct ap_val *ap_val);
size_t ap_val_try_parse_float(struct arena_allocator *arena, enum ap_float_ty ty,
                            struct sized_str str, struct ap_val *ap_val);

void ap_val_fprintf(FILE *file, struct ap_val value);

#endif
