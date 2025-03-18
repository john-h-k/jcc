
#include "ap_val.h"
#include "alloc.h"
#include <assert.h>
#include <ctype.h>
#include <limits.h>

/**************************  ap_int  **************************/

#define NUM_CHUNKS(v)                                                          \
  (DEBUG_ASSERT((((v).num_bits + 63) / 64) <= 4, "num too big"),               \
   (((v).num_bits + 63) / 64))

struct ap_int ap_int_zero(size_t num_bits) {
  return (struct ap_int){.num_bits = num_bits};
}

#define CHECK64(v) DEBUG_ASSERT((v).num_bits <= 64, ">64 bit ap_int");
#define CHECK_SAME(lhs, rhs)                                                   \
  DEBUG_ASSERT((lhs).num_bits == (rhs).num_bits, "ap_int bit count mismatch");
#define CHECK_BINN(lhs, rhs)                                                            \
  CHECK64(lhs);                                                                \
  CHECK64(rhs);                                                                \
  CHECK_SAME(lhs, rhs);
#define CHECK_BIN() CHECK_BINN(lhs, rhs)

struct ap_int ap_int_add(struct ap_int lhs, struct ap_int rhs) {
  CHECK_BIN();

  struct ap_int res = {.num_bits = lhs.num_bits,
                       .chunks = {[0] = lhs.chunks[0] + rhs.chunks[0]}};

  return res;
}

struct ap_int ap_int_sub(struct ap_int lhs, struct ap_int rhs) {
  CHECK_BIN();

  struct ap_int res = {.num_bits = lhs.num_bits,
                       .chunks = {[0] = lhs.chunks[0] - rhs.chunks[0]}};

  return res;
}

// #ifdef HAS_INT128

// static void wmul_64(uint64_t a, uint64_t b, uint64_t *lo, uint64_t *hi) {
//   uint128_t prod = (uint128_t)a * b;
//   *lo = (uint64_t)prod;
//   *hi = (uint64_t)(prod >> 64);
// }

// #else

// static void wmul_64(uint64_t a, uint64_t b, uint64_t *lo, uint64_t *hi) {
//   uint64_t a_lo = a & 0xFFFFFFFFULL, a_hi = a >> 32;
//   uint64_t b_lo = b & 0xFFFFFFFFULL, b_hi = b >> 32;
//   uint64_t p0 = a_lo * b_lo;
//   uint64_t p1 = a_lo * b_hi;
//   uint64_t p2 = a_hi * b_lo;
//   uint64_t p3 = a_hi * b_hi;
//   uint64_t mid = (p0 >> 32) + (p1 & 0xFFFFFFFFULL) + (p2 & 0xFFFFFFFFULL);
//   uint64_t mid_hi = mid >> 32;
//   uint64_t mid_lo = mid & 0xFFFFFFFFULL;
//   *lo = (p0 & 0xFFFFFFFFULL) | (mid_lo << 32);
//   *hi = p3 + (p1 >> 32) + (p2 >> 32) + mid_hi;
// }
// #endif

struct ap_int ap_int_mul(struct ap_int lhs, struct ap_int rhs) {
  CHECK_BIN();

  struct ap_int res = {.num_bits = lhs.num_bits,
                       .chunks = {[0] = lhs.chunks[0] * rhs.chunks[0]}};

  return res;
}

struct ap_int ap_int_mod(struct ap_int lhs, struct ap_int rhs) {
  // // TODO: here and div, handle bad values

  // struct ap_int quotient = ap_int_div(lhs, rhs);
  // struct ap_int product  = ap_int_mul(quotient, rhs);
  // return ap_int_sub(lhs, product);

  CHECK_BIN();

  struct ap_int res = {.num_bits = lhs.num_bits,
                       .chunks = {[0] = lhs.chunks[0] % rhs.chunks[0]}};

  return res;
}

struct ap_int ap_int_div(struct ap_int lhs, struct ap_int rhs) {
  CHECK_BIN();

  // FIXME: check for div zero

  struct ap_int res = {.num_bits = lhs.num_bits,
                       .chunks = {[0] = lhs.chunks[0] / rhs.chunks[0]}};

  return res;
}

static void ap_int_mul_into_i32(struct ap_int *a, int m) {
  CHECK64(*a);

  a->chunks[0] *= m;
}

static void ap_int_add_into_i32(struct ap_int *a, int addend) {
  a->chunks[0] += addend;
}

void ap_int_set(struct ap_int *ap_int, signed long long value) {
  static_assert(sizeof(ap_int->chunks[0]) == sizeof(value), "size mismatch!");

  *ap_int = (struct ap_int){
    .num_bits = ap_int->num_bits,
    .chunks[0] = (uint64_t)value
  };
}

static void ap_int_negate_into(struct ap_int *value) {
  *value = (struct ap_int){
    .num_bits = value->num_bits,
    .chunks[0] = -(int64_t)value->chunks[0]
  };
}

struct ap_int ap_int_negate(struct ap_int value) {
  ap_int_negate_into(&value);
  return value;
}

struct ap_int ap_int_not(struct ap_int value) {
  size_t n = NUM_CHUNKS(value);

  for (size_t i = 0; i < n; i++) {
    value.chunks[i] = ~value.chunks[i];
  }

  return value;
}

bool ap_int_try_parse(UNUSED struct arena_allocator *arena, size_t num_bits,
                      const char *str, size_t len, struct ap_int *ap_int) {
  if (!*str) {
    return false;
  }

  size_t i = 0;

  bool neg = false;
  switch (*str) {
  case '+':
    i++;
    break;
  case '-':
    neg = true;
    i++;
    break;
  default:
    break;
  }

  if (i == len) {
    return false;
  }

  size_t rem = len - i - 1;
  int base = 10;

  if (rem >= 2 && str[i] == '0' && str[i + 1] == 'x') {
    i += 2;
    base = 16;
  } else if (rem >= 2 && str[i] == '0' && str[i + 1] == 'b') {
    i += 2;
    base = 2;
  } else if (rem >= 1 && str[i] == '0') {
    i++;
    base = 8;
  }

  // FIXME: current impl just uses unsigned long long and isn't actually ap

  struct ap_int value = ap_int_zero(num_bits);

  for (; i < len; i++) {
    char ch = str[i];

    if (ch == '\'') {
      // digit seperator
      continue;
    }

    // FIXME: should only allow as last chars
    if (tolower(ch) == 'l' || tolower(ch) == 'u') {
      break;
    }

    if (ch < '0') {
      return false;
    }

    char digit;
    if (ch <= '9') {
      digit = ch - '0';
    } else {
      digit = (char)(10 + (toupper(ch)) - 'A');
    }

    if (digit > base) {
      return false;
    }

    ap_int_mul_into_i32(&value, base);
    ap_int_add_into_i32(&value, digit);
  }

  if (neg) {
    ap_int_negate_into(&value);
  }

  *ap_int = value;
  return true;
}

bool ap_int_nonzero(struct ap_int value) {
  size_t n = NUM_CHUNKS(value);

  for (size_t i = 0; i < n; i++) {
    if (value.chunks[i]) {
      return true;
    }
  }

  return false;
}

bool ap_int_eq(struct ap_int lhs, struct ap_int rhs) {
  size_t n = MIN(NUM_CHUNKS(lhs), NUM_CHUNKS(rhs));

  for (size_t i = 0; i < n; i++) {
    if (lhs.chunks[i] != rhs.chunks[i]) {
      return false;
    }
  }

  return true;
}

bool ap_int_neq(struct ap_int lhs, struct ap_int rhs) {
  return !ap_int_eq(lhs, rhs);
}

static int ap_int_cmp_unsigned(const struct ap_int *a, const struct ap_int *b) {
  size_t n = NUM_CHUNKS(*a);

  for (int i = n - 1; i >= 0; i--) {
    if (a->chunks[i] < b->chunks[i]) {
      return -1;
    }

    if (a->chunks[i] > b->chunks[i]) {
      return 1;
    }
  }

  return 0;
}

bool ap_int_gt(struct ap_int lhs, struct ap_int rhs) {
  DEBUG_ASSERT(lhs.num_bits == rhs.num_bits, "ap_int size mismatch");

  bool lhs_neg = lhs.chunks[3] >> 63;
  bool rhs_neg = rhs.chunks[3] >> 63;

  if (lhs_neg != rhs_neg) {
    return !lhs_neg;
  }

  int cmp = ap_int_cmp_unsigned(&lhs, &rhs);
  return lhs_neg ? (cmp < 0) : (cmp > 0);
}

bool ap_int_gteq(struct ap_int lhs, struct ap_int rhs) {
  return !ap_int_lt(lhs, rhs);
}

bool ap_int_lt(struct ap_int lhs, struct ap_int rhs) {
  DEBUG_ASSERT(lhs.num_bits == rhs.num_bits, "ap_int size mismatch");

  bool lhs_neg = lhs.chunks[3] >> 63;
  bool rhs_neg = rhs.chunks[3] >> 63;

  if (lhs_neg != rhs_neg) {
    return lhs_neg;
  }

  int cmp = ap_int_cmp_unsigned(&lhs, &rhs);
  return lhs_neg ? (cmp > 0) : (cmp < 0);
}

bool ap_int_lteq(struct ap_int lhs, struct ap_int rhs) {
  return !ap_int_gt(lhs, rhs);
}

struct ap_int ap_int_xor(struct ap_int lhs, struct ap_int rhs) {
  DEBUG_ASSERT(lhs.num_bits == rhs.num_bits, "ap_int size mismatch");
  struct ap_int result = ap_int_zero(lhs.num_bits);

  size_t n = NUM_CHUNKS(result);

  for (size_t i = 0; i < n; i++) {
    result.chunks[i] = lhs.chunks[i] ^ rhs.chunks[i];
  }

  return result;
}

struct ap_int ap_int_or(struct ap_int lhs, struct ap_int rhs) {
  DEBUG_ASSERT(lhs.num_bits == rhs.num_bits, "ap_int size mismatch");
  struct ap_int result = ap_int_zero(lhs.num_bits);

  size_t n = NUM_CHUNKS(result);

  for (size_t i = 0; i < n; i++) {
    result.chunks[i] = lhs.chunks[i] | rhs.chunks[i];
  }

  return result;
}

struct ap_int ap_int_and(struct ap_int lhs, struct ap_int rhs) {
  DEBUG_ASSERT(lhs.num_bits == rhs.num_bits, "ap_int size mismatch");
  struct ap_int result = ap_int_zero(lhs.num_bits);

  size_t n = NUM_CHUNKS(result);

  for (size_t i = 0; i < n; i++) {
    result.chunks[i] = lhs.chunks[i] & rhs.chunks[i];
  }

  return result;
}

unsigned long long ap_int_as_ull(struct ap_int value) {
  if ((value.chunks[1] || value.chunks[2] || value.chunks[3]) &&
      !(value.chunks[1] == ULLONG_MAX && value.chunks[2] == ULLONG_MAX &&
        value.chunks[3] == ULLONG_MAX)) {
    // if (value.chunks[1] || value.chunks[2] || value.chunks[3]) {
    TODO("shifts for large/negative rhs");
  }

  return value.chunks[0];
}

struct ap_int ap_int_lshift(struct ap_int lhs, struct ap_int rhs) {
  CHECK_BIN();

  struct ap_int res = {.num_bits = lhs.num_bits,
                       .chunks = {[0] = lhs.chunks[0] << rhs.chunks[0]}};

  return res;
}

struct ap_int ap_int_rshift(struct ap_int lhs, struct ap_int rhs) {
   CHECK_BIN();

  struct ap_int res = {.num_bits = lhs.num_bits,
                       .chunks = {[0] = lhs.chunks[0] >> rhs.chunks[0]}};

  return res;
}

/************************** ap_float **************************/

bool ap_float_try_parse(struct arena_allocator *arena, enum ap_float_ty ty,
                        const char *str, size_t len,
                        struct ap_float *ap_float) {
  char *buf = arena_alloc_strndup(arena, str, len);

  char *end;

  bool has_suf = false;
  if (len) {
    char ch = (char)tolower(str[len - 1]);
    has_suf = ch == 'l' || ch == 'f';
  }

  ap_float->ty = ty;
  switch (ty) {
  case AP_FLOAT_TY_F16:
    ap_float->f16 = strtof(buf, &end);
    break;
  case AP_FLOAT_TY_F32:
    ap_float->f32 = strtof(buf, &end);
    break;
  case AP_FLOAT_TY_F64:
    ap_float->f64 = strtod(buf, &end);
    break;
  }

  size_t parse_len = end - buf;
  if (has_suf) {
    len--;
  }

  return parse_len == len;
}

struct ap_float ap_float_zero(enum ap_float_ty ty) {
  switch (ty) {
  case AP_FLOAT_TY_F16:
    return (struct ap_float){.ty = ty, .f16 = 0};
  case AP_FLOAT_TY_F32:
    return (struct ap_float){.ty = ty, .f32 = 0};
  case AP_FLOAT_TY_F64:
    return (struct ap_float){.ty = ty, .f64 = 0};
  }
}

bool ap_float_nonzero(struct ap_float val) {
  switch (val.ty) {
  case AP_FLOAT_TY_F16:
    return (bool)val.f16;
  case AP_FLOAT_TY_F32:
    return (bool)val.f32;
  case AP_FLOAT_TY_F64:
    return (bool)val.f64;
  }
}

#define AP_FLOAT_BIN_OP(op)                                                    \
  DEBUG_ASSERT((lhs).ty == (rhs).ty, "ap float ty mismatch");                  \
  switch ((lhs).ty) {                                                          \
  case AP_FLOAT_TY_F16:                                                        \
    return (struct ap_float){.ty = (lhs).ty, .f16 = (lhs).f16 op(rhs).f16};    \
  case AP_FLOAT_TY_F32:                                                        \
    return (struct ap_float){.ty = (lhs).ty, .f32 = (lhs).f32 op(rhs).f32};    \
  case AP_FLOAT_TY_F64:                                                        \
    return (struct ap_float){.ty = (lhs).ty, .f64 = (lhs).f64 op(rhs).f64};    \
  }

struct ap_float ap_float_add(struct ap_float lhs, struct ap_float rhs) {
  AP_FLOAT_BIN_OP(+)
}
struct ap_float ap_float_sub(struct ap_float lhs, struct ap_float rhs) {
  AP_FLOAT_BIN_OP(-)
}
struct ap_float ap_float_mul(struct ap_float lhs, struct ap_float rhs) {
  AP_FLOAT_BIN_OP(*)
}
struct ap_float ap_float_div(struct ap_float lhs,
                             struct ap_float rhs){AP_FLOAT_BIN_OP(/)}

#define AP_FLOAT_BIN_LOG_OP(op)                                                \
  DEBUG_ASSERT((lhs).ty == (rhs).ty, "ap float ty mismatch");                  \
  switch ((lhs).ty) {                                                          \
  case AP_FLOAT_TY_F16:                                                        \
    return (lhs).f16 op(rhs).f16;                                              \
  case AP_FLOAT_TY_F32:                                                        \
    return (lhs).f32 op(rhs).f32;                                              \
  case AP_FLOAT_TY_F64:                                                        \
    return (lhs).f64 op(rhs).f64;                                              \
  }

PUSH_NO_WARN("-Wfloat-equal") bool ap_float_eq(struct ap_float lhs,
                                               struct ap_float rhs) {
  AP_FLOAT_BIN_LOG_OP(==)
}

bool ap_float_neq(struct ap_float lhs, struct ap_float rhs) {
  AP_FLOAT_BIN_LOG_OP(!=)
}

bool ap_float_gt(struct ap_float lhs, struct ap_float rhs) {
  AP_FLOAT_BIN_LOG_OP(>)
}

bool ap_float_gteq(struct ap_float lhs, struct ap_float rhs) {
  AP_FLOAT_BIN_LOG_OP(>=)
}

bool ap_float_lt(struct ap_float lhs, struct ap_float rhs) {
  AP_FLOAT_BIN_LOG_OP(<)
}

bool ap_float_lteq(struct ap_float lhs,
                   struct ap_float rhs){AP_FLOAT_BIN_LOG_OP(<=)}

POP_NO_WARN()

#define AP_FLOAT_UN_OP(op)                                                     \
  switch ((value).ty) {                                                        \
  case AP_FLOAT_TY_F16:                                                        \
    return (struct ap_float){.ty = (value).ty, .f16 = op(value).f16};          \
  case AP_FLOAT_TY_F32:                                                        \
    return (struct ap_float){.ty = (value).ty, .f32 = op(value).f32};          \
  case AP_FLOAT_TY_F64:                                                        \
    return (struct ap_float){.ty = (value).ty, .f64 = op(value).f64};          \
  }

struct ap_float ap_float_negate(struct ap_float value) {
  AP_FLOAT_UN_OP(-)
}

// FIXME: this method is INHERENTLY BROKEN because it cannot handle sign
struct ap_val ap_val_from_ull(unsigned long long value) {
  struct ap_int ap_int = {
    .num_bits = 64
  };
  ap_int_set(&ap_int, value);

  return MK_AP_VAL_INT(ap_int);
}

#define AP_VAL_UN_OP(op)                                                       \
  switch (value.ty) {                                                          \
  case AP_VAL_TY_INVALID:                                                      \
    return MK_AP_VAL_INVALID();                                                \
  case AP_VAL_TY_INT:                                                          \
    return MK_AP_VAL_INT(ap_int_##op(value.ap_int));                           \
  case AP_VAL_TY_FLOAT:                                                        \
    return MK_AP_VAL_FLT(ap_float_##op(value.ap_float));                       \
  }

#define AP_VAL_BIN_OP(op)                                                      \
  if ((lhs).ty == AP_VAL_TY_INVALID || (rhs).ty == AP_VAL_TY_INVALID) {        \
    return MK_AP_VAL_INVALID();                                                \
  }                                                                            \
  DEBUG_ASSERT((lhs).ty == (rhs).ty, "ap_val type mismatch");                  \
  switch ((lhs).ty) {                                                          \
  case AP_VAL_TY_INT:                                                          \
    return MK_AP_VAL_INT(ap_int_##op((lhs).ap_int, (rhs).ap_int));             \
  case AP_VAL_TY_FLOAT:                                                        \
    return MK_AP_VAL_FLT(ap_float_##op((lhs).ap_float, (rhs).ap_float));       \
  case AP_VAL_TY_INVALID:                                                      \
    unreachable();                                                             \
  }

#define AP_VAL_BIN_LOG_OP(op)                                                  \
  if ((lhs).ty == AP_VAL_TY_INVALID || (rhs).ty == AP_VAL_TY_INVALID) {        \
    return MK_AP_VAL_INVALID();                                                \
  }                                                                            \
  DEBUG_ASSERT((lhs).ty == (rhs).ty, "ap_val type mismatch");                  \
  switch ((lhs).ty) {                                                          \
  case AP_VAL_TY_INT:                                                          \
    return ap_val_from_ull(ap_int_##op((lhs).ap_int, (rhs).ap_int));           \
  case AP_VAL_TY_FLOAT:                                                        \
    return ap_val_from_ull(ap_float_##op((lhs).ap_float, (rhs).ap_float));     \
  case AP_VAL_TY_INVALID:                                                      \
    unreachable();                                                             \
  }

#define AP_VAL_BIN_INT_OP(op)                                                  \
  if ((lhs).ty != AP_VAL_TY_INT || (rhs).ty != AP_VAL_TY_INT) {                \
    return MK_AP_VAL_INVALID();                                                \
  }                                                                            \
                                                                               \
  return MK_AP_VAL_INT(ap_int_##op((lhs).ap_int, (rhs).ap_int));

bool ap_val_nonzero(struct ap_val value) {
  switch (value.ty) {
  case AP_VAL_TY_INVALID:
    return false;
  case AP_VAL_TY_INT:
    return ap_int_nonzero(value.ap_int);
  case AP_VAL_TY_FLOAT:
    return ap_float_nonzero(value.ap_float);
  }
}

bool ap_val_zero(struct ap_val value) {
  switch (value.ty) {
  case AP_VAL_TY_INVALID:
    return false;
  case AP_VAL_TY_INT:
    return !ap_int_nonzero(value.ap_int);
  case AP_VAL_TY_FLOAT:
    return !ap_float_nonzero(value.ap_float);
  }
}

struct ap_val ap_val_add(struct ap_val lhs, struct ap_val rhs) {
  AP_VAL_BIN_OP(add);
}
struct ap_val ap_val_sub(struct ap_val lhs, struct ap_val rhs) {
  AP_VAL_BIN_OP(sub);
}
struct ap_val ap_val_mul(struct ap_val lhs, struct ap_val rhs) {
  AP_VAL_BIN_OP(mul);
}
struct ap_val ap_val_div(struct ap_val lhs, struct ap_val rhs) {
  AP_VAL_BIN_OP(div);
}

struct ap_val ap_val_mod(struct ap_val lhs, struct ap_val rhs) {
  AP_VAL_BIN_INT_OP(mod);
}

struct ap_val ap_val_eq(struct ap_val lhs, struct ap_val rhs) {
  AP_VAL_BIN_LOG_OP(eq);
}
struct ap_val ap_val_neq(struct ap_val lhs, struct ap_val rhs) {
  AP_VAL_BIN_LOG_OP(neq);
}

struct ap_val ap_val_gt(struct ap_val lhs, struct ap_val rhs) {
  AP_VAL_BIN_LOG_OP(gt);
}
struct ap_val ap_val_gteq(struct ap_val lhs, struct ap_val rhs) {
  AP_VAL_BIN_LOG_OP(gteq);
}
struct ap_val ap_val_lt(struct ap_val lhs, struct ap_val rhs) {
  AP_VAL_BIN_LOG_OP(lt);
}
struct ap_val ap_val_lteq(struct ap_val lhs, struct ap_val rhs) {
  AP_VAL_BIN_LOG_OP(lteq);
}

struct ap_val ap_val_rshift(struct ap_val lhs, struct ap_val rhs) {
  AP_VAL_BIN_INT_OP(rshift);
}
struct ap_val ap_val_lshift(struct ap_val lhs, struct ap_val rhs) {
  AP_VAL_BIN_INT_OP(lshift);
}

struct ap_val ap_val_or(struct ap_val lhs, struct ap_val rhs) {
  AP_VAL_BIN_INT_OP(or);
}
struct ap_val ap_val_and(struct ap_val lhs, struct ap_val rhs) {
  AP_VAL_BIN_INT_OP(and);
}
struct ap_val ap_val_xor(struct ap_val lhs, struct ap_val rhs) {
  AP_VAL_BIN_INT_OP(xor);
}

struct ap_val ap_val_negate(struct ap_val value) { AP_VAL_UN_OP(negate); }
struct ap_val ap_val_not(struct ap_val value) {
  switch (value.ty) {
  case AP_VAL_TY_INVALID:
  case AP_VAL_TY_FLOAT:
    return (struct ap_val){.ty = AP_VAL_TY_INVALID};
  case AP_VAL_TY_INT:
    return (struct ap_val){.ty = AP_VAL_TY_INT,
                           .ap_int = (ap_int_not(value.ap_int))};
  };
}
