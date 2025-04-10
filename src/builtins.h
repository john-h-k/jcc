#ifndef BUILTINS_H
#define BUILTINS_H

#include <stddef.h>
#include "typechk.h"

// `BUILTIN_FN` the macro for each builtin function
// `BUILTIN_TY` the macro for each builtin type

enum builtin_type_spec_ty {
  BUILTIN_TYPE_SPEC_TY_ANY,
  BUILTIN_TYPE_SPEC_TY_BUILTIN,
  BUILTIN_TYPE_SPEC_TY_VA_LIST,
  BUILTIN_TYPE_SPEC_TY_TD_VAR_TY,
};

struct builtin_type_spec {
  enum builtin_type_spec_ty ty;

  union {
    const struct td_var_ty *td_var_ty;
  };
};

struct builtin_fn_spec {
  struct builtin_type_spec ret;
  struct builtin_type_spec params[16];
  size_t num_params;
};

#define BUILTIN_TYPE_SPEC_TD_VAR_TY(td)                                              \
  ((struct builtin_type_spec){.ty = BUILTIN_TYPE_SPEC_TY_TD_VAR_TY, .td_var_ty = (td)})

#define BUILTIN_TYPE_SPEC_VA_LIST                                              \
  ((struct builtin_type_spec){.ty = BUILTIN_TYPE_SPEC_TY_VA_LIST})

#define BUILTIN_TYPE_SPEC_ANY                                              \
  ((struct builtin_type_spec){.ty = BUILTIN_TYPE_SPEC_TY_ANY})

#define ARR_LIT(...) __VA_ARGS__

#define BUILTINS_LIST                                                          \
                                                                               \
  BUILTIN_TY(va_list)                                                          \
                                                                               \
  /********** FUNCS **********/                                                \
  BUILTIN_FN(va_start, BUILTIN_TYPE_SPEC_TD_VAR_TY(&TD_VAR_TY_VOID), 1, ARR_LIT({BUILTIN_TYPE_SPEC_VA_LIST}))                            \
  BUILTIN_FN(va_arg, BUILTIN_TYPE_SPEC_TD_VAR_TY(&TD_VAR_TY_VOID), 1, ARR_LIT({BUILTIN_TYPE_SPEC_VA_LIST}))                              \
  BUILTIN_FN(va_copy, BUILTIN_TYPE_SPEC_TD_VAR_TY(&TD_VAR_TY_VOID), 2, ARR_LIT({BUILTIN_TYPE_SPEC_VA_LIST, BUILTIN_TYPE_SPEC_VA_LIST}))                             \
  BUILTIN_FN(va_end, BUILTIN_TYPE_SPEC_TD_VAR_TY(&TD_VAR_TY_VOID), 1, ARR_LIT({BUILTIN_TYPE_SPEC_VA_LIST}))                              \
                                                                               \
  /* TODO: BUILTIN_FN(err_expr, {BUILTIN_TYPE_SPEC_ANY}) */

#define BUILTIN_TY(...)
#define BUILTIN_FN(name, ret_ty, param_count, param_tys) \
  const extern struct builtin_fn_spec builtin_ ## name;

BUILTINS_LIST

#undef BUILTIN_FN
#undef BUILTIN_TY

#endif
