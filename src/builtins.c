#include "builtins.h"

#define BUILTIN_TY(...)
#define BUILTIN_FN(name, ret_ty, param_count, param_tys) \
  const struct builtin_fn_spec builtin_ ## name = { \
    .ret = ret_ty, \
    .num_params = param_count, \
    .params = param_tys \
  };

BUILTINS_LIST

#undef BUILTIN_FN
#undef BUILTIN_TY
