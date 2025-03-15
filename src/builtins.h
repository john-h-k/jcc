#ifndef BUILTINS_H
#define BUILTINS_H

// `BUILTIN_FN` the macro for each builtin function
// `BUILTIN_TY` the macro for each builtin type

#define BUILTINS_LIST                                                          \
  \                                                                            \
                                                                               \
      /********** TYPES **********/                                            \
      BUILTIN_TY(va_list)                                                      \
                                                                               \
      /********** FUNCS **********/                                            \
      BUILTIN_FN(va_start) BUILTIN_FN(va_arg) BUILTIN_FN(va_copy)              \
          BUILTIN_FN(va_end)

#endif
