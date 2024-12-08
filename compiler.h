#ifndef COMPILER_H
#define COMPILER_H

#include "program.h"

struct compiler;

enum compile_target_arch {
  COMPILE_TARGET_ARCH_NATIVE,
  COMPILE_TARGET_ARCH_MACOS_ARM64,
  COMPILE_TARGET_ARCH_MACOS_X86_64,
  COMPILE_TARGET_ARCH_EEP,
  COMPILE_TARGET_ARCH_RV32I
};

enum compile_log_flags {
  COMPILE_LOG_FLAGS_NONE = 0,
  COMPILE_LOG_FLAGS_PREPROC = 1,
  COMPILE_LOG_FLAGS_PARSE = 2,
  COMPILE_LOG_FLAGS_TYPECHK = 4,
  COMPILE_LOG_FLAGS_IR = 8,
  COMPILE_LOG_FLAGS_LOWER = 32,
  COMPILE_LOG_FLAGS_REGALLOC = 64,
  COMPILE_LOG_FLAGS_EMIT = 256,
  COMPILE_LOG_FLAGS_ASM = 512,

  COMPILE_LOG_FLAGS_ALL = -1,
};

#define COMPILER_LOG_ENABLED(compiler, flag) compiler->args.log_flags &flag

struct compile_args {
  enum compile_target_arch target_arch;
  enum compile_log_flags log_flags;

  bool preproc_only;
  bool build_object_file;

  size_t num_include_paths;
  const char **include_paths;

  char *output;
};

enum compiler_create_result {
  COMPILER_CREATE_RESULT_SUCCESS,
  COMPILER_CREATE_RESULT_FAILURE
};

enum compile_result { COMPILE_RESULT_SUCCESS = 0, COMPILE_RESULT_BAD_FILE, COMPILE_RESULT_FAILURE };

enum compiler_create_result create_compiler(struct program *program,
                                            const char *output,
                                            const struct compile_args *args,
                                            struct compiler **compiler);
enum compile_result compile(struct compiler *compiler);
void free_compiler(struct compiler **compiler);

#endif
