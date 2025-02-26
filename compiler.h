#ifndef COMPILER_H
#define COMPILER_H

#include "program.h"
#include "target.h"

struct compiler;

enum compile_arch {
  COMPILE_ARCH_NATIVE, /* default */
  COMPILE_ARCH_X86_64,
  COMPILE_ARCH_ARM64,
  COMPILE_ARCH_RV32I,
  COMPILE_ARCH_EEP,
};

enum compile_target {
  COMPILE_TARGET_MACOS_ARM64 = 1 /* so that there is no default */,
  COMPILE_TARGET_MACOS_X86_64,
  COMPILE_TARGET_LINUX_ARM64,
  COMPILE_TARGET_LINUX_X86_64,
  COMPILE_TARGET_LINUX_RV32I,
  COMPILE_TARGET_EEP,
};

enum compile_log_flags {
  COMPILE_LOG_FLAGS_NONE = 0,
  COMPILE_LOG_FLAGS_ARGS = 1,
  COMPILE_LOG_FLAGS_PREPROC = 2,
  COMPILE_LOG_FLAGS_PARSE = 4,
  COMPILE_LOG_FLAGS_TYPECHK = 8,
  COMPILE_LOG_FLAGS_IR = 16,
  COMPILE_LOG_FLAGS_OPTS = 32,
  COMPILE_LOG_FLAGS_LOWER = 64,
  COMPILE_LOG_FLAGS_REGALLOC = 128,
  COMPILE_LOG_FLAGS_EMIT = 256,
  COMPILE_LOG_FLAGS_ASM = 512,

  COMPILE_LOG_FLAGS_ALL = -1,
};

enum compile_c_standard {
  COMPILE_C_STANDARD_C11,
  COMPILE_C_STANDARD_C17,
  COMPILE_C_STANDARD_C23,
};

enum compile_opts_level {
  COMPILE_OPTS_LEVEL_0,
  COMPILE_OPTS_LEVEL_1,
  COMPILE_OPTS_LEVEL_2,
  COMPILE_OPTS_LEVEL_3,
};

#define COMPILER_LOG_ENABLED(compiler, flag) compiler->args.log_flags &flag

struct compile_args {
  enum compile_c_standard c_standard;
  enum compile_target target;
  enum compile_log_flags log_flags;
  enum compile_opts_level opts_level;

  bool preproc_only;
  bool build_asm_file;
  bool build_object_file;

  size_t num_include_paths;
  const char **include_paths;

  const char *fixed_timestamp;

  const char *output;
};

enum compiler_create_result {
  COMPILER_CREATE_RESULT_SUCCESS,
  COMPILER_CREATE_RESULT_FAILURE
};

enum compile_result {
  COMPILE_RESULT_SUCCESS = 0,
  COMPILE_RESULT_BAD_FILE,
  COMPILE_RESULT_FAILURE
};

enum compiler_create_result create_compiler(struct program *program,
                                            const struct target *target,
                                            const char *output,
                                            const char *working_dir,
                                            const struct compile_args *args,
                                            struct compiler **compiler);
enum compile_result compile(struct compiler *compiler);
void free_compiler(struct compiler **compiler);

#endif
