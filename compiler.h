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
  COMPILE_LOG_FLAGS_ARGS = 1 << 0,
  COMPILE_LOG_FLAGS_PREPROC = 1 << 1,
  COMPILE_LOG_FLAGS_PARSE = 1 << 2,
  COMPILE_LOG_FLAGS_TYPECHK = 1 << 3,
  COMPILE_LOG_FLAGS_IR = 1 << 4,
  COMPILE_LOG_FLAGS_LOWER_ABI = 1 << 5,
  COMPILE_LOG_FLAGS_OPTS = 1 << 6,
  COMPILE_LOG_FLAGS_LOWER = 1 << 7,
  COMPILE_LOG_FLAGS_REGALLOC = 1 << 8,
  COMPILE_LOG_FLAGS_ELIM_PHI = 1 << 9,
  COMPILE_LOG_FLAGS_CODEGEN_PREPARE = 1 << 10,
  COMPILE_LOG_FLAGS_CODEGEN = 1 << 11,
  COMPILE_LOG_FLAGS_EMIT = 1 << 12,
  COMPILE_LOG_FLAGS_BUILD_OBJECT = 1 << 13,

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

  bool use_graphcol_regalloc;

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
