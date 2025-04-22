#ifndef COMPILER_H
#define COMPILER_H

#include "diagnostics.h"
#include "fs.h"
#include "program.h"

#include <stdio.h>

struct compiler;

enum jcc_driver { JCC_DRIVER_COMPILER, JCC_DRIVER_LSP };

enum FLAG_ENUM codegen_flags {
  CODEGEN_FLAG_NONE = 0,

  // use mnemonics in assembly output
  CODEGEN_FLAG_MNEMONICS = 1 << 0,
};

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

enum compile_platform {
  COMPILE_PLATFORM_MACOS,
  COMPILE_PLATFORM_LINUX,
};

static inline void compile_target_decomp(enum compile_target target,
                                         enum compile_arch *arch,
                                         enum compile_platform *platform) {
  switch (target) {
  case COMPILE_TARGET_MACOS_ARM64:
    *arch = COMPILE_ARCH_ARM64;
    *platform = COMPILE_PLATFORM_MACOS;
    break;
  case COMPILE_TARGET_MACOS_X86_64:
    *arch = COMPILE_ARCH_X86_64;
    *platform = COMPILE_PLATFORM_MACOS;
    break;
  case COMPILE_TARGET_LINUX_ARM64:
    *arch = COMPILE_ARCH_ARM64;
    *platform = COMPILE_PLATFORM_LINUX;
    break;
  case COMPILE_TARGET_LINUX_X86_64:
    *arch = COMPILE_ARCH_X86_64;
    *platform = COMPILE_PLATFORM_MACOS;
    break;
  case COMPILE_TARGET_LINUX_RV32I:
    *arch = COMPILE_ARCH_RV32I;
    *platform = COMPILE_PLATFORM_LINUX;
    break;
  case COMPILE_TARGET_EEP:
    BUG("eep");
  }
}

enum FLAG_ENUM compile_log_flags {
  COMPILE_LOG_FLAGS_NONE = 0,
  COMPILE_LOG_FLAGS_ARGS = 1 << 0,
  COMPILE_LOG_FLAGS_PREPROC = 1 << 1,
  COMPILE_LOG_FLAGS_PARSE = 1 << 2,
  COMPILE_LOG_FLAGS_TYPECHK = 1 << 3,
  COMPILE_LOG_FLAGS_IR = 1 << 4,
  COMPILE_LOG_FLAGS_INLINE = 1 << 5,
  COMPILE_LOG_FLAGS_LOWER_ABI = 1 << 6,
  COMPILE_LOG_FLAGS_OPTS = 1 << 7,
  COMPILE_LOG_FLAGS_LOWER = 1 << 8,
  COMPILE_LOG_FLAGS_REGALLOC = 1 << 9,
  COMPILE_LOG_FLAGS_ELIM_PHI = 1 << 10,
  COMPILE_LOG_FLAGS_CODEGEN_PREPARE = 1 << 11,
  COMPILE_LOG_FLAGS_CODEGEN = 1 << 12,
  COMPILE_LOG_FLAGS_EMIT = 1 << 13,
  COMPILE_LOG_FLAGS_BUILD_OBJECT = 1 << 14,

  COMPILE_LOG_FLAGS_ALL = -1,
};

enum compile_language {
  COMPILE_LANGUAGE_NONE,
  COMPILE_LANGUAGE_C,
  COMPILE_LANGUAGE_C_HEADER,
  COMPILE_LANGUAGE_CPP_OUTPUT,
  COMPILE_LANGUAGE_OBJECT,
  COMPILE_LANGUAGE_SHAREDLIB,
};

enum compile_c_standard {
  COMPILE_C_STANDARD_C11,
  COMPILE_C_STANDARD_C17,
  COMPILE_C_STANDARD_C23,
  COMPILE_C_STANDARD_C2Y,
};

enum compile_opts_level {
  COMPILE_OPTS_LEVEL_0,
  COMPILE_OPTS_LEVEL_1,
  COMPILE_OPTS_LEVEL_2,
  COMPILE_OPTS_LEVEL_3,
};

#define COMPILER_LOG_ENABLED(compiler, flag) compiler->args.log_flags &flag

enum compile_file_ty {
  COMPILE_FILE_TY_NONE,
  COMPILE_FILE_TY_PATH,
  COMPILE_FILE_TY_STDOUT,
};

// the point of this struct is passing it around rather than `FILE *`, so we
// don't keep a file open during the entirety of compilation while still letting
// us disambiguate between stdout and a path easily
struct compile_file {
  enum compile_file_ty ty;

  union {
    const char *path;
  };
};

#define COMPILE_FILE_NONE ((struct compile_file){.ty = COMPILE_FILE_TY_NONE})
#define COMPILE_FILE_STDOUT                                                    \
  ((struct compile_file){.ty = COMPILE_FILE_TY_STDOUT})

FILE *compiler_open_file(struct compile_file file);

struct compile_args {
  enum compile_c_standard c_standard;
  enum compile_target target;
  enum compile_log_flags log_flags;
  enum compile_opts_level opts_level;
  enum codegen_flags codegen_flags;

  struct hashtbl *log_symbols;

  bool print_diagnostics;

  bool preproc_only;
  bool lex_only;
  bool parse_only;
  bool syntax_only;
  bool build_asm_file;
  bool build_object_file;

  bool no_warnings;
  bool warnings_as_errors;

  bool verbose;

  bool use_graphcol_regalloc;

  const char *diagnostics_sink;

  size_t num_defines;
  struct preproc_define_macro *defines;

  const char *sys_root;
  size_t num_sys_include_paths;
  const char **sys_include_paths;

  size_t num_include_paths;
  char **include_paths;

  const char *fixed_timestamp;

  struct compile_file output;
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

// used by lexer to decide if it uses preproc_next_token or
// preproc_next_raw_token
enum compile_preproc_mode {
  COMPILE_PREPROC_MODE_PREPROC,
  COMPILE_PREPROC_MODE_NO_PREPROC,
};

struct target;

struct compiler_create_args {
  struct program program;
  struct fs *fs;
  const struct target *target;
  struct compile_file output;
  const char *working_dir;
  struct compile_args args;
  enum compile_preproc_mode mode;
};

enum compiler_create_result
compiler_create(const struct compiler_create_args *args,
                struct compiler **compiler);

enum compile_result compile(struct compiler *compiler);

struct typechk;
struct typechk_result;
void compiler_get_tchk(struct compiler *compiler, struct typechk **tchk, struct typechk_result *result);

struct compiler_diagnostics *
compiler_get_diagnostics(struct compiler *compiler);

void free_compiler(struct compiler **compiler);

#endif
