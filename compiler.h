#ifndef COMPILER_H
#define COMPILER_H

struct compiler;

enum compile_target_arch {
  COMPILE_TARGET_ARCH_MACOS_ARM64,
  COMPILE_TARGET_ARCH_MACOS_X86_64
};

struct compile_args {
  enum compile_target_arch target_arch;
};

enum compiler_create_result {
  COMPILER_CREATE_RESULT_SUCCESS,
  COMPILER_CREATE_RESULT_FAILURE
};

enum compile_result { COMPILE_RESULT_SUCCESS = 0, COMPILE_RESULT_BAD_FILE };

enum compiler_create_result create_compiler(const char *program,
                                            const char *output,
                                            const struct compile_args *args,
                                            struct compiler **compiler);
enum compile_result compile(struct compiler *compiler);
void free_compiler(struct compiler **compiler);

#endif
