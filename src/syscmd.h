#ifndef SYSCMD_H
#define SYSCMD_H

#include "alloc.h"

struct syscmd;

enum FLAG_ENUM syscmd_buf_flags {
  SYSCMD_BUF_FLAG_NONE = 0,
  SYSCMD_BUF_FLAG_STRIP_TRAILING_NEWLINE = 1 << 0,
};

struct syscmd *syscmd_create(struct arena_allocator *arena,
                             const char *process);

void syscmd_add_arg(struct syscmd *syscmd, const char *arg);
void syscmd_add_arg_val(struct syscmd *syscmd, const char *arg0,
                        const char *arg1);

// TODO: these should probably be a different type to allow non-filenames
void syscmd_set_stdout_path(struct syscmd *syscmd, enum syscmd_buf_flags flags,
                            const char *output);
void syscmd_set_stderr_path(struct syscmd *syscmd, enum syscmd_buf_flags flags,
                            const char *output);

// lives as long as arena does
void syscmd_set_stdout(struct syscmd *syscmd, enum syscmd_buf_flags flags,
                       char **buf);
void syscmd_set_stderr(struct syscmd *syscmd, enum syscmd_buf_flags flags,
                       char **buf);

void syscmd_set_stdin(struct syscmd *syscmd, ustr_t value);

void syscmd_write_cmd(const struct syscmd *restrict cmd, FILE *file);

int syscmd_exec(struct syscmd *restrict *syscmd);

struct syscmd_timeout {
  int secs;
};

enum syscmd_exec_result {
  SYSCMD_EXEC_RESULT_EXEC,
  SYSCMD_EXEC_RESULT_FAILED,
  SYSCMD_EXEC_RESULT_TIMEOUT,
};

struct syscmd_exec {
  enum syscmd_exec_result result;
  int exc;
};

// TODO: `sysmcd_exec` should also return this structure
struct syscmd_exec syscmd_timed_exec(struct syscmd *restrict *syscmd,
                                     struct syscmd_timeout timeout);

#endif
