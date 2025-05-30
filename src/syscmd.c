#include "syscmd.h"

#include "alloc.h"
#include "vector.h"

#include <time.h>

#if __has_include(<unistd.h>) && __has_include(<fcntl.h>)
#include <fcntl.h>
#include <unistd.h>

#define SYSCMD_UNIX 1

#else

#error "TODO: `syscmd` for non unix platforms"

#endif

struct syscmd {
  struct arena_allocator *arena;

  const char *process;
  struct vector *args;

  const char *stdout_redir;
  const char *stderr_redir;

  enum syscmd_buf_flags stdout_flags;
  enum syscmd_buf_flags stderr_flags;

  char **stdout_buf;
  char **stderr_buf;

  const char *stdin_val;
};

struct syscmd *syscmd_create(struct arena_allocator *arena,
                             const char *process) {
  struct syscmd *syscmd = aralloc(arena, sizeof(*syscmd));

  *syscmd = (struct syscmd){
      .arena = arena,
      .process = process,
      .args = vector_create_in_arena(sizeof(const char *), arena)};

  // first arg is process name (even though we explicitly pass the process)
  vector_push_back(syscmd->args, &process);

  return syscmd;
}

void syscmd_add_arg(struct syscmd *syscmd, const char *arg) {
  DEBUG_ASSERT(arg, "null arg!");

  vector_push_back(syscmd->args, &arg);
}

void syscmd_add_arg_val(struct syscmd *syscmd, const char *arg0,
                        const char *arg1) {
  syscmd_add_arg(syscmd, arg0);
  syscmd_add_arg(syscmd, arg1);
}

void syscmd_set_stdout_path(struct syscmd *syscmd, enum syscmd_buf_flags flags,
                            const char *output) {
  syscmd->stdout_buf = NULL;
  syscmd->stdout_flags = flags;
  syscmd->stdout_redir = output;
}

void syscmd_set_stderr_path(struct syscmd *syscmd, enum syscmd_buf_flags flags,
                            const char *output) {
  syscmd->stderr_buf = NULL;
  syscmd->stderr_flags = flags;
  syscmd->stderr_redir = output;
}

void syscmd_set_stdout(struct syscmd *syscmd, enum syscmd_buf_flags flags,
                       char **buf) {
  *buf = NULL;
  syscmd->stdout_redir = NULL;
  syscmd->stdout_flags = flags;
  syscmd->stdout_buf = buf;
}

void syscmd_set_stderr(struct syscmd *syscmd, enum syscmd_buf_flags flags,
                       char **buf) {
  *buf = NULL;
  syscmd->stderr_flags = flags;
  syscmd->stderr_redir = NULL;
  syscmd->stderr_buf = buf;
}

void syscmd_write_cmd(const struct syscmd *restrict cmd, FILE *file) {
  size_t num_args = vector_length(cmd->args);
  for (size_t i = 0; i < num_args; i++) {
    const char **arg = vector_get(cmd->args, i);

    // post `syscmd_exec` there is a trailing null
    if (i + 1 == num_args && !*arg) {
      continue;
    }

    fprintf(file, "'%s'", *arg);
    if (i + 1 != num_args) {
      fprintf(file, " ");
    }
  }

  fprintf(file, "\n");
  fflush(file);
}

void syscmd_set_stdin(struct syscmd *syscmd, ustr_t value) {
  syscmd->stdin_val = aralloc_ustrconv(syscmd->arena, value);
}

#if SYSCMD_UNIX

static int syscmd_open_fd(const char *output) {
  int flags = O_WRONLY | O_CREAT;
  if (strcmp(output, "/dev/null") != 0) {
    flags |= O_TRUNC;
  }

  int fd = open(output, flags, 0644);

  invariant_assert(fd >= 0, "open file '%s' failed", output);

  return fd;
}

static char *syscmd_read_pipe(const struct syscmd *cmd,
                              enum syscmd_buf_flags flags, int pipe[static 2]) {
  struct vector *content = vector_create_in_arena(sizeof(char), cmd->arena);

  char buf[4096];
  ssize_t n;
  bool last_nl = false;
  while ((n = read(pipe[0], buf, sizeof(buf) - 1)) > 0) {
    last_nl = n && buf[n - 1] == '\n';
    buf[n] = '\0';
    vector_extend(content, buf, n);
  }

  if ((flags & SYSCMD_BUF_FLAG_STRIP_TRAILING_NEWLINE) && last_nl) {
    char *c = vector_tail(content);
    *c = '\0';
  } else {
    vector_push_back(content, &(char){0});
  }

  return vector_head(content);
}

#if __has_include(                                                             \
    <spawn.h>) && __has_include(<sys/types.h>) && __has_include(<sys/wait.h>)

#include <errno.h>
#include <spawn.h>
#include <sys/types.h>
#include <sys/wait.h>

extern char **environ;

#define SYSCMD_TIMEOUT_NONE (-1)
#define SYSCMD_TIMEOUT_POLL_MS (10)

int syscmd_exec(struct syscmd *restrict *syscmd) {

  struct syscmd_exec result = syscmd_timed_exec(
      syscmd, (struct syscmd_timeout){.secs = SYSCMD_TIMEOUT_NONE});
  switch (result.result) {

  case SYSCMD_EXEC_RESULT_EXEC:
    return result.exc;
  case SYSCMD_EXEC_RESULT_FAILED:
    // arbitrary
    // TODO: should also return syscmd_exec
    return -1;
  case SYSCMD_EXEC_RESULT_TIMEOUT:
    unreachable();
  }
}

struct syscmd_exec syscmd_timed_exec(struct syscmd *restrict *syscmd,
                                     struct syscmd_timeout timeout) {
  const struct syscmd *restrict s = *syscmd;

  vector_push_back(s->args, &(char *){NULL});
  char **args = vector_head(s->args);

  int stdin_pipe[2];
  int stdout_pipe[2];
  int stderr_pipe[2];

  int stdout_fd;
  int stderr_fd;

  posix_spawn_file_actions_t actions;
  posix_spawn_file_actions_init(&actions);

  if (s->stdin_val) {
    DEBUG_ASSERT(!pipe(stdin_pipe), "pipe failed");
    posix_spawn_file_actions_adddup2(&actions, stdin_pipe[0], STDIN_FILENO);
    posix_spawn_file_actions_addclose(&actions, stdin_pipe[1]);
  }

  if (s->stdout_buf) {
    DEBUG_ASSERT(!pipe(stdout_pipe), "pipe failed");
    posix_spawn_file_actions_adddup2(&actions, stdout_pipe[1], STDOUT_FILENO);
    posix_spawn_file_actions_addclose(&actions, stdout_pipe[0]);
  } else if (s->stdout_redir) {
    stdout_fd = syscmd_open_fd(s->stdout_redir);

    posix_spawn_file_actions_adddup2(&actions, stdout_fd, STDOUT_FILENO);
    posix_spawn_file_actions_addclose(&actions, stdout_fd);
  }

  if (s->stderr_buf) {
    DEBUG_ASSERT(!pipe(stderr_pipe), "pipe failed");
    posix_spawn_file_actions_adddup2(&actions, stderr_pipe[1], STDERR_FILENO);
    posix_spawn_file_actions_addclose(&actions, stderr_pipe[0]);
  } else if (s->stderr_redir) {
    stderr_fd = syscmd_open_fd(s->stderr_redir);

    posix_spawn_file_actions_adddup2(&actions, stderr_fd, STDERR_FILENO);
    posix_spawn_file_actions_addclose(&actions, stderr_fd);
  }

  pid_t pid;
  int ret;
  if ((ret = posix_spawnp(&pid, s->process, &actions, NULL, args, environ)) !=
      0) {
    fprintf(stderr, "spawnp '");
    syscmd_write_cmd(s, stderr);
    BUG_PERROR(ret, "spawnp failed!");
  }

  if (s->stdin_val) {
    size_t len = strlen(s->stdin_val);
    size_t written = 0;

    while (written < len) {
      written += write(stdin_pipe[1], &s->stdin_val[written], len - written);
    }

    close(stdin_pipe[1]);
  }

  bool timed_out = false;
  int status;

  if (timeout.secs == SYSCMD_TIMEOUT_NONE) {
    if (waitpid(pid, &status, 0) == -1) {
      BUG("waitpid failed");
    }
  } else {
    // poll-loop

    struct timespec interval = {.tv_sec = 0,
                                .tv_nsec = SYSCMD_TIMEOUT_POLL_MS * 1000000};

    struct timespec start;
    struct timespec now;

    invariant_assert(TIME_UTC == timespec_get(&start, TIME_UTC),
                     "timespec_get failed");

    while (1) {
      pid_t st = waitpid(pid, &status, WNOHANG);
      if (st == -1) {
        BUG("waitpid failed");
      }

      if (st > 0) {
        break;
      }

      invariant_assert(TIME_UTC == timespec_get(&now, TIME_UTC),
                       "timespec_get failed");

      // FIXME: doesn't handle nanos properly
      time_t elapsed = now.tv_sec - start.tv_sec;

      if (elapsed >= timeout.secs) {
        // TODO: should we give a more lenient signal first?
        kill(pid, SIGKILL);

        timed_out = true;
        break;
      }

      nanosleep(&interval, NULL);
    }
  }

  if (s->stdout_buf) {
    close(stdout_pipe[1]);

    if (timed_out) {
      *s->stdout_buf = NULL;
    } else {
      *s->stdout_buf = syscmd_read_pipe(s, s->stdout_flags, stdout_pipe);
    }

    close(stdout_pipe[0]);
  } else if (s->stdout_redir) {
    close(stdout_fd);
  }

  if (s->stderr_buf) {
    close(stderr_pipe[1]);

    if (timed_out) {
      *s->stderr_buf = NULL;
    } else {
      *s->stderr_buf = syscmd_read_pipe(s, s->stderr_flags, stderr_pipe);
    }

    close(stderr_pipe[0]);
  } else if (s->stderr_redir) {
    close(stderr_fd);
  }

  posix_spawn_file_actions_destroy(&actions);

  *syscmd = NULL;

  if (timed_out) {
    return (struct syscmd_exec){.result = SYSCMD_EXEC_RESULT_TIMEOUT};
  } else if (WIFEXITED(status)) {
    return (struct syscmd_exec){.result = SYSCMD_EXEC_RESULT_EXEC,
                                .exc = WEXITSTATUS(status)};
  } else {
    return (struct syscmd_exec){.result = SYSCMD_EXEC_RESULT_FAILED};
  }
}

#else

static void syscmd_child_redir(int redir_fd, const char *output) {
  int fd = syscmd_open_fd(output);

  dup2(fd, redir_fd);
  close(fd);
}

int syscmd_exec(struct syscmd **syscmd) {
  struct syscmd *s = *syscmd;

  // TODO: handle stdin

  int stdout_pipe[2];
  if (s->stdout_buf) {
    DEBUG_ASSERT(!pipe(stdout_pipe), "pipe failed");
  }

  int stderr_pipe[2];
  if (s->stderr_buf) {
    DEBUG_ASSERT(!pipe(stderr_pipe), "pipe failed");
  }

  int status;

  pid_t pid = fork();
  if (pid == 0) {
    vector_push_back(s->args, &(char *){NULL});
    char **args = vector_head(s->args);

    if (s->stdout_redir) {
      syscmd_child_redir(STDOUT_FILENO, s->stdout_redir);
    } else if (s->stdout_buf) {
      close(stdout_pipe[0]);
      dup2(stdout_pipe[1], STDOUT_FILENO);
      close(stdout_pipe[1]);
    }

    if (s->stderr_redir) {
      syscmd_child_redir(STDERR_FILENO, s->stderr_redir);
    } else if (s->stderr_buf) {
      close(stderr_pipe[0]);
      dup2(stderr_pipe[1], STDERR_FILENO);
      close(stderr_pipe[1]);
    }

    invariant_assert(execvp(s->process, args) >= 0, "execvp call failed!");

    unreachable();
  } else if (pid < 0) {
    BUG("fork failed");
  } else {
    if (waitpid(pid, &status, 0) < 0) {
      BUG("waitpid");
    }
  }

  // FIXME: is it safe to assume no null chars?

  if (s->stdout_buf) {
    *s->stdout_buf = syscmd_read_pipe(s, s->stdout_flags, stdout_pipe);
  }

  if (s->stderr_buf) {
    *s->stderr_buf = syscmd_read_pipe(s, s->stderr_flags, stderr_pipe);
  }

  *syscmd = NULL;
  return status;
}

#endif

#endif
