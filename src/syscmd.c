#include "syscmd.h"

#include "alloc.h"
#include "vector.h"

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
};

struct syscmd *syscmd_create(struct arena_allocator *arena,
                             const char *process) {
  struct syscmd *syscmd = arena_alloc(arena, sizeof(*syscmd));

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

void syscmd_write_cmd(struct syscmd *cmd, FILE *file) {
  size_t num_args = vector_length(cmd->args);
  for (size_t i = 0; i < num_args; i++) {
    const char **arg = vector_get(cmd->args, i);

    fprintf(file, "'%s'", *arg);
    if (i + 1 != num_args) {
      fprintf(file, " ");
    }
  }

  fprintf(file, "\n");
  fflush(file);
}

#if SYSCMD_UNIX

static int syscmd_open_fd(const char *output) {
  int fd = open(output, O_WRONLY | O_CREAT | O_TRUNC, 0644);

  invariant_assert(fd >= 0, "open file failed");

  return fd;
}

static char *syscmd_read_pipe(struct syscmd *cmd, enum syscmd_buf_flags flags,
                              int pipe[static 2]) {
  close(pipe[1]);

  struct vector *content = vector_create_in_arena(sizeof(char), cmd->arena);

  char buf[4096];
  ssize_t n;
  bool last_nl = false;
  while ((n = read(pipe[0], buf, sizeof(buf) - 1)) > 0) {
    last_nl = n && buf[n - 1] == '\n';
    buf[n] = '\0';
    vector_extend(content, buf, n);
  }

  close(pipe[0]);

  if ((flags & SYSCMD_BUF_FLAG_STRIP_TRAILING_NEWLINE) && last_nl) {
    char *c = vector_tail(content);
    *c = '\0';
  } else {
    vector_push_back(content, &(char){0});
  }

  return vector_head(content);
}

#if __has_include(<spawn.h>) && __has_include(<sys/types.h>) && __has_include(<sys/wait.h>)

#include <spawn.h>
#include <sys/types.h>
#include <sys/wait.h>

extern char **environ;

int syscmd_exec(struct syscmd **syscmd) {
  struct syscmd *s = *syscmd;

  vector_push_back(s->args, &(char *){NULL});
  char **args = vector_head(s->args);

  int stdout_pipe[2];
  int stderr_pipe[2];

  posix_spawn_file_actions_t actions;
  posix_spawn_file_actions_init(&actions);

  if (s->stdout_buf) {
    pipe(stdout_pipe);
    posix_spawn_file_actions_adddup2(&actions, stdout_pipe[1], STDOUT_FILENO);
    posix_spawn_file_actions_addclose(&actions, stdout_pipe[0]);
  } else if (s->stdout_redir) {
    int stdout_fd = syscmd_open_fd(s->stdout_redir);

    posix_spawn_file_actions_adddup2(&actions, stdout_fd, STDOUT_FILENO);
    posix_spawn_file_actions_addclose(&actions, stdout_fd);
  }

  if (s->stderr_buf) {
    pipe(stderr_pipe);
    posix_spawn_file_actions_adddup2(&actions, stderr_pipe[1], STDERR_FILENO);
    posix_spawn_file_actions_addclose(&actions, stderr_pipe[0]);
  } else if (s->stderr_redir) {
    int stderr_fd = syscmd_open_fd(s->stderr_redir);

    posix_spawn_file_actions_adddup2(&actions, stderr_fd, STDERR_FILENO);
    posix_spawn_file_actions_addclose(&actions, stderr_fd);
  }

  int status;

  pid_t pid;
  if (posix_spawnp(&pid, s->process, &actions, NULL, args, environ) != 0) {
    BUG("spawnp failed");
  }

  if (waitpid(pid, &status, 0) == -1) {
    BUG("waitpid failed");
  }

  if (s->stdout_buf) {
    *s->stdout_buf = syscmd_read_pipe(s, s->stdout_flags, stdout_pipe);
  }

  if (s->stderr_buf) {
    *s->stderr_buf = syscmd_read_pipe(s, s->stderr_flags, stderr_pipe);
  }

  posix_spawn_file_actions_destroy(&actions);

  *syscmd = NULL;
  return status;
}

#else

static void syscmd_child_redir(int redir_fd, const char *output) {
  int fd = syscmd_open_fd(output);

  dup2(fd, redir_fd);
  close(fd);
}

int syscmd_exec(struct syscmd **syscmd) {
  struct syscmd *s = *syscmd;

  int stdout_pipe[2];
  if (s->stdout_buf) {
    pipe(stdout_pipe);
  }

  int stderr_pipe[2];
  if (s->stderr_buf) {
    pipe(stderr_pipe);
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
