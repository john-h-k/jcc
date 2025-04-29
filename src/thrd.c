#include "thrd.h"

#include "util.h"

#include <errno.h>
#include <pthread.h>

#ifdef PTHREAD_IMPL

/****************************** call_once ******************************/

void call_once(once_flag *flag, void (*func)(void)) {
  invariant_assert(!pthread_once(flag, func), "pthread_once failed");
}

/****************************** mtx_t ******************************/

int mtx_init(mtx_t *mtx, int type) {
  pthread_mutexattr_t attr;
  pthread_mutexattr_init(&attr);

  if (type & mtx_recursive) {
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
  } else {
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_NORMAL);
  }

  int ret = pthread_mutex_init(&mtx->handle, &attr);
  pthread_mutexattr_destroy(&attr);
  mtx->type = type;

  return ret == 0 ? thrd_success : thrd_error;
}

void mtx_destroy(mtx_t *mtx) { pthread_mutex_destroy(&mtx->handle); }

int mtx_lock(mtx_t *mtx) {
  return pthread_mutex_lock(&mtx->handle) == 0 ? thrd_success : thrd_error;
}

int mtx_trylock(mtx_t *mtx) {
  return pthread_mutex_trylock(&mtx->handle) == 0 ? thrd_success : thrd_error;
}

int mtx_unlock(mtx_t *mtx) {
  return pthread_mutex_unlock(&mtx->handle) == 0 ? thrd_success : thrd_error;
}

int mtx_timedlock(mtx_t *mtx, const struct timespec *ts) {
  if (!(mtx->type & mtx_timed)) {
    return thrd_error;
  }

#ifdef _POSIX_TIMEOUTS
  switch (pthread_mutex_timedlock(&mtx->handle, ts)) {
  case 0:
    return thrd_success;
  case ETIMEDOUT:
    return thrd_timedout;
  default:
    return thrd_error;
  }
#else
  (void)ts;
  return thrd_error;
#endif
}

/****************************** cnd_t ******************************/

// TODO: properly map return codes

int cnd_init(cnd_t *cond) { return pthread_cond_init(cond, NULL); }

int cnd_signal(cnd_t *cond) { return pthread_cond_signal(cond); }
int cnd_broadcast(cnd_t *cond) { return pthread_cond_broadcast(cond); }

int cnd_wait(cnd_t *cond, mtx_t *mutex) {
  return pthread_cond_wait(cond, &mutex->handle);
}

int cnd_timedwait(cnd_t *restrict cond, mtx_t *restrict mutex,
                  const struct timespec *restrict time_point) {
  switch (pthread_cond_timedwait(cond, &mutex->handle, time_point)) {
  case 0:
    return thrd_success;
  case ETIMEDOUT:
    return thrd_timedout;
  default:
    return thrd_error;
  }
}

void cnd_destroy(cnd_t *cond) { pthread_cond_destroy(cond); }

/****************************** thrd_t ******************************/

// adapter because `pthread_create` returns `void *` but `thrd_create` returns
// `int`
struct thrd_wrapper_arg {
  thrd_start_t func;
  void *arg;
};

static void *thrd_start_adapter(void *arg) {
  struct thrd_wrapper_arg wrapper = *(struct thrd_wrapper_arg *)arg;

  free(arg);

  int ret = wrapper.func(wrapper.arg);

  return (void *)(intptr_t)ret;
}

// 1mb
#define THRD_STACK_SIZE (1024 * 1024 * 8)

int thrd_create(thrd_t *thr, thrd_start_t func, void *arg) {
  struct thrd_wrapper_arg *wrap = malloc(sizeof(*wrap));

  if (!wrap) {
    return thrd_nomem;
  }

  pthread_attr_t attrs;
  pthread_attr_init(&attrs);
  pthread_attr_setstacksize(&attrs, THRD_STACK_SIZE);

  wrap->func = func;
  wrap->arg = arg;
  return pthread_create(thr, &attrs, thrd_start_adapter, wrap) == 0
             ? thrd_success
             : thrd_error;
}

int thrd_join(thrd_t thr, int *res) {
  void *ret;

  if (pthread_join(thr, &ret) != 0) {
    return thrd_error;
  }

  if (res) {
    *res = (int)(intptr_t)ret;
  }

  return thrd_success;
}

#if __STDC_VERSION__ >= 202311L
[[noreturn]] void thrd_exit(int res) {
#else
_Noreturn void thrd_exit(int res) {
#endif
  pthread_exit((void *)(intptr_t)res);
}

#endif
