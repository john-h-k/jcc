#ifndef THRD_H
#define THRD_H

// polyfilled version of C11's `<threads.h>`

// TODO: support single-threaded only version of JCC that stubs these as empty

// disabled due to stack size being too small
#if __has_include(<threads.h>) && 0

#include <threads.h>

#elif __has_include(<pthread.h>)

#define PTHREAD_IMPL

#include <pthread.h>

/****************************** call_once ******************************/

#define ONCE_FLAG_INIT PTHREAD_ONCE_INIT

typedef pthread_once_t once_flag;

void call_once(once_flag* flag, void (*func)(void));

/****************************** mtx_t ******************************/

typedef struct {
    pthread_mutex_t handle;
    int type;
} mtx_t;

enum {
  mtx_plain = 0,
  mtx_recursive = 1,
  mtx_timed = 2
};

int mtx_init(mtx_t* mtx, int type);
void mtx_destroy(mtx_t* mtx);
int mtx_lock(mtx_t* mtx);
int mtx_trylock(mtx_t* mtx);
int mtx_unlock(mtx_t* mtx);
int mtx_timedlock(mtx_t* mtx, const struct timespec* ts);

/****************************** thrd_t ******************************/

typedef pthread_t thrd_t;

typedef int (*thrd_start_t)(void*);

enum {
  thrd_success = 0,
  thrd_error = 1,
  thrd_nomem = 2,
  thrd_timedout = 3,
  thrd_busy = 4,
};

int thrd_create(thrd_t* thr, thrd_start_t func, void* arg);
int thrd_join(thrd_t thr, int* res);

#else

#error "No thread library supported"

#endif

#define LOCK(mtx, ...)                                                       \
  invariant_assert(mtx_lock((mtx)) == thrd_success, "lock failed");           \
  {__VA_ARGS__};                                                                     \
  invariant_assert(mtx_unlock((mtx)) == thrd_success, "unlock failed");

#endif
