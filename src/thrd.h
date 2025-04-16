#ifndef THRD_H
#define THRD_H

// polyfilled version of C11's `<threads.h>`

// TODO: support single-threaded only version of JCC that stubs these as empty

#if __has_include(<threads.h>)

#include <threads.h>

#elif __has_include(<pthread.h>)

#define PTHREAD_IMPL

#include <pthread.h>

#define ONCE_FLAG_INIT PTHREAD_ONCE_INIT

typedef pthread_once_t once_flag;

void call_once(once_flag* flag, void (*func)(void));

#else

#error "No thread library supported"

#endif

#endif
