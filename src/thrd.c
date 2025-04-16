#include "thrd.h"
#include "util.h"

#ifdef PTHREAD_IMPL

void call_once(once_flag* flag, void (*func)(void)) {
  invariant_assert(!pthread_once(flag, func), "pthread_once failed");
}

#endif
