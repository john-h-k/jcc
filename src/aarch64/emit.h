#ifndef AARCH64_EMIT_H
#define AARCH64_EMIT_H

#include "../emit.h"

const char *aarch64_linux_mangle(struct arena_allocator *arena, const char *name);
const char *aarch64_macos_mangle(struct arena_allocator *arena, const char *name);

// intervals MUST be sorted such that `interval[i].op_id == i` ID
struct emitted_unit aarch64_emit(const struct cg_unit *unit);

#endif
