#ifndef X64_EMIT_H
#define X64_EMIT_H

#include "../emit.h"
#include "../ir/build.h"
#include "../lsra.h"

const char *x64_linux_mangle(struct arena_allocator *arena, const char *name);
const char *x64_macos_mangle(struct arena_allocator *arena, const char *name);

// intervals MUST be sorted such that `interval[i].op_id == i` ID
struct emitted_unit x64_emit(const struct codegen_unit *unit);

#endif
