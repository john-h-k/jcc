# Memory Management

Memory management in C is significantly constrained compared to literally any other language.
Correctness and performance are essential within a compiler.

The principal memory management primitives are located in [`alloc.h`](https://github.com/john-h-k/jcc/tree/main/src/alloc.h) and [`alloc.c`](https://github.com/john-h-k/jcc/tree/main/src/alloc.c).

The basic interface is:

```c
void arena_allocator_create(struct arena_allocator **allocator);
void arena_allocator_free(struct arena_allocator **allocator);

void *arena_alloc(struct arena_allocator *allocator, size_t size);
```

(There are many helper methods such as `arena_alloc_strdup` to perform C-stdlib functions within an arena).

## Correctness

All debug & test builds use address sanitiser and leak sanitiser where available, including in CI. This catches the vast majority of leaks, which primarily occur from the few usages of non-arena such as in the top-level compiler instances and the linker invokers. The small number of arenas used in the project, seperated generally by compiler stage, make verifying that they are freed very easy.

## Performance

The tree/graph-like nature of ASTs and IR mean allocations must be very fast. Parsing extremely large (100k lines+) files with `ALWAYS_MALLOC`, which does not use the arena, can lead to compile times with ~45% of time spent in `malloc` and ~50% spent in `free`, dominating program execution.

The core `arena_alloc` function is carefully optimised to be extremely fast.

```c
// marked as COLD to prevent inlining and keep the fast path small, as this is not a likely branch
COLD static void *arena_alloc_large(struct arena_allocator *allocator,
                                    size_t size);

static void *arena_alloc_in_new(struct arena_allocator *allocator,
                                size_t aligned);


void *arena_alloc(struct arena_allocator *allocator, size_t size) {
  // deterministically return NULL for zero-sized allocations
  if (!size) {
    return 0;
  }

  size_t aligned = ROUND_UP(size, ALIGNMENT);

  struct arena *arena;

  // unlikely macro uses `__builtin_expect` macro to inform compiler this is an unlikely branch
  if (UNLIKELY(aligned > BLOCK_SIZE)) {
    // slow path
    return arena_alloc_large(allocator, size);
  } else {
    arena = allocator->last;

    void *allocation;
    if (LIKELY(try_alloc_in_arena(arena, aligned, &allocation))) {
      return allocation;
    }
  }

  // need to create a new arena
  return arena_alloc_in_new(allocator, aligned);
}

```

`BLOCK_SIZE` & `ALIGNMENT` are both tunable. By default, block size is 4mb and alignment is 16.
The `ALWAYS_MALLOC` macro can be defined to force all allocations go through `malloc` instead without interfering with consumers.

## Containers

Originally, the container types (principally `struct vector` and `struct hashtbl`) allocated their memory via `malloc`. They have since been changed to support using arenas, and the majority of usages in the compile use these variants. At some point I will remove the old versions entirely, but it is not a priority.
