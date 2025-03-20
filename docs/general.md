# General style & utilities

JCC uses 'LLVM C-style', where typedefs are rarely used (preferring explicit `struct`/`union`/`enum`) usage.

The two most important utility files are:

* [`compinfo.h`](https://github.com/john-h-k/jcc/tree/main/src/compinfo.h)
  - This file deals with the various different architectures, operating systems, and compilers that it may be being compiled on
  - It verifies things like the project being compiled as C (rather than C++) and that it is being compiled as C11 or greater
  - The OS and arch detection are important, as they are used to select the "native platform" for the compiler
  - It also detects the presence of various sanitisers (such as UBSan and AddressSan)

* [`util.h`](https://github.com/john-h-k/jcc/tree/main/src/util.h) & [`util.c`](https://github.com/john-h-k/jcc/tree/main/src/util.c)
  - Contains general purpose utilities
  - Most attributes (such as `__attribute__(format(printf))` defined as `PRINTF_ARGS` macro) are defined here in a cross-compiler manner
  - Additionally, the various assertion and failure methods are in this file
    - `DEBUG_ASSERT(cond, msg)` - Standard debug assertion
    - `invariant_assert(cond, msg)` - An assertion that is included in all modes
    - `TODO(msg)` - For not yet implemented code paths
    - `BUG(msg)` - Similar to `DEBUG_ASSERT(0, msg)` but more explicit
    - `unreachable()` - Informs compiler this path is unreachable, while trapping in non-release modes
    - `BREAKPOINT()` - When ran in debugger, breakpoint at this location

  - It also defines the `struct sized_str` type, which is used across the compiler instead of `const char *`
    - Allows handling of strings which contain null characters
    - More performant by eliminating frequent `strlen` calls
  
