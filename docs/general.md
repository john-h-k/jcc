# General style & utilities

JCC uses 'LLVM C-style', where typedefs are rarely used (preferring explicit `struct`/`union`/`enum`) usage. Some of the conventions in this style are odd, particularly in comparison to C++, but they are used consistently throughout the project. Examples:
* Two-letter prefixes
  * In a normal language you could have `codegen::alloc_instr` or similar, but that is not an option. Instead of cluttering files with huge function names, two letter prefixes are used, such as `cg_alloc_instr` or `vt_create_entry` (for creating an entry in a **v**ar-**t**able)
  * Namespacing is ignored for `static` functions only defined in the C file as there is no risk of collision

The two most important utility files are:

* [`compinfo.h`](https://github.com/john-h-k/jcc/tree/main/src/compinfo.h)
  - This file deals with the various different architectures, operating systems, and compilers that it may be being compiled on
  - It verifies things like the project being compiled as C (rather than C++) and that it is being compiled as C11 or greater
  - The OS and arch detection are important, as they are used to select the "native platform" for the compiler
  - It also detects the presence of various sanitisers (such as UBSan and AddressSan)

* [`util.h`](https://github.com/john-h-k/jcc/tree/main/src/util.h) & [`util.c`](https://github.com/john-h-k/jcc/tree/main/src/util.c)
  - Contains general purpose utilities
  - Most attributes (such as `__attribute__((format(printf)))` defined as `PRINTF_ARGS` macro) are defined here in a cross-compiler manner
  - Additionally, the various assertion and failure methods are in this file
    - `DEBUG_ASSERT(cond, msg)` - Standard debug assertion
    - `invariant_assert(cond, msg)` - An assertion that is included in all modes
    - `TODO(msg)` - For not yet implemented code paths
    - `BUG(msg)` - Similar to `DEBUG_ASSERT(0, msg)` but more explicit
    - `unreachable()` - Informs compiler this path is unreachable, while trapping in non-release modes
    - `BREAKPOINT()` - When ran in debugger, breakpoint at this location
    - `debug_print_stack_trace()` - This will either print the stack trace via the sanitiser library, or use `addr2line` to attempt to build one. All of the above assertions and failure methods call this method, so assertion failures provide full stacktraces, which is exceptionally useful for debugging

  - It also defines the `struct sized_str` type, which is used across the compiler instead of `const char *`
    - Allows handling of strings which contain null characters
    - More performant by eliminating frequent `strlen` calls
  
## Code hygiene & Readability

Readability is certainly negatively impacted by the project being in C, but that is sort of par for the course. The formatting conventions encourage shorter variable names, but sensible names are still chosen - especially for public interfaces - and comments are used prudently to inform the reader of context.

JCC extensively uses encapsulation to keep concerns seperated. A good example is the lexer type:

```c
struct lexer;

enum lex_create_result lexer_create(struct program *program,
                                    struct preproc *preproc,
                                    struct lexer **lexer);
void lexer_free(struct lexer **lexer);

struct lex_pos lex_get_position(struct lexer *lexer);
void lex_backtrack(struct lexer *lexer, struct lex_pos position);

void lex_peek_token(struct lexer *lexer, struct lex_token *token);
void lex_consume_token(struct lexer *lexer, struct lex_token token);

```

Consumers do not have access to the lexer itself, and can only interact with it via methods provided in the header. This ensures a minimal public interface for each type, as well as preventing a host of bugs related to accidentally copying or overwriting types. This is used in all places it is practical, with the notable exception being the IR format.

### IR

Due to the variety of different passes that must work with it (IR builder, optimisations, lowering, register allocation, codegen), and the need for performance, the type itself is provided publically. However, there are still several measures to minimise bugs and improve general hygiene. The `ir.h` file provides a large set of helper methods for many simple IR operations, such as adding new operations, detaching basicblocks, and pruning the IR graph. Additionally, there is an IR validation file [`validate.c`](https://github.com/john-h-k/jcc/tree/main/src/ir/validate.c) which is ran after every single mutating IR pass, and performs extensive validation of the IR graph. A small set of the checks it implements:

* Ensures all operations, statements, and basicblocks are correctly linked within the graph
* Ensures that incompatible operation flags are not set
* Ensures all instructions that consume addresses have addresses of type `IR_VAR_TY_POINTER`
* Ensures instructions have correct types, for example that a zero-extend op does not have a floating point input or output type
* Ensures that phi node predecessors are actually predecessors of the basicblock

Additionally, `DEBUG_ASSERT` is used liberally across all IR passes to clearly show function invariants to readers and quickly catch problems, rather than slowly corrupting the IR graph.
