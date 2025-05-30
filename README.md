# jcc

JCC is designed to be a pure C11 (no dependencies) C11/C18/C23 compiler.

<sup><sub>No dependencies = no dependencies outside of libc and some basic OS calls needed to e.g read directories</sub></sup>

* For the work-in-progress rustc backend, see [rustc_codegen_jcc](https://github.com/john-h-k/rustc_codegen_jcc)

## CI Status

| OS     | AArch64 (Arm64)                                                                                          | x64                                                                                                      | RISC-V (32)                                                                                              |
|--------|--------------------------------------------------------------------------------------------------------- |---------------------------------------------------------------------------------------------------       | -------------------------------------------------------------------------------------------------------- |
| Ubuntu | ![Ubuntu AArch64](https://img.shields.io/github/actions/workflow/status/john-h-k/jcc/ubuntu-aarch64.yml) | ![Ubuntu x64](https://img.shields.io/github/actions/workflow/status/john-h-k/jcc/ubuntu-x86_64.yml)      |    ![Ubuntu RISC-V](https://img.shields.io/github/actions/workflow/status/john-h-k/jcc/ubuntu-riscv.yml) |
| macOS  | ![macOS AArch64](https://img.shields.io/github/actions/workflow/status/john-h-k/jcc/macos-aarch64.yml)   | ![macOS x64](https://img.shields.io/github/actions/workflow/status/john-h-k/jcc/macos-x86_64.yml)        |                                                                                                          |

<sup><sub>If tests are failing, ignore it! Development is very active (and pushes sometimes break things)</sub></sup>

Aims:
* To be a complete C11/C18/C23 compiler with full functionality (WIP)
* To use zero third-party dependencies or helper tools (no parser generators, assemblers, lexers, etc) other than system linker
* To follow best practices and have sensible compiler architecture
  * Building the "smallest" C compiler is an explicit non-goal
* To be useful for learning about compilers
  * Uses proper IRs rather than AST -> ASM
  * Generates machine code, not assembly
  * Builds SSA form and puts values in registers rather than spilling everything
  * Builds object files and invokes system linker manually (rather than via a compiler or an assembler)
  * Doesn't use hacks (mostly...)

## Is it sound?
No, it is text based

## Why?

I just wanted to write a C compiler. It happens to be an easily buildable, easily runnable compiler that is still a grokkable size, It is probably too large to be considered a toy compiler, but the core architecture is much more accessible
than the the shoggoth of Clang/GCC.

## Support

Compiler can fully bootstrap itself and passes the full C std test suite (which is not as large as it sounds).

AArch64, x64, and RISC-V 32 are supported, although some of the x64 ABI is not yet fully implemented and RISC-V 32 64 bit integers are WIP.
Working with RISC-V requires installing a RISC-V linker.

#### Things that don't work yet

1. `va_arg` outside of macOS arm64
2. Atomics (`stdatomic.h`)
3. Linking on musl-based distros. This is relatively simple and should work soon
4. Digraphs (plan to add) and trigraphs (plan to add but only with an explicit argument, as with most compilers)
5. `setjmp`/`longjmp`
6. Some small C23 features (`nullptr_t`, which I will get round to soon, `static` in compound literals, `u8` strings, and the inbuilt attributes)

## Requirements

<div style="display: flex; justify-content: space-between;">

#### For installation

<div style="width: 48%;">

* C11-compliant C compiler
* POSIX shell
* `git`, `curl`, or `wget` for downloading sources
* Nothing else!

</div>

#### For development

<div style="width: 48%; border-left: 1px solid black; padding-left: 10px;">

* C11-compliant C compiler
* Bash, version >=3
* CMake
* A few other tools are used by `jcc.sh` commands to make for a more pleasant experience, but are not needed. These include `bat` (for syntax-highlighting), `fd`, and `rg`

</div>

</div>

### Installation

To directly install `jcc` for playing around with (tested on macOS & various Linux distros):

```sh
curl -sSL https://jcc.johnk.dev/install.sh | sh
```

<sup><sub>The above URL is just a direct fetch of [./scripts/install.sh](./scripts/install.sh) which you can verify by visiting it. </sub></sup>
<sup><sub>It is NOT a redirect, it forwards the content itself. If you prefer, you can directly curl the script from <a href="http://raw.githubusercontent.com/john-h-k/jcc/refs/heads/main/scripts/install.sh">raw.githubusercontent.com/john-h-k/jcc/refs/heads/main/scripts/install.sh</a></sub></sup>

`wget` can also be used, or you can clone the repository and run `./scripts/install.sh` if you somehow have `git` but not `curl` or `wget`(???).

To install for development (which is realistically what you should do!):

* Ensure you have `bash` and `cmake` installed
* Fork & clone the repo (exercise left to reader)
* Run `./jcc.sh` for help

### Development

The `jcc.sh` script can be used for common workflows. A key subset of the commands can be seen here (run `./jcc.sh` for all commands):

```
jcc.sh COMMAND

COMMANDS:
    help        Show help
    run         Build, then run JCC with provided arguments
    debug       Build, then run JCC under LLDB/GDB with provided arguments
    test        Run tests
    test-all    Run tests with all optimisation levels
    format      Format codebase
```

For the test script, run `jcc.sh test help`.

## Design

* Arg parsing
  * Declarative style arguments for simplicit. Very macro-heavy
  * Code is [`args.h`](src/args.h) and [`args.c`](src/args.c)
* Preprocessor
  * Has two modes
    * Self-contained - when invoked with the `-E` flag, will run the preprocessor and output the result
    * Streaming - in normal compilation, tokens from the preprocessor are consumed and fed to the lexer
  * Code is [`preproc.h`](src/preproc.h) and [`preproc.c`](src/preproc.c)
* Frontend - Lexer + Parser
  * These work in lockstep (src/tokens are provided on-demand by the lexer), and build the AST
  * It is a very loose and untyped AST, to try and parse as many programs as possible, with little verification
  * Lexing code is [`lex.h`](src/lex.h) and [`lex.c`](src/lex.c)
    * Lexer takes preproc tokens
  * Parsing code is [`parse.h`](src/parse.h) and [`parse.c`](src/parse.c)
* Semantic analysis - Typecheck
  * Builds a _typed_ AST from the parser output
  * Performs most validation (src/are types correct, do variables exist, etc)
  * Parsing code is [`typechk.h`](src/typechk.h) and [`typechk.c`](src/typechk.c)
* Intermediate Representations and passes
  * All code located in the [`ir`](src/ir) folder
  * IR representation structs and helper methods are in [`ir/ir.h`](src/ir/ir.h) and [`ir/ir.c`](src/ir/ir.c)
  * Pretty-printing functionality is in [`ir/prettyprint.h`](src/ir/prettyprint.h) and [`ir/prettyprint.c`](src/ir/prettyprint.c)
    * This also includes graph-building functionality with graphviz
  * IR building
    * This stage converts the AST into an SSA IR form
    * It assumes the AST is entirely valid and well-typed
    * Code is [`ir/build.h`](src/ir/build.h) and [`ir/build.c`](src/ir/build.c)
  * ABI Lowering
    * Lowers arguments and return values into platform specifics (e.g large structs to pointers)
    * Doing this earlier helps optimisation stage
  * Optimisation passes
    * These include inlining, constant folding, dead-branch elimination, and local promotion (a la LLVM's `mem2reg`)
  * Lowering
    * Firstly, global lowering is performed. This lowers certain operations that are lowered on all platforms
      * E.g `br.switch`s are converted into a series of if-elses, and `load.glb/store.glb` operations are transformed to `addr GLB + load.addr/store.addr`
    * This converts the IR into the platform-native form
    * Then, per-target lowering occurs
      * For example, AArch64 has no `%` instr, so `x = a % b` is converted to `c = a / b; x = a - (src/c * b)`
    * The code for lowering is within the appropriate backend folders
  * Register allocation
    * Simple LSRA, done seperately across floating-point & general-purpose registers
  * Eliminate phi
    * Splits critical edges and inserts moves to preserve semantics of phi ops
* Code Generation
  * Converts the IR into a list of 1:1 machine code instructions
  * These are all target specific
  * Currently codegen does too much - in the future I would like to move lots of its responsibilities (e.g prologue/epilogue) into IR passes
* Emitting
  * Actually emits the instructions from code generation into memory
* Object file building
  * Writes the object file to disk
  * Currently only macOS Mach-O (src/in [macos](src/macos)) and ELF (src/in [linux](src/linux)) are supported
* Linking
  * Links using the platform linker
  * Effectively just runs the linker as one would from the command line
  * Platform specific link-code in [macos](src/macos) and [linux](src/linux)


