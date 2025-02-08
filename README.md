# jcc

JCC is designed to be a pure C11 (no dependencies) C11/C18/C23 compiler. 

## CI Status

| OS     | AArch64 (Arm64)                                                                                   | x64 |
|--------|---------------------------------------------------------------------------------------------------|-----|
| Ubuntu | ![Ubuntu AArch64](https://github.com/john-h-k/jcc/actions/workflows/ubuntu-aarch64.yml/badge.svg) | WIP |
| macOS  | ![macOS AArch64](https://github.com/john-h-k/jcc/actions/workflows/macos-aarch64.yml/badge.svg)   | n/a |
| Debian | ![Debian AArch64](https://github.com/john-h-k/jcc/actions/workflows/debian-aarch64.yml/badge.svg) | WIP |

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

## Support

It currently supports AArch64 almost fully, with partial WIP RISC-V support. x64 is in the pipeline

#### Things that don't work yet

1. Macro-like functions (soon!)
2. `va_list` and variadic function implementation. Calling them works fine
3. Compound literals
4. Complex constant expressions like `int foo[7 * 3 + sizeof(int)] = {};`

## Design

* Preprocessor
  * Has two modes
    * Self-contained - when invoked with the `-E` flag, will run the preprocessor and output the result
    * Streaming - in normal compilation, tokens from the preprocessor are consumed and fed to the lexer
  * Code is [`preproc.h`](preproc.h) and [`preproc.c`](preproc.c)
* Frontend - Lexer + Parser
  * These work in lockstep (tokens are provided on-demand by the lexer), and build the AST
  * It is a very loose and untyped AST, to try and parse as many programs as possible, with little verification
  * Lexing code is [`lex.h`](lex.h) and [`lex.c`](lex.c)
    * Lexer takes preproc tokens
  * Parsing code is [`parse.h`](parse.h) and [`parse.c`](parse.c)
* Semantic analysis - Typecheck
  * Builds a _typed_ AST from the parser output
  * Performs most validation (are types correct, do variables exist, etc)
  * Parsing code is [`typechk.h`](typechk.h) and [`typechk.c`](typechk.c)
* Intermediate Representations and passes
  * All code located in the [`ir`](ir) folder
  * IR representation structs and helper methods are in [`ir/ir.h`](ir/ir.h) and [`ir/ir.c`](ir/ir.c)
  * Pretty-printing functionality is in [`ir/prettyprint.h`](ir/prettyprint.h) and [`ir/prettyprint.c`](ir/prettyprint.c)
    * This also includes graph-building functionality with graphviz
  * IR building
    * This stage converts the AST into an SSA IR form
    * It assumes the AST is entirely valid and well-typed
    * Code is [`ir/build.h`](ir/build.h) and [`ir/build.c`](ir/build.c)
  * Lowering
    * Firstly, global lowering is performed. This lowers certain operations that are lowered on all platforms
      * E.g `br.switch`s are converted into a series of if-elses, and `load.glb/store.glb` operations are transformed to `addr GLB + load.addr/store.addr`
    * This converts the IR into the platform-native form
    * Then, per-target lowering occurs
      * For example, AArch64 has no `%` instr, so `x = a % b` is converted to `c = a / b; x = a - (c * b)`
    * The code for lowering is within the appropriate backend folders
  * Register allocation
    * Simple LSRA, done seperately across floating-point & general-purpose registers
  * Eliminate phi
    * Splits critical edges and inserts moves to preserve semantics of phi ops
* Code Generation
  * Converts the IR into a list of 1:1 machine code instructions
  * These are all target specific
  * Currently codegen does too much - in the future I would like to move lots of its responsibilities (e.g ABI) into IR passes
* Emitting
  * Actually emits the instructions from code generation into memory
* Object file building
  * Writes the object file to disk
  * Currently only macOS Mach-O (in [macos](./macos)) and ELF (in [linux](./linux)) are supported
* Linking
  * Links using the platform linker
  * Effectively just runs the linker as one would from the command line
  * Platform specific link-code in [macos](./macos) and [linux](./linux)


