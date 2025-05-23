# JCC

This site contains architectural/design information about JCC.

* [General style & utilities + code hygiene](./general.html)
* [Memory Management](./memory.html)
* [Parse](./parse.html)
* [Typechk](./typechk.html)
* [Regalloc](./regalloc.html)

# Architecture Overview

## Driver

The driver performs argument parsing, as well as invoking the various components used for compilation.

### Arg parsing

* Declarative style arguments for simplicit. Very macro-heavy
* Code is [`args.h`](https://github.com/john-h-k/jcc/tree/main/src/args.h) and [`args.c`](https://github.com/john-h-k/jcc/tree/main/src/args.c)

## Frontend

The frontend runs the preprocessor, lexer, and parser in lockstep, before performing semantic analysis (called 'typechk' within the compiler).

See the [Parse](./parse.html) article for a detailed breakdown of the lexer & parser.
See the [Typechk](./typechk.html) article for a detailed breakdown of the typechk semantic analysis pass.

* Preprocessor
  * Has two modes
    * Self-contained - when invoked with the `-E` flag, will run the preprocessor and output the result
    * Streaming - in normal compilation, tokens from the preprocessor are consumed and fed to the lexer
  * Code is [`preproc.h`](https://github.com/john-h-k/jcc/tree/main/src/preproc.h) and [`preproc.c`](https://github.com/john-h-k/jcc/tree/main/src/preproc.c)
* Lexer + Parser
  * These work in lockstep (tokens are provided on-demand by the lexer), and build the AST
  * It is a very loose and untyped AST, to try and parse as many programs as possible, with little verification
  * Lexing code is [`lex.h`](https://github.com/john-h-k/jcc/tree/main/src/lex.h) and [`lex.c`](https://github.com/john-h-k/jcc/tree/main/src/lex.c)
    * Lexer takes preproc tokens
  * Parsing code is [`parse.h`](https://github.com/john-h-k/jcc/tree/main/src/parse.h) and [`parse.c`](https://github.com/john-h-k/jcc/tree/main/src/parse.c)
* Semantic analysis - Typecheck
  * Builds a _typed_ AST from the parser output
  * Performs most validation (are types correct, do variables exist, etc)
  * Parsing code is [`typechk.h`](https://github.com/john-h-k/jcc/tree/main/src/typechk.h) and [`typechk.c`](https://github.com/john-h-k/jcc/tree/main/src/typechk.c)

## Backend

* Intermediate Representations and passes
  * All code located in the [`ir`](https://github.com/john-h-k/jcc/tree/main/src/ir) folder
  * IR representation structs and helper methods are in [`ir/ir.h`](https://github.com/john-h-k/jcc/tree/main/src/ir/ir.h) and [`ir/ir.c`](https://github.com/john-h-k/jcc/tree/main/src/ir/ir.c)
  * Pretty-printing functionality is in [`ir/prettyprint.h`](https://github.com/john-h-k/jcc/tree/main/src/ir/prettyprint.h) and [`ir/prettyprint.c`](https://github.com/john-h-k/jcc/tree/main/src/ir/prettyprint.c)
    * This also includes graph-building functionality with graphviz
  * IR building
    * This stage converts the AST into an SSA IR form
    * It assumes the AST is entirely valid and well-typed
    * Code is [`ir/build.h`](https://github.com/john-h-k/jcc/tree/main/src/ir/build.h) and [`ir/build.c`](https://github.com/john-h-k/jcc/tree/main/src/ir/build.c)
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
      * For example, AArch64 has no `%` instr, so `x = a % b` is converted to `c = a / b; x = a - (c * b)`
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
  * Currently only macOS Mach-O (in [macos](https://github.com/john-h-k/jcc/tree/main/src/macos)) and ELF (in [linux](https://github.com/john-h-k/jcc/tree/main/src/linux)) are supported
* Linking
  * Links using the platform linker
  * Effectively just runs the linker as one would from the command line
  * Platform specific link-code in [macos](https://github.com/john-h-k/jcc/tree/main/src/macos) and [linux](https://github.com/john-h-k/jcc/tree/main/src/linux)) folders


