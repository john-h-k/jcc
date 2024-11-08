<details>
  <summary>Using EEP mode</summary>

1. Compile the compiler (instructions assume macOS with clang & CMake installed)

```sh
mkdir build
cd build

cmake -DCMAKE_BUILD_TYPE=debug .. && cmake --build .
cd ..
```

2. The compiler binary will now be `build/jcc`
3. Run the compiler with `-Teep` flag on your file, e.g `./LOCATION_OF_JCC/build/jcc -Teep -Lasm mul.c` - the `-Lasm` flag will print out the EEP assembly as well
4. It will output a file next to it suffixed by `.obj` which will be your EEP assembly
5. Rename this from `.c.obj` to `.ram` and then you can import it into Issie

</details>

# jcc

JCC is designed to be a pure C11 (no dependencies) C11/C18/C23 compiler. 

## Is it sound?
No, it is text based

## Design

* Preprocessor
  * Runs before anything else
  * In the future, we could consider instead streaming tokens from preproc -> lexer for efficiency
  * Code is [`preproc.h`](preproc.h) and [`preproc.c`](preproc.c)
* Frontend - Lexer + Parser
  * These work in lockstep (tokens are provided on-demand by the lexer), and build the AST
  * It is a very loose and untyped AST, to try and parse as many programs as possible, with little verification
  * Lexing code is [`lex.h`](lex.h) and [`lex.c`](lex.c)
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
      * E.g `br.switch`s are converted into a series of if-elses, and `loadglb/storeglb` operations are transformed to `loadaddr/storeaddr`
    * This converts the IR into the platform-native form
    * Then, per-target lowering occurs
      * For example, AArch64 has no `%` instr, so `x = a % b` is converted to `c = a / b; x = a - (c * b)`
    * The code for lowering is within the appropriate backend folders
* Code Generation
  * Converts the IR into a list of 1:1 machine code instructions
  * These are all target specific
* Emitting
  * Actually emits the instructions from code generation into memory
* Object file building
  * Writes the object file to disk
  * Currently only macOS Mach-O format supported
  * Code is [`macos/mach-o.h`](macos/mach-o.h) and [`macos/mach-o.c`](macos/mach-o.c)
* Linking
  * Links using the platform linker
  * Effectively just runs the linker as one would from the command line
  * Code is [`link.h`](link.h) and [`link.c`](link.c)

