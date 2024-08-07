# How to use EEP mode

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


# jcc

JCC is designed to be a pure C11 (no dependencies) C11 compiler. 

## Is it sound?
No, it is text based

## Design

The current design consists of 3 stages:

* Frontend - Parser + Lexer
  * These work in lockstep (tokens are provided on-demand by the lexer), and build the AST
  * Lexing code is [`lex.h`](lex.h) and [`lex.c`](lex.c)
  * Parsing code is [`parse.h`](parse.h) and [`parse.c`](parse.c)
* Intermediate Representations and passes
  * All code located in the [`ir`](ir) folder
  * IR representation structs and helper methods are in [`ir/ir.h`](ir/ir.h) and [`ir/ir.c`](ir/ir.c)
  * Pretty-printing functionality is in [`ir/prettyprint.h`](ir/prettyprint.h) and [`ir/prettyprint.c`](ir/prettyprint.c)
    * This also includes graph-building functionality with graphviz
  * IR building
    * This stage converts the AST into an SSA IR form
    * Code is [`ir/build.h`](ir/build.h) and [`ir/build.c`](ir/build.c)
  * Lowering
    * This converts the IR into the platform-native form
    * Currently only AArch64 is supported
    * The code for lowering is in the [aarch64](aarch64) folder
      * [`aarch64/lower.c`](aarch64/lower.c) contains the code that actually lowers the IR to assembly
* Code Generation & Linking
  * Emitting
    * Currently only AArch64 is supported
    * The code for emitting AArch64 is in the [aarch64](aarch64) folder
      * [`aarch64/isa.h`](aarch64/isa.h) contains macros used for creating AArch64 instructions
      * [`aarch64/emitter.c`](aarch64/emitter.c) contains the `aarch64_emitter` type used to actually emit these into memory
  * Object file building
    * Writes the object file to disk
    * Currently only macOS Mach-O format supported
    * Code is [`macos/mach-o.h`](macos/mach-o.h) and [`macos/mach-o.c`](macos/mach-o.c)
  * Linking
    * Links using the platform linker
    * Effectively just runs the linker as one would from the command line
    * Code is [`link.h`](link.h) and [`link.c`](link.c)

