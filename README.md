# jcc

JCC is designed to be a pure C11 (no dependencies) C11 compiler. 

## Design

The current design consists of <N> stages:

* Parser + Lexer
  * These work in lockstep (tokens are provided on-demand by the lexer), and build the AST
  * Lexing code is [`lex.h`](lex.h) and [`lex.c`](lex.c)
  * Parsing code is [`parse.h`](parse.h) and [`parse.c`](parse.c)
* IR building
  * This stage converts the AST into an SSA (not yet perfect SSA) IR form
  * Code is [`ir.h`](ir.h) and [`ir.c`](ir.c)
* Lowering
  * This converts the IR into the platform-native form
  * Currently only AArch64 is supported
  * The code for lowering is in the [aarch64](aarch64) folder
    * [`lower.c`](aarch64/lower.c) contains the code that actually lowers the IR to assembly
    * [`emitter.c`](aarch64/emitter.c) contains the functions that abstract over the AArch64 machine code
* Object file building
  * Writes the object file to disk
  * Currently only macOS Mach-O format supported
  * Code is [`macos/mach-o.h`](macos/mach-o.h) and [`macos/mach-o.c`](macos/mach-o.c)
* Linking
  * Links using the platform linker
  * Effectively just runs the linker as one would from the command line
  * Code is [`link.h`](link.h) and [`link.c`](link.c)
  
