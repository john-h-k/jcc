cmake_minimum_required(VERSION 3.13)
project(jcc C)
set(CMAKE_C_STANDARD 11)

set(CMAKE_COMPILE_WARNING_AS_ERROR ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(
  SOURCE_FILES
  main.c

  vector.c
  alloc.c
  log.c
  graphwriter.c

  compiler.c
  diagnostics.c
  lex.c
  parse.c
  preproc.c
  program.c
  link.c
  lower.c
  liveness.c
  disasm.c
  codegen.c
  hashtbl.c
  bitset.c
  var_table.c
  typechk.c
  lsra.c
  graphcol.c
  
  ir/ir.c
  ir/build.c
  ir/var_refs.c
  ir/eliminate_phi.c
  ir/prettyprint.c

  aarch64.c
  aarch64/codegen.c
  aarch64/lower.c
  aarch64/emit.c
  aarch64/emitter.c

  # riscv.c
  # riscv/codegen.c
  # riscv/lower.c
  # riscv/emit.c
  # riscv/emitter.c

  # eep.c
  # eep/codegen.c
  # eep/lower.c
  # eep/emit.c
  # eep/emitter.c
  # eep/object.c
  # eep/disasm.c

  macos/mach-o.c
  macos/mach-o.c
)

if(MSVC)
  add_compile_option(/W4 /WX)
else()
  add_link_options(-fsanitize=address,undefined)
  add_compile_options(-fsanitize=address,undefined)
  add_compile_options(
    -Wall
    -Wextra
    -Wpedantic
    -Weverything
    -Werror

    -Wno-error=unused-parameter

    -Wmissing-prototypes
    -Wsign-conversion
    -Wimplicit-fallthrough
    -Wstrict-prototypes
    -Wmissing-field-initializers

    -Wno-missing-noreturn
    -Wno-unused-macros
    -Wno-switch-enum
    -Wno-bad-function-cast
    -Wno-undef
    -Wno-padded
    -Wno-declaration-after-statement
    -Wno-class-varargs
    -Wno-sign-conversion
    -Wno-conditional-uninitialized
    -Wno-shorten-64-to-32
    -Wno-assign-enum
  )
  # TODO: remove binary literals in source
  add_compile_options(-Wno-gnu-binary-literal)
endif()

add_executable(jcc ${SOURCE_FILES})

