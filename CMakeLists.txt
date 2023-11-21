cmake_minimum_required(VERSION 3.13)
project(jcc)
set(CMAKE_C_STANDARD 23)

set(
  SOURCE_FILES
  main.c

  vector.c
  alloc.c
  log.c

  compiler.c
  diagnostics.c
  lex.c
  parse.c
  ir.c
  link.c
  disasm.c
  var_table.c
  lsra.c

  passes/eliminate_phi.c

  aarch64/lower.c
  aarch64/emit.c
  aarch64/emitter.c

  macos/mach-o.c
  macos/mach-o.c
)

if(MSVC)
  add_compile_option(/W4 /WX)
else()
  add_compile_options(-Wall -Wextra -Wpedantic -Werror)
  add_compile_options(-Wno-gnu-binary-literal)
endif()

add_executable(jcc ${SOURCE_FILES})

