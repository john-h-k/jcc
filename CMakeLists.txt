cmake_minimum_required(VERSION 3.13)
project(jcc C)
set(CMAKE_C_STANDARD 23)

set(CMAKE_COMPILE_WARNING_AS_ERROR ON)

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
  link.c
  liveness.c
  disasm.c
  var_table.c
  lsra.c
  graphcol.c
  
  ir/ir.c
  ir/build.c
  ir/var_refs.c
  ir/eliminate_phi.c
  ir/prettyprint.c

  aarch64.c
  aarch64/lower.c
  aarch64/emit.c
  aarch64/emitter.c

  eep.c
  eep/lower.c
  eep/emit.c
  eep/emitter.c
  eep/object.c
  eep/disasm.c

  macos/mach-o.c
  macos/mach-o.c
)

if(MSVC)
  add_compile_option(/W4 /WX)
else()
  add_link_options(-fsanitize=undefined)
  add_compile_options(-fsanitize=undefined)
  add_compile_options(-Wall -Wextra -Wpedantic -Werror -Wimplicit-fallthrough -Wstrict-prototypes)
  # TODO: remove binary literals in source
  add_compile_options(-Wno-gnu-binary-literal)
endif()

add_executable(jcc ${SOURCE_FILES})

