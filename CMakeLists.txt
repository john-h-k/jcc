cmake_minimum_required(VERSION 3.13)
project(jcc C)
set(CMAKE_C_STANDARD 11)

set(CMAKE_COMPILE_WARNING_AS_ERROR ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(
  SOURCE_FILES
  main.c
  args.c

  vector.c
  hashtbl.c
  deque.c
  hash.c
  bitset.c

  profile.c

  alloc.c
  log.c
  graphwriter.c

  compiler.c
  diagnostics.c
  builtins.c
  lex.c
  io.c
  parse.c
  preproc.c
  program.c
  lower.c
  liveness.c
  disasm.c
  codegen.c
  var_table.c
  typechk.c
  lsra.c
  graphcol.c
  
  ir/ir.c
  ir/build.c
  ir/var_refs.c
  ir/eliminate_phi.c
  ir/prettyprint.c
  ir/validate.c
  ir/rw.c

  opts/opts.c
  opts/cnst_fold.c
  opts/cnst_branches.c
  opts/instr_comb.c
  opts/promote.c
  opts/inline.c

  aarch64.c
  aarch64/codegen.c
  aarch64/lower.c
  aarch64/emit.c
  aarch64/emitter.c

  x64.c
  x64/codegen.c
  x64/lower.c
  x64/emit.c
  x64/emitter.c

  rv32i.c
  rv32i/codegen.c
  rv32i/lower.c
  rv32i/emit.c
  rv32i/emitter.c
  rv32i/object.c

  # eep.c
  # eep/codegen.c
  # eep/lower.c
  # eep/emit.c
  # eep/emitter.c
  # eep/object.c
  # eep/disasm.c

  macos/mach-o.c
  macos/link.c

  linux/elf.c
  linux/link.c
)

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  message(STATUS "Debug build detected")

  if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
    message(WARNING "Debug mode may be VERY SLOW on GCC due to ASan problems - Clang w/ ASan is significantly faster")
  endif()
  
  add_link_options(-fsanitize=address,undefined)
  add_compile_options(-fsanitize=address,undefined -fno-sanitize-recover=all)

  add_compile_options(-g)
  add_link_options(-g3 -ldl)
  add_link_options(-rdynamic)
endif()

if(MSVC)
  add_compile_option(/W4 /WX)
elseif(CMAKE_C_COMPILER_ID STREQUAL "AppleClang")
  message(STATUS "Building with AppleClang")

  add_compile_options(
    -Wall
    -Wextra
    -Wpedantic
    -Weverything

    -Wno-error=unreachable-code

    -Wzero-as-null-pointer-constant
    -Wbool-conversion

    -Wmissing-prototypes
    -Wsign-conversion
    -Wimplicit-fallthrough
    -Wstrict-prototypes
    -Wmissing-field-initializers

    # i would _like_ to enable this, but it causes errors everywhere flag enums are used
    -Wno-sign-conversion

    -Wno-missing-noreturn
    -Wno-unused-macros
    -Wno-switch-enum
    -Wno-bad-function-cast
    -Wno-undef
    -Wno-padded
    -Wno-declaration-after-statement
    -Wno-class-varargs
    -Wno-conditional-uninitialized
    -Wno-shorten-64-to-32
    -Wno-assign-enum
    # TODO: remove binary literals in source
    -Wno-gnu-binary-literal
  )
elseif(CMAKE_C_COMPILER_ID STREQUAL "Clang")
  # nothing for now
elseif(CMAKE_C_COMPILER_ID STREQUAL "GNU")
  # nothing for now
else()
  message(WARNING "Unknown compiler: ${CMAKE_C_COMPILER_ID}")
endif()

add_executable(jcc ${SOURCE_FILES})
target_link_libraries(jcc m)

