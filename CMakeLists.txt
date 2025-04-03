cmake_minimum_required(VERSION 3.13)
project(jcc C)
set(CMAKE_C_STANDARD 11)

set(CMAKE_COMPILE_WARNING_AS_ERROR ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(
  SOURCE_FILES
  src/main.c
  src/args.c

  src/util.c
  src/vector.c
  src/hashtbl.c
  src/deque.c
  src/hash.c
  src/bitset.c
  src/fcache.c
  src/json.c

  src/profile.c

  src/alloc.c
  src/log.c
  src/graphwriter.c

  src/ap_val.c

  src/lsp/lsp.c
  src/lsp/lsp_types.c

  src/compiler.c
  src/diagnostics.c
  src/builtins.c
  src/lex.c
  src/io.c
  src/parse.c
  src/preproc.c
  src/program.c
  src/lower.c
  src/liveness.c
  src/disasm.c
  src/codegen.c
  src/var_table.c
  src/typechk.c
  src/lsra.c
  src/graphcol.c
  src/
  src/ir/ir.c
  src/ir/build.c
  src/ir/var_refs.c
  src/ir/eliminate_phi.c
  src/ir/prettyprint.c
  src/ir/validate.c
  src/ir/rw.c

  src/opts/opts.c
  src/opts/cnst_fold.c
  src/opts/cnst_branches.c
  src/opts/instr_comb.c
  src/opts/promote.c
  src/opts/inline.c

  src/aarch64.c
  src/x64.c
  src/rv32i.c

  src/macos/mach-o.c
  src/macos/link.c

  src/linux/elf.c
  src/linux/link.c
)

# build for all platforms (support cross compilation)
# TODO: add a more granular system for including targets

if("${CMAKE_C_COMPILER_ID}" STREQUAL "")
    set(CMAKE_C_COMPILER_ID "JCC" CACHE STRING "Custom compiler ID" FORCE)
endif()

if(NOT DEFINED ARCHITECTURES OR "${ARCHITECTURES}" STREQUAL "")
  set(ARCHITECTURES "aarch64;rv32i;x64")

  if(NOT CMAKE_C_COMPILER_ID STREQUAL "JCC")
    add_definitions(-DJCC_ALL)
  endif()
endif()

foreach(arch IN LISTS ARCHITECTURES)
  if("${arch}" STREQUAL "aarch64")
    if(NOT CMAKE_C_COMPILER_ID STREQUAL "JCC")
      add_definitions(-DJCC_AARCH64)
    endif()

    list(APPEND SOURCE_FILES
      src/aarch64/codegen.c
      src/aarch64/lower.c
      src/aarch64/emit.c
      src/aarch64/emitter.c
    )
  elseif("${arch}" STREQUAL "x64")
    if(NOT CMAKE_C_COMPILER_ID STREQUAL "JCC")
      add_definitions(-DJCC_X64)
    endif()

    list(APPEND SOURCE_FILES
      src/x64/codegen.c
      src/x64/lower.c
      src/x64/emit.c
      src/x64/emitter.c
    )
  elseif("${arch}" STREQUAL "rv32i")
    if(NOT CMAKE_C_COMPILER_ID STREQUAL "JCC")
      add_definitions(-DJCC_RV32I)
    endif()

    list(APPEND SOURCE_FILES
      src/rv32i/codegen.c
      src/rv32i/lower.c
      src/rv32i/emit.c
      src/rv32i/emitter.c
      src/rv32i/object.c
    )
  else()
    message(FATAL_ERROR "Unsupported architecture: ${arch}")
  endif()
endforeach()

# currently unused
# eep.c
# eep/codegen.c
# eep/lower.c
# eep/emit.c
# eep/emitter.c
# eep/object.c
# eep/disasm.c

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  message(STATUS "Debug build detected")

  if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
    message(WARNING "Debug mode may be VERY SLOW on GCC due to ASan problems - Clang w/ ASan is significantly faster")
  endif()
  

  if(NOT DEFINED NO_SAN OR NO_SAN STREQUAL "")
    if(NOT CMAKE_C_COMPILER_ID STREQUAL "JCC")
      add_link_options(-fsanitize=address,undefined)
      add_compile_options(-fsanitize=address,undefined -fno-sanitize-recover=all)

      add_compile_options(-g)
      add_link_options(-g3 -ldl)
      add_link_options(-rdynamic)
    endif()
  else()
    message(STATUS "Sanitisers disabled")
  endif()
endif()

if(MSVC)
  add_compile_option(/W4 /WX)
elseif(CMAKE_C_COMPILER_ID STREQUAL "JCC")
elseif(CMAKE_C_COMPILER_ID STREQUAL "AppleClang")
  message(STATUS "Building with AppleClang")

  add_compile_options(
    -Wall
    -Wextra
    -Wpedantic
    -Weverything

    # -Wformat-security
    # -Wformat
    # -Wformat=2
    # -Wconversion
    -Wimplicit-fallthrough
    -Wswitch

    # -Wimplicit
    # -Wincompatible-pointer-types
    # -Wint-conversion

    -Wno-error=unreachable-code

    -Wno-switch-default
    -Wno-pre-c11-compat
    -Wno-c23-extensions

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
