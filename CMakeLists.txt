cmake_minimum_required(VERSION 3.13)
project(jcc)
set(CMAKE_C_STANDARD 23)

set(SOURCE_FILES main.c lex.c parse.c compiler.c arm64_emitter.c alloc.c macos/mach-o.c)

if(MSVC)
  add_compile_option(/W4 /WX)
else()
  add_compile_options(-Wall -Wextra -Wpedantic -Werror)
  add_compile_options(-Wno-gnu-binary-literal)
endif()

add_executable(jcc ${SOURCE_FILES})

