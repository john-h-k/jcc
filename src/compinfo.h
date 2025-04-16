#ifndef COMPINFO_H
#define COMPINFO_H

#ifdef __cplusplus
#error "do not compile jcc as C++"
#endif

#define JCC_VERSION "0.0.1"
#define JCC_VERSION_INT 1

// information about the compilation of JCC, e.g arch it was built for, OS, C
// version

/****************************** Utility macros ******************************/

#ifdef __has_feature
#define HAS_FEATURE(name) __has_feature(name)
#else
#define HAS_FEATURE(name) 0
#endif

#ifdef __has_builtin
#define HAS_BUILTIN(name) __has_builtin(name)
#else
#define HAS_BUILTIN(name) 0
#endif

#ifdef __has_attribute
#define HAS_ATTRIBUTE(name) __has_attribute(name)
#else
#define HAS_ATTRIBUTE(name) 0
#endif

#ifdef __has_c_attribute
#define HAS_C_ATTRIBUTE(name) __has_c_attribute(name)
#else
#define HAS_C_ATTRIBUTE(name) 0
#endif

/******************************** C Standard ********************************/

#define STDC23 202311L
#define STDC18 201710L
#define STDC11 201112L

#define STDC_MIN(v) __STDC_VERSION__ >= (v)
#define STDC_MIN_23 STDC_MIN(STDC23)

#if __STDC_VERSION__ < STDC11
#define EXPAND_INNER(x) "jcc only supports C11 or later (__STDC_VERSION__ is '" #x "')"
#define EXPAND(x) EXPAND_INNER(x)
#pragma message(EXPAND(__STDC_VERSION__))
#error "C11 compiler requirement not met"
#undef EXPAND
#undef EXPAND_INNER
#endif

/********************************* Compiler *********************************/

#ifdef UTIL_MACRO_DEBUG

#if __JCC__
#pragma message "Compiler is JCC"
#elif __clang__
#pragma message "Compiler is clang"
#elif __GNUC__
#pragma message "Compiler is GCC"
#else
#pragma message "unrecognised compiler"
#endif

#if __STDC_VERSION__ > STDC23
#pragma message "C version is unknown, but newer than C23"
#elif __STDC_VERSION__ > STDC18
#pragma message "C version is C23"
#elif __STDC_VERSION__ > STDC18
#pragma message "C version is C18"
#else
#pragma message "C version is C11"
#endif

#endif

/******************************** Sanitizers ********************************/

#if HAS_FEATURE(memory_sanitizer) || defined(MEMORY_SANITIZER) ||              \
    defined(__SANITIZE_MEMORY__)
#define MSAN 1
#endif

#if HAS_FEATURE(address_sanitizer) || defined(ADDRESS_SANITIZER) ||            \
    defined(__SANITIZE_ADDRESS__)
#define ASAN 1
#endif

#if HAS_FEATURE(leak_sanitizer) || defined(LEAK_SANITIZER) ||            \
    defined(__SANITIZE_LEAK__)
#define LSAN 1

void __lsan_ignore_object(const void* p);
#define LSAN_IGNORE(p) __lsan_ignore_object(p)
#else
#define LSAN_IGNORE(p) ((void)(p))
#endif

#if HAS_FEATURE(hwaddress_sanitizer) || defined(HWADDRESS_SANITIZER) ||        \
    defined(__SANITIZE_HWADDRESS__)
#define HWASAN 1
#endif

#if HAS_FEATURE(thread_sanitizer) || defined(THREAD_SANITIZER) ||              \
    defined(__SANITIZE_THREAD__)
#define TSAN 1
#endif

// NOTE: for reasons, there is no __SANITIZE_UNDEFINED__, and so we cannot
// detect this on GCC
#if HAS_FEATURE(undefined_behavior_sanitizer) ||                               \
    defined(UNDEFINED_BEHAVIOR_SANITIZER)
#define UBSAN 1
#else
#define UBSAN 0
#endif

#if MSAN || ASAN || LSAN || HWASAN || TSAN
#define SAN 1
#endif

/************************************ OS ************************************/

#if defined(__APPLE__) || defined(__MACH__)
#define OS_APPLE 1
// needs to be darwin to match `uname`
#define OS_NAME "darwin"
#elif defined(__linux__)
#define OS_LINUX 1
#define OS_NAME "linux"
#elif defined(__FreeBSD__)
#define OS_FREEBSD 1
#define OS_NAME "FreeBSD"
#elif defined(unix) || defined(__unix__) || defined(__unix)
#define OS_UNIX 1
#define OS_NAME "unix"
#elif defined(_WIN32) || defined(_WIN64) || defined(__CYGWIN__)
#define OS_WINDOWS 1
#define OS_NAME "windows"
#else
#define OS_NAME "<Unknown OS>"
#endif

/*********************************** Arch ***********************************/

#if defined(__aarch64__)
#define ARCH_AARCH64 1
#define ARCH_NAME "aarch64"
#elif defined(__x86_64__)
#define ARCH_X86_64 1
#define ARCH_NAME "x86_64"
#elif defined(__riscv__) || defined(__riscv) || defined(__riscv32)
#define ARCH_RISCV32 1
#define ARCH_NAME "rv32i"
#else
#define ARCH_NAME "<Unknown architecture>"
#endif

#endif
