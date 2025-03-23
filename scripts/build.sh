#!/usr/bin/env bash

# the build environment of this script expects both bash and cmake
# minimal builds, requiring only sh and a C compiler, can be achieved via install.sh

source ./scripts/profile.sh

clean-all() {
    # nukes entire build directory
    # occasionally useful

    echo -e "${BOLD}Cleaning..." 1>&2

    mkdir -p build
    rm -rf build/*

    echo -e "${BOLD}Done!\n" 1>&2
}

clean() {
    echo -e "${BOLD}Cleaning..." 1>&2

    cd build
    cmake --build . --target clean
    cd - > /dev/null

    echo -e "${BOLD}Done!\n" 1>&2
}

_get_generator() {
    # if already configured, use current generator
    if [ -f build/CMakeCache.txt ]; then
        grep -Eo 'CMAKE_GENERATOR:INTERNAL=.*$' build/CMakeCache.txt | sed 's/^.*=//' 2>/dev/null
    else
        # else, default to Ninja, fallback to Make
        command -v Ninja &>/dev/null && echo "Ninja" || echo "Unix Makefiles"
    fi
}

configure() {
    help() {
        echo "JCC configure"
        echo "John Kelly <johnharrykelly@gmail.com>"
        echo ""
        echo "jcc.sh configure [-h|--help] OPTIONS"
        echo ""
        echo "OPTIONS:"
        echo "    -G "
        echo "        CMake Generator to use (default: 'Ninja' if installed, else 'Unix Makefiles')"
        echo ""
        echo "    --clean "
        echo "        Clean artifacts"
        echo ""
        echo "    --clean-all "
        echo "        Completely clean all artifacts, including CMakeCache"
        echo ""
        echo "    -t, --default-target "
        echo "        Set the default target for the build (equivalent to providing '-target <DEFAULT>' on every invocation)"
        echo ""
        echo "    --enable-arch "
        echo "        Enable architecture"
        echo ""
        echo "    --no-san "
        echo "        Disable sanitisers in debug mode - can speedup build and testing"
        echo ""
        echo "    --cc "
        echo "        C compiler to use"
        echo ""
        echo "    -m, --mode"
        echo "        Mode to build (default: 'Debug'). Values:"
        echo "            * d | D | deb   | debug          | Debug           - Debug"
        echo "            * r | R | rel   | release        | Release         - Release"
        echo "            * rd| RD| reldeb| relwithdebinfo | RelWithDebInfo  - Release with debug info"
        echo ""
        echo "    --profile-build "
        echo "        Enable '-ftime-trace', output traces to \$BUILD_DIR/traces, and a unified trace to \$BUILD_DIR/trace.json"
        echo ""

        if ! command -v bat &>/dev/null; then
           echo -e "${BOLD}Install 'bat' for syntax highlighting${RESET}"
        fi

        echo ""
    }
    
    mode="Debug"
    default_target=""
    profile_build=""

    generator=""
    archs=""
    no_san=""
    cc=""
    fresh=""

    if [ -z "$generator" ]; then
        generator="$(_get_generator)"
    fi

    while [[ $# -gt 0 ]]; do
      case "$1" in
        --help|-h|h|help)
          help
          exit 0
          ;;
        --clean)
              clean
          shift
          fresh="--fresh"
          ;;
        --clean-all)
          clean-all
          shift
          fresh="--fresh"
          ;;
        --no-san)
          no_san="1"
          shift
          ;;
        --enable-arch)
          shift
          [ -z "$archs" ] && archs="$1" || archs="$archs;$1"
          shift
          ;;
        --profile-build)
          profile_build="1"
          shift
          ;;
        --cc)
          shift
          cc="$1"
          shift
          ;;
        --cflags)
          shift
          cflags="$1"
          shift
          ;;
        -t|--default-target)
          shift
          default_target="$1"
          shift
          ;;
        --clean)
          clean
          shift
          ;;
        --clean-all)
          clean-all
          shift
          ;;
        -m|--mode)
          shift
          mode=$(get_mode "$1")

          if [ -z "$mode" ]; then
            echo -e "${BOLDRED}Unrecognised mode '$1'!${RESET}"
          fi

          shift
          ;;
        -G)
          shift
          generator="$1"
          shift
          ;;
        *)
          echo "Unknown argument: $1"
          exit 1
          ;;
      esac
    done

    mkdir -p build

    echo -e "${BOLD}Build configuration: ${RESET}"
    if [ -n "$cc" ]; then
        echo -e "${BOLD}    cc=$cc${RESET}"
    fi
    if [ -n "$cflags" ]; then
        echo -e "${BOLD}    cflags=$cflags${RESET}"
    fi
    echo -e "${BOLD}    mode=$mode${RESET}"
    echo -e "${BOLD}    generator=$generator${RESET}"
    if [ -n "$arches" ]; then
        echo -e "${BOLD}    architectures=$archs"
    fi
    if [ -n "$profile_build" ]; then
        echo -e "${BOLD}    profile_build=true"
    fi
    if [ -n "$default_target" ]; then
        echo -e "${BOLD}    default_target=$default_target${RESET}"
    fi
    echo -e ""

    flags="$cflags"

    # ninja causes colours to get dropped, so force them
    if ! [[ "$cc" == *"jcc"* ]]; then
        flags="$flags -fdiagnostics-color=always"
    fi

    if [ -z "$cc" ]; then
        # if ASan on try and use clang
        if [[ "$mode" == "Debug" ]] && [ -z "$no_san" ] && command -v clang &>/dev/null; then
            cc="clang"
        else
            cc="cc"
        fi
    fi

    if [ -n "$profile_build" ]; then
        mkdir -p build/traces
        flags="$flags -ftime-trace=traces"
    fi

    if [ -n "$default_target" ]; then
        # ideally we would statically validate this is a valid target but not sure if there is a way to do that
        # without just hard coding the values here
        echo -e "${BOLD}Building with JCC_DEFAULT_TARGET=$default_target${RESET}" 1>&2

        flags="$flags -DJCC_DEFAULT_TARGET=\"$default_target\""
    fi

    cd build

    no_san_flag=$( [ -n "$no_san" ] && echo "-DNO_SAN=1" || echo "" )
    if ! (cmake $fresh $no_san_flag -DARCHITECTURES="$archs" -DCMAKE_C_COMPILER="$cc" -G "$generator" -DCMAKE_C_FLAGS="$flags" -DCMAKE_BUILD_TYPE=$mode ..); then
        echo -e "${BOLDRED}Configuring build failed!${RESET}"
        exit -1
    fi
}

build() {
    if ! configure "$@"; then
        echo -e "${BOLDRED}Configure failed!${RESET}"
    fi

    num_procs=$(nproc 2> /dev/null || sysctl -n hw.physicalcpu 2> /dev/null || { echo -e "${BOLDYELLOW}Could not find core count; defaulting to 4${RESET}" >&2; echo 4; }; )

    if ! cmake --build . --parallel $num_procs; then
        echo -e "${BOLDRED}Build failed!${RESET}"
        exit -1
    fi

    # we are now in build dir (should make this clearer)
    if [ -d traces ]; then
        jq -s '{traceEvents: map(.traceEvents) | add}' traces/*.json > trace.json
    fi
    
    # if [ -n "$default_target" ]; then
    #     # verifies --default-target
    #     MallocNanoZone=0 ./jcc --version >/dev/null
    # fi

    echo -e "${BOLD}Build complete${RESET}"  1>&2

    cd - > /dev/null
}

mini-bootstrap() {
    build --clean --mode rel
    # build --clean

    flags="-DJCC_ALL"
    # doesn't support -D yet
    jcc_flags=""

    clang_files=(
        src/aarch64/codegen.c
        src/aarch64/emit.c
        src/aarch64/emitter.c
        src/aarch64/lower.c
        src/aarch64.c
        src/alloc.c
        src/args.c
        src/bitset.c
        src/builtins.c
        src/codegen.c
        src/compiler.c
        src/deque.c
        src/diagnostics.c
        src/disasm.c
        src/eep/codegen.c
        src/eep/disasm.c
        src/eep/emit.c
        src/eep/emitter.c
        src/eep/lower.c
        src/eep/object.c
        src/eep.c
        # src/graphcol.c
        # src/graphwriter.c
        # src/hash.c
        # src/hashtbl.c
        # src/io.c
        # src/ir/build.c
        # src/ir/eliminate_phi.c
        # src/ir/ir.c
        src/ir/prettyprint.c
        src/ir/rw.c
        src/ir/validate.c
        src/ir/var_refs.c
        src/lex.c
        src/linux/elf.c
        src/linux/link.c
        src/liveness.c
        src/log.c
        src/lower.c
        src/lsra.c
        src/macos/link.c
        src/macos/mach-o.c
        src/main.c
        src/opts/cnst_branches.c
        src/opts/cnst_fold.c
        src/opts/inline.c
        src/opts/instr_comb.c
        src/opts/opts.c
        src/opts/promote.c
        src/parse.c
        src/ap_val.c
        src/preproc.c
        src/profile.c
        src/program.c
        src/rv32i/codegen.c
        src/rv32i/emit.c
        src/rv32i/emitter.c
        src/rv32i/lower.c
        src/rv32i/object.c
        src/rv32i.c
        src/typechk.c
        src/util.c
        src/var_table.c
        src/vector.c
        src/x64/codegen.c
        src/x64/emit.c
        src/x64/emitter.c
        src/x64/lower.c
        src/x64.c
    )

    tmpdir=$(mktemp -d)
    trap "rm -rf $tmpdir" EXIT

    objects=()
    for file in src/**/*.c src/*.c; do
        jcc="1"

        for other in "${clang_files[@]}"; do
            if [[ "$(realpath $other)" == "$(realpath $file)" ]]; then
                jcc=""
                break
            fi
        done

        filename="$(basename "$file")"
        output="$(mktemp -p $tmpdir)_$filename.o"

        if [ -n "$jcc" ]; then
            echo "Using JCC for $file"
            # jcc $flags "$file" -o "$tmpdir/$filename.o"
            # use preproc seperately because it breaks for some things in combined mode
            if ! build/jcc1 $jcc_flags "$file" -E > "$tmpdir/$filename.c"; then
                echo "preproc fail"
                exit -1
            fi

            if ! build/jcc1 $jcc_flags "$tmpdir/$filename.c" -c -o "$output"; then
                echo "jcc fail"
                exit -1
            fi

            objname="build/$filename.o"
            mkdir -p "$(dirname "$objname")"
            cp "$output.o" "$objname"

        else
            cc $flags "$file" -c -o "$output"
        fi

        objects+=("$output")
    done

    # cc $flags -DJCC_ALL -o build/jcc "${objects[@]}" -lm
    cp build/jcc build/jcc0
    rm build/jcc
    build/jcc0 -o build/jccm1 "${objects[@]}"
}

bootstrap() {
    stage="${1:-3}"

    _stage_build() {
        cc="$1"
        shift
        n="$1"
        shift
        flags="$@"

        # do a pre-configure so timing is more accurate
        configure "$@" --clean --enable-arch aarch64 --enable-arch rv32i --cc "$cc" > /dev/null
        cd ..

        start=$(profile_begin)

        # known issues with x64 emitter file; only build for aarch64/rv32i
        if ! build "$@" --enable-arch aarch64 --enable-arch rv32i --cc "$cc" > /dev/null; then
            echo -e "${BOLDRED}stage$n build fail${RESET}"
            exit 1
        fi

        cc_name=$(basename "$cc")
        profile_end "stage$n (cc=$cc_name) took " $start

        cp build/jcc "build/jcc$n"

        echo -e "${BOLD}stage$n built to 'jcc$n'${RESET}"
    }

    rm -f jcc0 jcc1

    if ! _stage_build cc 0 --mode release; then
        exit -1
    fi

    [ "$stage" -gt 0 ] || return 0

    if ! _stage_build $(pwd)/build/jcc0 1; then
        exit -1
    fi

    [ "$stage" -gt 1 ] || return 0

    if ! _stage_build $(pwd)/build/jcc1 2; then
        exit -1
    fi

    if command diff build/jcc1 build/jcc2; then
        echo -e "${BOLDGREEN}jcc1 and jcc2 are the same!${RESET}"
    else
        echo -e "${BOLDRED}Differences found between jcc1 and jcc2!${RESET}"
    fi

    [ "$stage" -gt 2 ] || return 0

    if ! _stage_build $(pwd)/build/jcc2 3; then
        exit -1
    fi

    if command diff build/jcc2 build/jcc3; then
        echo -e "${BOLDGREEN}jcc2 and jcc3 are the same!${RESET}"
    else
        echo -e "${BOLDRED}Differences found between jcc2 and jcc3!${RESET}"
    fi

    echo -e "${BOLD}Done!${RESET}"

    # build rv32i native compiler
    # if ! cmake "${cmake_flags[@]}" .. -DCMAKE_C_FLAGS="-target rv32i-unknown-elf -isysroot /opt/riscv -isystem /opt/riscv/riscv64-unknown-elf/include" -DCMAKE_C_COMPILER=$(pwd)/jcc3 >/dev/null; then
    #     echo -e "${BOLDRED}risc-v configure fail${RESET}"
    #     exit -1
    # fi

    # if ! cmake --build . --parallel 1; then
    #     echo -e "${BOLDRED}risc-v build fail${RESET}"
    #     exit -1
    # fi

    # cp jcc jcc-rv32i
}
