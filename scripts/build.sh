#!/usr/bin/env bash

# the build environment of this script expects both bash and cmake
# minimal builds, requiring only sh and a C compiler, can be achieved via install.sh

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

    # if already configured, use current generator
    if [ -f build/CMakeCache.txt ]; then
      generator=$(grep -Eo 'CMAKE_GENERATOR:INTERNAL=.*$' build/CMakeCache.txt | sed 's/^.*=//' 2>/dev/null)
    fi

    if [ -z "$generator" ]; then
        # else, default to Ninja, fallback to Make
        generator="$(command -v Ninja &>/dev/null && echo "Ninja" || echo "Unix Makefiles")"
        generator="Unix Makefiles"
    fi

    while [[ $# -gt 0 ]]; do
      case "$1" in
        --help|-h|h|help)
          help
          exit 0
          ;;
        --profile-build)
          profile_build="1"
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
    echo -e "${BOLD}    mode=$mode${RESET}"
    echo -e "${BOLD}    generator=$generator${RESET}"
    if [ -n "$profile_build" ]; then
    echo -e "${BOLD}    profile_build=true"
    fi
    if [ -n "$default_target" ]; then
        echo -e "${BOLD}    default_target=$default_target${RESET}"
    fi
    echo -e ""

    flags=""

    # ninja causes colours to get dropped, so force them
    flags="$flags -fdiagnostics-color=always"

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
    if ! (cmake -G "$generator" -DCMAKE_C_FLAGS="$flags" -DCMAKE_BUILD_TYPE=$mode .. >/dev/null); then
        echo -e "${BOLDRED}Configuring build failed!${RESET}"
        exit -1
    fi
}

build() {
    configure "$@"

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
