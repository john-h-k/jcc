#!/usr/bin/env bash

_register_sub_fn benchmark

benchmark-parse() {
    _benchmark '-fparse-only -O0 -g0 --no-warnings' "$@"
}

benchmark-syntax() {
    _benchmark '-fsyntax-only -O0 -g0 --no-warnings' "$@"
}

benchmark-lex() {
    _benchmark '-flex-only -O0 -g0 --no-warnings' "$@"
}

benchmark-compile() {
    _benchmark '-O0 -g0 --no-warnings' "$@"
}

_benchmark() {
    bench_flags="$1"
    shift
    sources="$@"

    mkdir -p .tmp

    _mk_sqlite3() {
        [ -d .tmp/sqlite3 ] && return 0

        echo -e "${BOLD}Downloading sqlite3 amalgamation...${RESET}"
        curl -sSL 'https://www.sqlite.org/2025/sqlite-amalgamation-3490100.zip' > .tmp/sqlite3.zip
        unzip .tmp/sqlite3.zip -d .tmp/sqlite3

        clang -E -P -fno-blocks .tmp/sqlite3/sqlite-amalgamation-3490100/sqlite3.c -o .tmp/sqlite3/sqlite-amalgamation-3490100/sqlite3.i
    }

    _mk_huge() {
        # max funcs seems to rapidly increase compile time (20 takes >100x as long to run as 10)
        max_funcs=10
        csmith --max-funcs $max_funcs > .tmp/huge.c
    }

    if [ -z "$sources" ]; then
        # _mk_huge
        # sources="$sources,.tmp/huge.c"

        _mk_sqlite3
        sources="$SCRIPT_DIR/.tmp/sqlite3/sqlite-amalgamation-3490100/sqlite3.c"
    fi

    clean-all
    build --mode release --clean

    compilers="clang"
    compilers="$compilers,$(pwd)/build/jcc"

    output="$(pwd)/.tmp/benchmark_parse.md"
    warmup=10
    min=10

    cd "$CALLER_DIR"

    hyperfine \
        --ignore-failure \
        --export-markdown $output \
        --warmup $warmup \
        --min-runs $min \
        --parameter-list compiler "$compilers" \
        --parameter-list source "$sources" \
        --shell none \
        --command-name '{compiler} - {source}' \
        "{compiler} $bench_flags -c -std=c2x -I ../csmith-jcc/runtime {source}"

    has_tool glow && glow $output || cat $output 

    cd - &> /dev/null
}
