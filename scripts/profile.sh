#!/usr/bin/env bash

if command -v gdate &>/dev/null; then
    profile_cmd="gdate"
elif date --version 2>/dev/null | grep -q "GNU coreutils"; then
    profile_cmd="date"
else
    if [ -n "$PROFILE" ]; then
        echo -e "${BOLDRED}GNU date not available, cannot profile. Install coreutils or use gdate.${RESET}" >&2
        exit -1
    fi

    echo -e "${BOLDYELLOW}GNU date not available, not timestamping. Install coreutils or use gdate.${RESET}" >&2
fi

profile_begin() {
    if [ -n "$profile_cmd" ]; then
        $profile_cmd +%s%3N
    fi
}

profile_elapsed() {
    if [ -n "$profile_cmd" ]; then
        end_time=$($profile_cmd +%s%3N)
        echo $((end_time - $2))
    fi
}

profile_end() {
    if [ -n "$profile_cmd" ]; then
        end_time=$($profile_cmd +%s%3N)
        elapsed=$((end_time - $2))

        printf "${BOLD}$1%d.%03ds${RESET}\n" $((elapsed / 1000)) $((elapsed % 1000)) >&2
    fi
}
