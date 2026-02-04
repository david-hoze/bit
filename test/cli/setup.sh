#!/bin/bash
# test/cli/setup.sh - Common test setup

TEST_DIR="$TEMP/bit-test"

setup_fresh_repo() {
    rm -rf "$TEST_DIR"
    mkdir -p "$TEST_DIR"
    cd "$TEST_DIR"
    bit init >/dev/null 2>&1
}

cleanup() {
    rm -rf "$TEST_DIR"
}
