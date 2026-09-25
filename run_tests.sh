#!/bin/bash
# Run the Dolphin test corpus using rescue mode.
# Usage: ./run_tests.sh

set -uo pipefail

COMPILER="./_build/default/bin/main.exe"
POSITIVE_DIR="test/tests/positive"
NEGATIVE_DIR="test/tests/negative"

pass=0
fail=0

run_positive() {
    local f="$1"
    if "$COMPILER" rescue --phase 1 "$f" > /dev/null 2>&1; then
        printf "  %-50s \033[32mOK\033[0m\n" "$f"
        pass=$((pass + 1))
    else
        printf "  %-50s \033[31mFAILED\033[0m\n" "$f"
        fail=$((fail + 1))
    fi
}

run_negative() {
    local f="$1"
    if "$COMPILER" rescue --phase 1 "$f" > /dev/null 2>&1; then
        printf "  %-50s \033[31mUNEXPECTED OK\033[0m\n" "$f"
        fail=$((fail + 1))
    else
        printf "  %-50s \033[32mOK (rejected)\033[0m\n" "$f"
        pass=$((pass + 1))
    fi
}

echo "Building..."
dune build
echo ""

echo "=== POSITIVE TESTS ==="
for f in "$POSITIVE_DIR"/*.dlp; do
    run_positive "$f"
done

echo ""
echo "=== NEGATIVE TESTS ==="
for f in "$NEGATIVE_DIR"/*.dlp; do
    run_negative "$f"
done

echo ""
echo "========================================"
echo "Results: $pass passed, $fail failed out of $((pass + fail)) tests"
if [ "$fail" -eq 0 ]; then
    echo -e "\033[32mAll tests passed!\033[0m"
else
    echo -e "\033[31m$fail test(s) failed.\033[0m"
    exit 1
fi
