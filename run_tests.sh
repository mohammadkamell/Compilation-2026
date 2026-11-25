# Runs our own Dolphin test corpus (test/tests/).
# Usage: bash run_tests.sh              (our own frontend)
#        MODE=rescue bash run_tests.sh  (reference parser)

set -uo pipefail

COMPILER="./_build/default/bin/main.exe"
POSITIVE_DIR="test/tests/positive"
NEGATIVE_DIR="test/tests/negative"
ASSIGNMENT5_NEGATIVE_TEST_DIR="test/tests/assignment-05-tests"
ASSIGNMENT7_TEST_DIR="test/tests/assignment-07-tests"
ASSIGNMENT8_TEST_DIR="test/tests/assignment-08-tests"

PHASE=5
MODE="${MODE:-compile}"   # compile = our own frontend, rescue = reference parser

pass=0
fail=0


TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT

run_positive() {
    local f="$1"
    local expected exp_code code actual
    expected=$(sed -n 's|^// Expected output[^:]*: *||p' "$f" | head -n 1 | tr -d '\r')
    exp_code=$(sed -n 's|^// Expected exit code: *||p' "$f" | head -n 1 | tr -d '\r')
    exp_code=${exp_code:-0}

    if ! "$COMPILER" "$MODE" --phase "$PHASE" "$f" > "$TMP/out.ll" 2> /dev/null; then
        printf "  %-50s \033[31mFAILED (compile)\033[0m\n" "$f"; fail=$((fail + 1)); return
    fi
    if ! clang "$TMP/out.ll" src/runtime/runtime.c src/runtime/stdlib.c -o "$TMP/prog" -Wno-override-module 2> /dev/null; then
        printf "  %-50s \033[31mFAILED (clang)\033[0m\n" "$f"; fail=$((fail + 1)); return
    fi

    timeout 5 "$TMP/prog" < /dev/null > "$TMP/actual" 2> /dev/null
    code=$?
    if [ "$code" -eq 124 ]; then
        printf "  %-50s \033[31mTIMEOUT (infinite loop?)\033[0m\n" "$f"; fail=$((fail + 1)); return
    fi

    actual=$(echo $(cat "$TMP/actual"))
    expected=$(echo $expected)
    if [ "$actual" = "$expected" ] && [ "$code" -eq "$exp_code" ]; then
        printf "  %-50s \033[32mOK\033[0m\n" "$f"; pass=$((pass + 1))
    else
        printf "  %-50s \033[31mWRONG\033[0m (expected '%s' exit %s, got '%s' exit %s)\n" \
               "$f" "$expected" "$exp_code" "$actual" "$code"
        fail=$((fail + 1))
    fi
}

run_negative() {
    local f="$1"
    "$COMPILER" "$MODE" --phase "$PHASE" "$f" > /dev/null 2>&1
    local code=$?
    if [ "$code" -eq 1 ]; then
        printf "  %-50s \033[32mOK (rejected)\033[0m\n" "$f"
        pass=$((pass + 1))
    elif [ "$code" -eq 0 ]; then
        printf "  %-50s \033[31mUNEXPECTED OK\033[0m\n" "$f"
        fail=$((fail + 1))
    else
        printf "  %-50s \033[31mCRASH (exit %d)\033[0m\n" "$f" "$code"
        fail=$((fail + 1))
    fi
}

echo "Building..."
dune build || { echo "Build failed"; exit 1; }
echo ""
echo "Mode: $MODE (phase $PHASE)"

echo "=== POSITIVE TESTS ==="
for f in "$POSITIVE_DIR"/*.dlp; do
    run_positive "$f"
done

echo ""
echo "=== NEGATIVE TESTS ==="
for f in "$NEGATIVE_DIR"/*.dlp; do
  [ -e "$f" ] || continue
  run_negative "$f"
done

echo ""
echo "=== ASSIGNMENT 5 (NEGATIVE TESTS) ==="
for f in "$ASSIGNMENT5_NEGATIVE_TEST_DIR"/*.dlp; do
  [ -e "$f" ] || continue
  run_negative "$f"
done

echo ""
echo "=== ASSIGNMENT 7 (PHASE 4 POSITIVE TESTS) ==="
for f in "$ASSIGNMENT7_TEST_DIR"/*.dlp; do
  [ -e "$f" ] || continue
  run_positive "$f"
done

echo ""
echo "=== ASSIGNMENT 8 (PHASE 5 POSITIVE TESTS) ==="
for f in "$ASSIGNMENT8_TEST_DIR"/pos_*.dlp; do
  [ -e "$f" ] || continue
  run_positive "$f"
done

echo ""
echo "=== ASSIGNMENT 8 (PHASE 5 NEGATIVE TESTS) ==="
for f in "$ASSIGNMENT8_TEST_DIR"/neg_*.dlp; do
  [ -e "$f" ] || continue
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
