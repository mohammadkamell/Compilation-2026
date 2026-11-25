#!/bin/bash
# Runs the example programs provided with the course (test/tests/examples) and
# compares their output with the .out files. A .out file contains the program's
# standard output followed by its exit code.
# Usage: bash run_examples.sh              (our own frontend)
#        MODE=rescue bash run_examples.sh  (reference parser)

MODE="${MODE:-compile}"
COMPILER="./_build/default/bin/main.exe"
DIR="test/tests/examples"
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
pass=0; fail=0

dune build || { echo "Build failed"; exit 1; }

echo ""
echo "=== EXAMPLE PROGRAMS (provided with the course, $DIR, mode: $MODE) ==="

for f in "$DIR"/*.dlp; do
  base="${f%.dlp}"
  name=$(basename "$base")
  if ! "$COMPILER" "$MODE" --phase 5 "$f" > "$TMP/out.ll" 2> "$TMP/err"; then
    printf "  %-20s \033[31mFAILED (compile)\033[0m  %s\n" "$name" "$(head -n 1 "$TMP/err")"
    fail=$((fail+1)); continue
  fi
  if ! clang "$TMP/out.ll" src/runtime/runtime.c src/runtime/stdlib.c -o "$TMP/prog" \
         -Wno-override-module 2> "$TMP/err"; then
    printf "  %-20s \033[31mFAILED (clang)\033[0m  %s\n" "$name" "$(head -n 1 "$TMP/err")"
    fail=$((fail+1)); continue
  fi
  timeout 10 "$TMP/prog" < /dev/null > "$TMP/actual" 2> /dev/null
  code=$?
  if [ "$code" -eq 124 ]; then
    printf "  %-20s \033[31mTIMEOUT\033[0m\n" "$name"; fail=$((fail+1)); continue
  fi
  # expected: the .out file; actual: stdout followed by the exit code
  expected=$(cat "$base.out")
  actual=$(cat "$TMP/actual"; echo "$code")
  if [ "$expected" = "$actual" ]; then
    printf "  %-20s \033[32mOK\033[0m\n" "$name"; pass=$((pass+1))
  else
    printf "  %-20s \033[31mWRONG OUTPUT\033[0m\n" "$name"
    diff <(echo "$expected") <(echo "$actual") | head -n 6
    fail=$((fail+1))
  fi
done

echo ""
echo "========================================"
echo "Examples: $pass passed, $fail failed out of $((pass + fail))"
[ "$fail" -eq 0 ]
