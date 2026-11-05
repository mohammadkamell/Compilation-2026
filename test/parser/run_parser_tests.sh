#!/bin/bash
# Compares our parser with the reference parser (dolphin-serialize).
# Positive cases must give the same AST; negative cases must be rejected by both.
# Run from the project root: bash test/parser/run_parser_tests.sh

DRIVER="./_build/default/test/parser/test_parser.exe"
CASES="test/parser/cases"
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
pass=0; fail=0

dune build || { echo "Build failed"; exit 1; }

echo "=== POSITIVE (same AST as reference) ==="
for f in "$CASES"/positive/*.dlp; do
  if ! dolphin-serialize serialize --phase 4 --output "$TMP/ref.json" "$f" >/dev/null 2>&1; then
    printf "  %-55s \033[31mREFERENCE REJECTS\033[0m\n" "$f"; fail=$((fail+1)); continue
  fi
  "$DRIVER" json "$TMP/ref.json" > "$TMP/ref.txt" 2>&1
  "$DRIVER" parse "$f"           > "$TMP/our.txt" 2>&1
  if diff -q "$TMP/ref.txt" "$TMP/our.txt" >/dev/null; then
    printf "  %-55s \033[32mSAME\033[0m\n" "$f"; pass=$((pass+1))
  else
    printf "  %-55s \033[31mDIFF\033[0m\n" "$f"; fail=$((fail+1))
    diff "$TMP/ref.txt" "$TMP/our.txt" | head -n 20
  fi
done

echo ""
echo "=== NEGATIVE (rejected by both) ==="
for f in "$CASES"/negative/*.dlp; do
  dolphin-serialize serialize --phase 4 --output "$TMP/ref.json" "$f" >/dev/null 2>&1
  ref=$?
  "$DRIVER" parse "$f" >/dev/null 2>&1
  our=$?
  if [ "$ref" -ne 0 ] && [ "$our" -eq 1 ]; then
    printf "  %-55s \033[32mOK (both reject)\033[0m\n" "$f"; pass=$((pass+1))
  else
    printf "  %-55s \033[31mMISMATCH\033[0m (reference exit %d, ours exit %d)\n" "$f" "$ref" "$our"
    fail=$((fail+1))
  fi
done

echo ""
echo "Results: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
