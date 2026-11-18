#!/usr/bin/env bash
# Runs every challenge's test suite. Requires GHC on PATH.
# Each test file is loaded together with its exN.hs source, so this works on
# case-sensitive file systems too (the modules are named Ex1..Ex6 but the
# files are lower-case). Exits non-zero if any test prints FAIL or a suite
# fails to load.
set -o pipefail
cd "$(dirname "$0")"
status=0
for t in tests/TestEx*.hs; do
  n="${t#tests/TestEx}"; n="${n%.hs}"
  out="$(ghc -ignore-dot-ghci -e main "$t" "ex$n.hs" 2>&1)" || status=1
  printf '%s\n' "$out"
  if printf '%s\n' "$out" | grep -q 'FAIL'; then status=1; fi
done
if [ "$status" -eq 0 ]; then echo "All test suites passed."; else echo "Some tests FAILED." >&2; fi
exit "$status"
