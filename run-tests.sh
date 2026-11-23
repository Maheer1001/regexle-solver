#!/usr/bin/env bash
# Runs every challenge's test suite plus the integration suite. Requires GHC
# on PATH. Each test file is loaded together with its exN.hs source, so this
# works on case-sensitive file systems too (the modules are named Ex1..Ex6 but
# the files are lower-case). Exits non-zero if any test prints FAIL or a suite
# fails to load.
set -o pipefail
cd "$(dirname "$0")"
status=0
passed=0
failed=0

run_suite() {
  local out
  out="$(ghc -ignore-dot-ghci -e main "$@" 2>&1)" || status=1
  printf '%s\n' "$out"
  local p f
  p="$(printf '%s\n' "$out" | grep -c 'Status: *PASS')"
  f="$(printf '%s\n' "$out" | grep -c 'FAIL')"
  passed=$((passed + p)); failed=$((failed + f))
  if [ "$f" -ne 0 ] || [ "$p" -eq 0 ]; then status=1; fi
}

for t in tests/TestEx*.hs; do
  n="${t#tests/TestEx}"; n="${n%.hs}"
  run_suite "$t" "ex$n.hs"
done
run_suite tests/TestPipeline.hs tests/Pipeline.hs ex2.hs ex3.hs ex4.hs ex5.hs

echo "Passed: $passed  Failed: $failed"
if [ "$status" -eq 0 ]; then echo "All test suites passed."; else echo "Some tests FAILED." >&2; fi
exit "$status"
