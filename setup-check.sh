#!/usr/bin/env bash
# Setup check: confirms GHC is installed and new enough, then solves the
# bundled sample puzzle end to end (parse -> unparse -> solve -> check) and
# prints the verified solution. Exits non-zero on any failure.
set -euo pipefail
cd "$(dirname "$0")"

if ! command -v ghc >/dev/null 2>&1; then
  echo "[FAIL] ghc not found on PATH. Install GHC 9.6+ (e.g. via GHCup)." >&2
  exit 1
fi

version="$(ghc --numeric-version)"
major="${version%%.*}"; rest="${version#*.}"; minor="${rest%%.*}"
if [ "$major" -lt 9 ] || { [ "$major" -eq 9 ] && [ "$minor" -lt 6 ]; }; then
  echo "[FAIL] GHC $version found; 9.6 or later is required." >&2
  exit 1
fi
echo "[ok] ghc $version on PATH"

ghc -ignore-dot-ghci -e main tests/SetupCheck.hs tests/Pipeline.hs \
  ex2.hs ex3.hs ex4.hs ex5.hs
