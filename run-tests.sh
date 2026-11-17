#!/usr/bin/env bash
# Runs every challenge's test suite. Requires GHC (runghc) on PATH.
set -e
cd "$(dirname "$0")"
for t in tests/TestEx*.hs; do runghc -i. "$t"; done
