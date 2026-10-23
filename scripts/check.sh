#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
cabal build all --enable-optimization=2
cabal test --test-options=--unit --test-show-details=direct
cabal test --test-options=--database --test-show-details=direct
npm run build:ui
npm run test:ui
python3 scripts/integration.py
