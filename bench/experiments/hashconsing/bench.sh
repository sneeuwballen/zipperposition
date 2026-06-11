#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../../.." && pwd)"

cd "$PROJECT_DIR"

echo "=== Building benchmark ==="
dune build bench/experiments/hashconsing/bench.exe --profile=release 2>&1

BENCH="_build/default/bench/experiments/hashconsing/bench.exe"

ITERS="${ITERS:-50}"
CONSTS="${CONSTS:-200}"
DEPTH="${DEPTH:-7}"
WARMUP="${WARMUP:-3}"
MIN_RUNS="${MIN_RUNS:-10}"

echo ""
echo "=== Parameters ==="
echo "  iters=$ITERS  consts=$CONSTS  depth=$DEPTH"
echo "  warmup=$WARMUP  min-runs=$MIN_RUNS"
echo ""

echo "=== Running hyperfine ==="
hyperfine \
  --warmup "$WARMUP" \
  --min-runs "$MIN_RUNS" \
  --export-json "$SCRIPT_DIR/results.json" \
  --export-markdown "$SCRIPT_DIR/results.md" \
  --show-output \
  -n "old" \
  "$BENCH old --iters $ITERS --consts $CONSTS --depth $DEPTH" \
  -n "spinlock" \
  "$BENCH spinlock --iters $ITERS --consts $CONSTS --depth $DEPTH" \
  -n "mutex" \
  "$BENCH mutex --iters $ITERS --consts $CONSTS --depth $DEPTH" \
  -n "hashtbl" \
  "$BENCH hashtbl --iters $ITERS --consts $CONSTS --depth $DEPTH"

echo ""
echo "=== Results saved to results.json and results.md ==="
