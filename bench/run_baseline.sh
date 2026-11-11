#!/bin/bash
set -e
cd "$(dirname "$0")/.."
EXT=$(python3 -c 'import sysconfig; print(sysconfig.get_config_var("EXT_SUFFIX"))')
SHA=$(git rev-parse --short HEAD)
OUT="bench/baseline-$SHA-$(date +%Y%m%d).jsonl"
python3 -c "import json; print(json.dumps({'_meta':{'sha':'$SHA','date':'$(date -Iseconds)','cpu':'AMD Ryzen 5 7640U','nproc':12,'n':10000,'note':'pre one-graph-refactor baseline (tenforty-ovz); profiles exercise the cross-form linker'}}))" > "$OUT"
for spec in "python parallel:interpreter"; do
  feats="${spec%%:*}"; label="${spec##*:}"
  echo "=== building $label ($feats) ==="
  cargo build -p tenforty-graph --release --features "$feats" 2>&1 | tail -1
  cp target/release/libgraphlib.so "src/tenforty/graphlib/graphlib$EXT"
  for c in 1 2 4 8; do
    for p in fed_simple fed_heavy ca ny; do
      RAYON_NUM_THREADS=$c uv run python scripts/bench_baseline.py --label $label --profile $p --cores $c --n 10000 --repeats 2 --out "$OUT"
    done
  done
done
echo "DONE -> $OUT"
