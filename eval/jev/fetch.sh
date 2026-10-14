#!/usr/bin/env bash
# Fetches the third-party text the integrity-features experiment is built from, into data/
# (gitignored): AgentDojo and BIPIA at the commits the experiment used, and twelve Wikipedia
# extracts. Nothing fetched is imported or executed; build_corpus.py reads it as text. Requests
# carry a generic User-Agent and nothing that identifies whoever runs them.
#
#   eval/jev/fetch.sh
#   python3 -I eval/jev/integrity-features/build_corpus.py eval/jev/data/agentdojo \
#     eval/jev/data/bipia eval/jev/data/wiki - eval/jev/results/corpus.jsonl
set -euo pipefail

cd "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
mkdir -p data/wiki results

pinned() { # dir url commit
  [ -d "data/$1/.git" ] || git clone -q "$2" "data/$1"
  git -C "data/$1" -c advice.detachedHead=false checkout -q "$3"
}
pinned agentdojo https://github.com/ethz-spylab/agentdojo 089ed468
pinned bipia https://github.com/microsoft/BIPIA a004b69e

for title in 2024_Summer_Olympics Apollo_11 Bridge Coffee Great_Recession Honey_bee Linux_kernel \
  Photosynthesis Printing_press Rhine Tokyo Volcano; do
  out="data/wiki/$title.json"
  [ -s "$out" ] && continue
  curl -fsS --retry 3 --retry-delay 5 -A "grit-eval" -o "$out" \
    "https://en.wikipedia.org/w/api.php?action=query&prop=extracts&explaintext=1&format=json&titles=$title"
  sleep 1
done
echo "fetched into $(pwd)/data"
