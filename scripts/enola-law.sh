#!/usr/bin/env bash
# The architecture gate. Run it by hand, or before a commit that moves code between
# packages. scripts/enola-plant is its test: it plants a breach of every check below.
#
# Scope: the rules in enola-intent.yaml, and any import cycle between production packages
# (read from the snapshot, whatever baseline is pinned). Mill rejects a cycle between modules but not one between the
# packages inside a module (a directory importing a subdirectory that imports it back),
# and cycles are read off import edges, which are sound here. grit declares no layers
# (Mill and the rules already cover them; see that file).
#
# Scope, part two: enola's Scala symbol facts never gate here. Never add a symbol-shaped
# explainer (exported-surface, complexity-outliers) to --fail-on. See the TRUST BOUNDARY note
# in mcp-arch.yaml.
set -euo pipefail

cd "$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
enola=tools/enola

[ -x "$enola" ] || { echo "no $enola -- run: bash scripts/fetch-enola.sh" >&2; exit 1; }

$enola --generate >/dev/null

# Every check below runs, and the script fails at the end if any failed: one run reports
# everything, and scripts/enola-plant's plants reach enola's check past the greps.
failed=0

lint=$($enola constraints lint mcp-arch.yaml) || { printf '%s\n' "$lint"; exit 1; }
printf '%s\n' "$lint"

# TRAP: a declaration that matches nothing is not a validation problem to enola -- `lint`
# prints "matches nothing" and still exits 0, and every rule naming it then passes
# VACUOUSLY. A mistyped component path, or one naming a file instead of a directory
# (enola's modules are directories), surfaces exactly this way.
if printf '%s\n' "$lint" | grep -q "matches nothing"; then
  echo "A declaration matches no code, so the rules naming it are not being checked." >&2
  failed=1
fi

# TRAP: enola has no package pattern, so rule 8 is one rule per class. A class a
# quarantine module starts importing without a matching rule may then be imported
# anywhere, and nothing says so. Every quarantined class grit.dbos (DBOS, JDBC, the
# driver), grit.models or grit.mcp (java.net.http), grit.slack (the Slack SDK) or grit.host
# (processes) imports must be named by a rule.
unruled=$( { grep -oE '"name":"core/dbos/src(/[a-z]+)* -\\u003e (dev\.dbos|java\.sql|javax\.sql|org\.postgresql)\.[^"]*"' .enola/facts.jsonl
    grep -oE '"name":"extensions/models/src(/[a-z]+)* -\\u003e java\.net\.http\.[^"]*"' .enola/facts.jsonl
    grep -oE '"name":"extensions/mcp/src(/[a-z]+)* -\\u003e java\.net\.http\.[^"]*"' .enola/facts.jsonl
    grep -oE '"name":"extensions/host/src(/[a-z]+)* -\\u003e (java\.lang\.Process|scala\.sys\.process)[^"]*"' .enola/facts.jsonl
    grep -oE '"name":"extensions/slack/src(/[a-z]+)* -\\u003e com\.slack\.[^"]*"' .enola/facts.jsonl; } |
  sed -E 's/.*u003e ([^"]*)"/\1/' | sort -u | while read -r class; do
    grep -qF "\"* -> $class\"" enola-intent.yaml || echo "$class"
  done)
if [ -n "$unruled" ]; then
  echo "A quarantine module imports classes that no rule in enola-intent.yaml names:" >&2
  printf '  %s\n' $unruled >&2
  failed=1
fi

# The greps below read every Scala source git tracks or would track: a new file not yet
# added is caught too. A file under a gitignored path is seen by neither these greps nor
# enola only while .gitignore and mcp-arch.yaml's `ignore` agree, so a path enola indexes
# must never be gitignored, or a source there would escape the greps.
sources=()
while IFS= read -r f; do [ -f "$f" ] && sources+=("$f"); done < <(git ls-files --cached --others --exclude-standard -- '*.scala')
# Production sources: under a `src` root that is not a module's `test`, `it` or `probes`.
production=()
for f in "${sources[@]}"; do
  [[ "$f" == */src/* && ! "$f" =~ /(test|it|probes)/src/ ]] && production+=("$f")
done

# names <grep flags and pattern...> -- the sources that match, one a line.
names() { grep -lE "$@" -- "${sources[@]}" || true; }

# TRAP: java.lang needs no import, so a process started with `new ProcessBuilder` or
# `Runtime.getRuntime.exec` makes no import edge, and rule 1d cannot see it. Grep for it in
# every production source. grit.tui's clipboard (clip.exe, in wire/term) predates grit.host
# and is the one exception.
started=$( { grep -lE 'ProcessBuilder|sys\.process|getRuntime\.exec|ProcessHandle' -- "${production[@]}" || true; } |
  grep -vE '^extensions/host/src/|^extensions/tui/src/wire/term/SystemTerminal\.scala$' || true)
if [ -n "$started" ]; then
  echo "Only grit.host may start a process; these sources name a process API:" >&2
  printf '  %s\n' $started >&2
  failed=1
fi

# TRAP: rule 1f sees only `import grit.core.visibility.LabelParts`. A fully qualified use, or
# one through a wildcard import, makes no edge to it, so grep every source for the name:
# only core's visibility package (its sources and tests) and grit.dbos may write it.
parts=$(names -w 'LabelParts' | grep -vE '^core/core/(src|test/src)/visibility/|^core/dbos/' || true)
if [ -n "$parts" ]; then
  echo "Only grit.dbos and core's visibility package may name LabelParts; these sources do:" >&2
  printf '  %s\n' $parts >&2
  failed=1
fi

# TRAP: rule 1g, as 1f above: only core's visibility package and grit.dbos may name
# Maintenance.
maintenance=$(names -w 'Maintenance' | grep -vE '^core/core/(src|test/src)/visibility/|^core/dbos/' || true)
if [ -n "$maintenance" ]; then
  echo "Only grit.dbos and core's visibility package may name Maintenance; these sources do:" >&2
  printf '  %s\n' $maintenance >&2
  failed=1
fi

# TRAP: rule 1h, as 1f above: only grit.dbos and the eval harness may name grit.dbos.internal.
internal=$(names 'grit\.dbos\.internal' | grep -vE '^core/dbos/|^eval/harness/' || true)
if [ -n "$internal" ]; then
  echo "Only grit.dbos and the eval harness may name grit.dbos.internal; these sources do:" >&2
  printf '  %s\n' $internal >&2
  failed=1
fi

# TRAP: rule 1i, as 1f above: only grit.dbos, the file defining it, and core's test stand-in
# for the store may name PrincipalIds.
ids=$(names -w 'PrincipalIds' |
  grep -vE '^core/dbos/|^core/core/src/id/PrincipalId\.scala$|^core/core/test/src/id/TestPrincipalIds\.scala$' || true)
if [ -n "$ids" ]; then
  echo "Only grit.dbos may name PrincipalIds; these sources do:" >&2
  printf '  %s\n' $ids >&2
  failed=1
fi

# TRAP: `check` grades cycles as NEW against the pinned baseline, so a cycle present when a
# baseline was pinned (by this script when none exists, or by anyone) passes from then on.
# There are none, so any cycle finding fails, at every confidence: `check` counts only those at
# its --min-confidence (1.00). enola finds cycles between production packages only; a cycle
# among test sources is not reported at all.
cycles=$(python3 -c '
import json
for i in json.load(open(".enola/insights.json")):
    if i.get("source") == "cycles":
        print("  " + i.get("title", ""))
        for e in i.get("evidence") or []:
            print("    " + (e.get("detail") or e.get("file") or e.get("symbol") or ""))
')
if [ -n "$cycles" ]; then
  echo "The law fails on any import cycle; enola reports these:" >&2
  printf '%s\n' "$cycles" >&2
  failed=1
fi

$enola baseline show mcp-arch.yaml >/dev/null 2>&1 || $enola baseline pin mcp-arch.yaml >/dev/null

rc=0
$enola check --fail-on=constraints,cycles mcp-arch.yaml || rc=$?
if [ "$rc" -eq 3 ]; then
  # Not comparable: most often a baseline pinned by another enola version. Re-pinning here
  # would accept whatever the tree holds, so it is left to a person, once this run's other
  # checks pass.
  echo "The pinned baseline is not comparable to this snapshot. When the checks above pass, re-pin it:" >&2
  echo "  tools/enola baseline pin mcp-arch.yaml" >&2
fi
[ "$rc" -eq 0 ] || failed=1

exit "$failed"
