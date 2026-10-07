#!/usr/bin/env bash
# The architecture gate. Run it by hand, or before a commit that moves code between
# packages. Adapted from actualbest's scripts/enola-law.sh.
#
# Scope: --fail-on=constraints,cycles -- the rules in enola-intent.yaml, and any new import
# cycle between packages. Mill rejects a cycle between modules but not one between the
# packages inside a module (a directory importing a subdirectory that imports it back),
# and cycles are read off import edges, which are sound here. grit declares no layers
# (Mill and the rules already cover them; see that file).
#
# Scope, part two: enola's Scala symbol facts are DEGRADED on this repository -- capture
# checking is on project-wide and the extractor drops symbols without reporting a parse
# error. Never add a symbol-shaped explainer (exported-surface, complexity-outliers) to
# --fail-on here. See the TRUST BOUNDARY note in mcp-arch.yaml.
set -euo pipefail

cd "$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
enola=tools/enola

[ -x "$enola" ] || { echo "no $enola -- run: bash scripts/fetch-enola.sh" >&2; exit 1; }

$enola --generate >/dev/null

lint=$($enola constraints lint mcp-arch.yaml) || { printf '%s\n' "$lint"; exit 1; }
printf '%s\n' "$lint"

# TRAP: a declaration that matches nothing is not a validation problem to enola -- `lint`
# prints "matches nothing" and still exits 0, and every rule naming it then passes
# VACUOUSLY. A mistyped component path, or one naming a file instead of a directory
# (enola's modules are directories), surfaces exactly this way.
if printf '%s\n' "$lint" | grep -q "matches nothing"; then
  echo "A declaration matches no code, so the rules naming it are not being checked." >&2
  exit 1
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
  exit 1
fi

# TRAP: java.lang needs no import, so a process started with `new ProcessBuilder` or
# `Runtime.getRuntime.exec` makes no import edge, and rule 1d cannot see it. Grep for it.
# grit.tui's clipboard (clip.exe, in wire/term) predates grit.host and is the one exception.
# Every production source root: grit.tui.examples' is nested in grit.tui's folder, and
# grit.eval's and grit.eval.harness' are outside the core/extensions/kit/deployments groups.
started=$(grep -rlE 'ProcessBuilder|sys\.process|getRuntime\.exec|ProcessHandle' --include='*.scala' core/*/src extensions/*/src extensions/tui/examples/src kit/src deployments/*/src eval/src eval/harness/src |
  grep -vE '^extensions/host/src/|^extensions/tui/src/wire/term/SystemTerminal\.scala$' || true)
if [ -n "$started" ]; then
  echo "Only grit.host may start a process; these sources name a process API:" >&2
  printf '  %s\n' $started >&2
  exit 1
fi

# TRAP: rule 1f sees only `import grit.core.visibility.LabelParts`. A fully qualified use, or
# one through a wildcard import, makes no edge to it, so grep every source for the name:
# only core's visibility package (its sources and tests) and grit.dbos may write it.
parts=$(grep -rlw 'LabelParts' --include='*.scala' core extensions kit deployments eval |
  grep -vE '^core/core/(src|test/src)/visibility/|^core/dbos/' || true)
if [ -n "$parts" ]; then
  echo "Only grit.dbos and core's visibility package may name LabelParts; these sources do:" >&2
  printf '  %s\n' $parts >&2
  exit 1
fi

# TRAP: rule 1g, as 1f above: only core's visibility package and grit.dbos may name
# Maintenance.
maintenance=$(grep -rlw 'Maintenance' --include='*.scala' core extensions kit deployments eval |
  grep -vE '^core/core/(src|test/src)/visibility/|^core/dbos/' || true)
if [ -n "$maintenance" ]; then
  echo "Only grit.dbos and core's visibility package may name Maintenance; these sources do:" >&2
  printf '  %s\n' $maintenance >&2
  exit 1
fi

# TRAP: rule 1h, as 1f above: only grit.dbos and the eval harness may name grit.dbos.internal.
internal=$(grep -rlE 'grit\.dbos\.internal' --include='*.scala' core extensions kit deployments eval |
  grep -vE '^core/dbos/|^eval/harness/' || true)
if [ -n "$internal" ]; then
  echo "Only grit.dbos and the eval harness may name grit.dbos.internal; these sources do:" >&2
  printf '  %s\n' $internal >&2
  exit 1
fi

$enola baseline show mcp-arch.yaml >/dev/null 2>&1 || $enola baseline pin mcp-arch.yaml >/dev/null

$enola check --fail-on=constraints,cycles mcp-arch.yaml
