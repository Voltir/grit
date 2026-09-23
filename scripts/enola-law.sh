#!/usr/bin/env bash
# The architecture gate. Run it by hand, or before a commit that moves code between
# packages. Adapted from actualbest's scripts/enola-law.sh.
#
# Scope: --fail-on=constraints ONLY. Layers and cycles are RATCHETS -- their verdict
# depends on which commit the baseline was pinned from, so in actualbest they run in CI
# against the pull request's merge base. grit has no CI and no remote, so a local ratchet
# would grade the branch against whatever happened to be pinned last, which is noise.
# Whether grit wants that is a question for the configuration session, not a default.
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
# prints "matches nothing" and still exits 0, and every rule or layer naming it then
# passes VACUOUSLY. Both traps in enola-intent.yaml (layer paths being service-relative,
# and enola's module model being directory-grained) surface exactly this way, and both
# were found by this grep rather than by the exit code.
if printf '%s\n' "$lint" | grep -q "matches nothing"; then
  echo "A declaration matches no code, so the rules or layers naming it are not being checked." >&2
  exit 1
fi

$enola baseline show mcp-arch.yaml >/dev/null 2>&1 || $enola baseline pin mcp-arch.yaml >/dev/null

$enola check --fail-on=constraints mcp-arch.yaml
