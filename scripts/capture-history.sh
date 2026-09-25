#!/usr/bin/env bash
# Captures one workflow's recorded steps from Postgres as a history fixture, in the shape
# grit.core.durable.History reads, under the epoch it was started in (ADR 0004):
#
#   bash scripts/capture-history.sh <workflow-id>
#
# Writes grit/turn/test/histories/<epoch>/captured-<id>.json and never overwrites.
# TurnReplayTests then replays it under every later build of that epoch. Commit it only if
# the turn is worth keeping as evidence: the fixture holds the step outputs verbatim,
# model replies included.
#
# PSQL overrides how psql is reached (default: the compose database).
set -euo pipefail

cd "$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
id=${1:?usage: capture-history.sh <workflow-id>}
psql=${PSQL:-docker compose exec -T postgres psql -U grit -d grit}

# DBOS stores a step's output through its own serializer: a grit step's String output
# arrives as a JSON string literal, which `#>> '{}'` unwraps. An error is a WireThrowable
# JSON object; its message is what a replay rethrows, kept as JSON null when the exception
# had none, so the step still reads as an error. A patch marker has neither.
query=$(cat <<SQL
SELECT jsonb_pretty(jsonb_build_object(
  'workflow', w.name,
  'id', w.workflow_uuid,
  'epoch', w.application_version,
  'source', 'captured',
  'steps', coalesce((
    SELECT jsonb_agg(jsonb_build_object('name', o.function_name)
             || CASE WHEN o.output IS NULL THEN '{}'::jsonb
                     ELSE jsonb_build_object('output', o.output::jsonb #>> '{}') END
             || CASE WHEN o.error IS NULL THEN '{}'::jsonb
                     ELSE jsonb_build_object('error', o.error::jsonb -> 'message') END
           ORDER BY o.function_id)
    FROM dbos.operation_outputs o WHERE o.workflow_uuid = w.workflow_uuid), '[]'::jsonb)))
FROM dbos.workflow_status w WHERE w.workflow_uuid = :'id'
SQL
)

json=$($psql -At -v id="$id" <<<"$query")
[ -n "$json" ] || { echo "no workflow $id" >&2; exit 1; }

epoch=$(sed -n 's/^ *"epoch": "\(.*\)",$/\1/p' <<<"$json" | head -1)
[ -n "$epoch" ] || { echo "workflow $id has no application version" >&2; exit 1; }
out="grit/turn/test/histories/$epoch/captured-${id//[^A-Za-z0-9._-]/_}.json"
[ -e "$out" ] && { echo "exists, not overwritten: $out" >&2; exit 1; }

mkdir -p "$(dirname "$out")"
printf '%s\n' "$json" >"$out"
echo "wrote $out"
