# 0014. Every deletion goes through a tombstone

Status: accepted (2026-09-26)

Context: grit keeps data under windows (ADR 0011): raw entries and workflow histories for one,
closings, period rows and usage for a longer one, plugin documents as a cache of the closings.
DBOS deletes nothing on its own, and its `garbageCollect` (by age, every finished workflow)
is not reachable from `DBOS` or `DBOSClient`; `deleteWorkflows` deletes a workflow whatever its
status. Deletion had grown two paths: the sweep's purge, a query for periods past the window,
and a plugin cursor's restart deleting its documents at once. The alternatives: keep deleting
by a query per table, with nothing recording what was decided or when; use DBOS's collection,
which would delete histories whose rows remain; or record each decision to delete where it is
made, and delete only what is recorded.

Decision:

- **Whatever knows a thing is finished writes a tombstone on it**, in the transaction that
  knows it: a seal on its period's raw entries, on the closing it replaces and on its
  conversation going quiet; a posting run on the runs from the cursor it moved past; a
  cursor's restart on the plugin's earlier documents; the sweep on a plugin no longer
  enabled. A tombstone names its target (`grit.core.retention.Target`) and when it was
  written. There is at most one per target: a second write keeps the first time.
- **One collector deletes, and only what a due tombstone names**, once its kind's window has
  passed since it was written, the window read from the settings in force at collection. It
  deletes the target's workflows first, and never one still queued or running (it defers the
  tombstone to a later sweep, behind the ones due since), then its rows and the tombstone's
  end in one transaction. What finds its target alive again (a quiet conversation with a
  later period, a plugin enabled again) spares the tombstone instead. Collecting is
  idempotent: a target already gone is collected with nothing to delete.
- **The ledger window is at least the raw window.** Every period's raw entries fall due
  before the closing and quiet tombstones that wait on them; a quiet conversation waits while
  any of its periods' raw entries are kept, so a cascade never takes rows whose histories only
  their own tombstone deletes.
- The one deletion no tombstone names is the collector's forgetting of tombstones ended longer
  ago than the ledger window.
- Every table declares its retention class in `schema.sql`: journal, ledger, cache or kept.

Consequences:

- What is deleted, and why, is a row that can be read before and after; a leak (a finished
  workflow or row no tombstone accounts for) is a bookkeeping bug that can be reported.
- A new table or workflow must say who writes its tombstones, or declare itself kept;
  `SchemaRetentionTests` fails on a table that declares no class.
- A window changed takes effect on tombstones already written.
- A tombstone written by a workflow step, or a step's cancelled work, adds rows inside the
  step's transaction, not steps: no patch or epoch (ADR 0004). A period sealed before
  tombstones existed has none, and is never purged.
- An ingest racing a quiet conversation's removal creates a fresh conversation for the same
  origin.
