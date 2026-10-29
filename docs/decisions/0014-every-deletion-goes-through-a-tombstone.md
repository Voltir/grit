# 0014. Every deletion goes through a tombstone

Status: accepted (2026-09-26); revised (2026-10-29)

Context: grit keeps data under windows: a closed period's raw entries and workflow histories
for one, closings, period rows and usage for a longer one, plugin documents for as long as
their plugin says. DBOS deletes nothing on its own; its `garbageCollect` (by age, every
finished workflow) is not reachable from `DBOS` or `DBOSClient`, and `deleteWorkflows`
deletes a workflow whatever its status. The alternatives: delete by a query per table, with
nothing recording what was decided or when; use DBOS's collection, which would delete
histories whose rows remain; or record each decision to delete where it is made, and delete
only what is recorded.

Decision:

- **Three windows** (`Windows`, data): *idle*, after which a quiet period lapses (ADR 0011);
  *retention*, how long a closed period's raw entries and workflow histories are kept; and
  *ledger*, how long a closing is kept once the next replaces it, and a conversation once its
  last period closed. The latest closing of a live conversation is kept with it. **The ledger
  window is at least the retention window** (`Windows.of` refuses otherwise): every period's
  raw entries fall due before the closing and quiet tombstones that wait on them, and a quiet
  conversation waits while any of its periods' raw entries are kept, so a cascade never takes
  rows whose histories only their own tombstone deletes.
- **Whatever knows a thing is finished writes a tombstone on it**, in the transaction that
  knows it: a seal on its period's raw entries, on the closing it replaces and on its
  conversation going quiet; a plugin's posting, cursor restart or document version on what it
  left behind; the sweep on a plugin no longer enabled, and on a session's log and row once
  kept long enough after it ends (ADR 0050); an ended schedule on its row. A
  tombstone names its target (`grit.core.retention.Target`) and when it was written, at most
  one per target: a second write keeps the first time.
- **One collector deletes, and only what a due tombstone names**, once its kind's window has
  passed since it was written, the window read from the settings in force at collection. It
  deletes the target's workflows first, and never one still queued or running (it defers the
  tombstone behind the ones due since), then its rows and the tombstone's end in one
  transaction. What finds its target alive again (a quiet conversation with a later period, a
  plugin enabled again) spares the tombstone instead. Collecting is idempotent.
- Besides live state, the one deletion no tombstone names is the collector's forgetting of
  tombstones ended longer ago than the ledger window.
- **Tombstones govern records, not live state.** A row that is live state with no history (a
  lock such as a claim, a cursor, a counter) is removed by the write that ends it, or by the
  sweep once it has lapsed, with no tombstone: tombstoning a release would hold the next claim
  until it was collected.
- **Every table declares its retention class** in `schema.sql`: journal (the retention
  window), ledger (the ledger window), cache (rebuilt from closings, deleted with them),
  expiring (deleted once its own expiry passes) or kept, each with what deletes it and what
  bounds it.

Consequences:

- What is deleted, and why, is a row that can be read before and after; a leak (a finished
  workflow or row no tombstone accounts for) is a bookkeeping bug that can be reported.
- A new table or workflow must say who writes its tombstones, or declare itself kept.
- A window changed takes effect on tombstones already written.
- A tombstone written by a step adds rows inside the step's transaction, not steps: no patch
  or epoch (ADR 0004).
- An ingest racing a quiet conversation's removal creates a fresh conversation for the same
  origin.
- Enforced by `Windows.of` and by `SchemaRetentionTests`, which fails on a table that
  declares no class.
