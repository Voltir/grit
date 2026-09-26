# grit.dbos

The DBOS quarantine (ADR 0001): the only module that names DBOS, JDBC or the Postgres
driver, translated into core's seams here. `resources/schema.sql` is the schema.

- **`sql`** — Postgres behind core's store seams: `DbConfig` (where the database is),
  `SqlDb`, `SqlJot`, `SqlEntryStore`, `SqlConversationStore`, `SqlUsageLedger`,
  `SqlModelProfileStore`, `SqlModelFactStore`, `SqlPeriodStore` (a conversation's periods,
  each seal numbered in commit order), `SqlLifecycleStore` (the settings in force, one
  row). Imports nothing else in dbos.
- **`workflow`** — DBOS behind `Durable`: `DbosDurable`, `DurableWorkflow` (registers a
  body under the fixed class name `grit.workflow`, so moving it strands no workflow row),
  `Turns` (how a turn is known to DBOS: its workflow name and queue), `Closes` (the close
  workflow, on the same queue under the conversation's partition, so it never runs beside
  one of its turns). Imports nothing else in dbos.
- **`engine`** — both, composed: `Engine` (what `grit.app` opens), `TurnStatus`, and
  `SqlInbox`, which records a message and enqueues its turn in one transaction (opening
  the conversation's next period when none is open), records a signal that a period is
  done, and sends a turn the answer to its gated call (`DBOSClient.send`); `Sweeper`,
  the sweep `Engine.sweepEvery` runs: every open period whose deadline has come has its
  close enqueued under its deterministic id, and an attempt that finished with its period
  still due is deleted and enqueued again; then every period closed longer ago than the
  retention window has its turn and close workflows deleted (`deleteWorkflows`), and after
  them its raw entries, keeping its closing entry. ← `sql`, `workflow`

`sql` and `workflow` are siblings and never name each other. No source file sits at the
root, and `scripts/enola-law.sh` fails on a new import cycle.
