# grit.dbos

The DBOS quarantine (ADR 0001): the only module that names DBOS, JDBC or the Postgres
driver, translated into core's seams here. `resources/schema.sql` is the schema.

- **`sql`** — Postgres behind core's store seams: `DbConfig` (where the database is),
  `SqlDb`, `SqlJot`, `SqlEntryStore`, `SqlConversationStore`, `SqlUsageLedger`,
  `SqlModelProfileStore`, `SqlModelFactStore`. Imports nothing else
  in dbos.
- **`workflow`** — DBOS behind `Durable`: `DbosDurable`, `DurableWorkflow` (registers a
  body under the fixed class name `grit.workflow`, so moving it strands no workflow row),
  `Turns` (how a turn is known to DBOS: its workflow name and queue). Imports nothing else in dbos.
- **`engine`** — both, composed: `Engine` (what `grit.app` opens), `TurnStatus`, and
  `SqlInbox`, which records a message and enqueues its turn in one transaction, and sends
  a turn the answer to its gated call (`DBOSClient.send`). ← `sql`,
  `workflow`

`sql` and `workflow` are siblings and never name each other. No source file sits at the
root, and `scripts/enola-law.sh` fails on a new import cycle.
