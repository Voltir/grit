# grit.dbos

The DBOS quarantine (ADR 0001): the only module that names DBOS, JDBC or the Postgres
driver, translated into core's seams here. `resources/schema.sql` is the schema.

- **`sql`** — Postgres behind core's store seams: `DbConfig` (where the database is),
  `SqlDb`, `SqlJot`, `SqlEntryStore`, `SqlConversationStore` (and each conversation's place, `grit.places`), `SqlUsageLedger`,
  `SqlModelProfileStore`, `SqlModelFactStore`, `SqlPeriodStore` (a conversation's periods,
  each seal numbered in commit order), `SqlLifecycleStore` (the settings in force, one
  row), `SqlPluginDocs` and `SqlPluginCursors` (each plugin's documents and cursor).
  Imports nothing else in dbos.
- **`workflow`** — DBOS behind `Durable`: `DbosDurable`, `DurableWorkflow` (registers a
  body under the fixed class name `grit.workflow`, so moving it strands no workflow row),
  `Turns` (how a turn is known to DBOS: its workflow name and queue), `Closes` and
  `Settles` (the close and settle workflows, on the same queue under the conversation's
  partition, so neither runs beside one of its turns), `Posts` (the posting workflow, on a `posts` queue partitioned by
  plugin). Imports nothing else in dbos.
- **`engine`** — both, composed: `Engine` (what `grit.app` opens), `TurnStatus`, and
  `SqlInbox`, which records a message and enqueues its turn in one transaction (opening
  the conversation's next period when none is open), and sends a turn the answer to its
  gated call (`DBOSClient.send`); `Sweeper`,
  the sweep `Engine.sweepEvery` runs: every open period whose deadline has come has its
  attempt on that deadline enqueued, under an id naming the deadline, so a moved deadline
  is a new attempt and no workflow is ever deleted to run again, and every other one quiet
  for the settle window has its question enqueued, under an id naming its quiet stretch; every enabled plugin
  behind the newest closed period has a run enqueued from its cursor, up to
  `PostRef.Attempts` runs from one cursor; what did not finish its work is logged as stuck;
  then every period closed longer ago than the retention window has its turn workflows and
  its close attempts and questions (found by their ids' prefix, `listWorkflows`) deleted, and after them
  its raw entries, keeping its closing entry. ← `sql`, `workflow`

`sql` and `workflow` are siblings and never name each other. No source file sits at the
root, and `scripts/enola-law.sh` fails on a new import cycle.
