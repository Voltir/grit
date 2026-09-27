# grit.dbos

The DBOS quarantine (ADR 0001): the only module that names DBOS, JDBC or the Postgres
driver, translated into core's seams here. `resources/schema.sql` is the schema.

- **`sql`** — Postgres behind core's store seams: `DbConfig` (where the database is),
  `SqlDb`, `SqlJot`, `SqlEntryStore`, `SqlConversationStore` (and each conversation's place, `grit.places`, and who began it, one of `grit.principals`), `SqlUsageLedger`,
  `SqlModelProfileStore`, `SqlModelFactStore`, `SqlToolSets`, `SqlPromptStore`, `SqlToolRequests` and
  `SqlEdgeDirectory` (hosted tool calls and the edges serving them, ADR 0017), `SqlPeriodStore` (a conversation's periods,
  each seal numbered in commit order), `SqlLifecycleStore` (the settings in force, one
  row), `SqlPluginDocs`, `SqlCacheDocs` and `SqlPluginCursors` (each plugin's documents, as read and
  as posted from one closing, and its cursor), `SqlTombstones` (what is to be deleted, ADR 0014).
  Imports nothing else in dbos.
- **`workflow`** — DBOS behind `Durable`: `DbosDurable`, `DurableWorkflow` (registers a
  body under the fixed class name `grit.workflow`, so moving it strands no workflow row, and
  counts it in `Running` while it runs),
  `Turns` (how a turn is known to DBOS: its workflow name and queue), `Closes` and
  `Settles` (the close and settle workflows, on the same queue under the conversation's
  partition, so neither runs beside one of its turns), `Posts` (the posting workflow, on a `posts` queue partitioned by
  plugin). Imports nothing else in dbos.
- **`engine`** — both, composed: `Link` (an edge's view of the engine: inbox, reads,
  streams, turn status, the holder, its registered edge; `Engine` is one, and
  `Link.attach` another for a process refused the lock), `EngineLock` (the database's one engine, ADR 0015: a
  session advisory lock, its `grit.engines` row and heartbeat), `Engine` (what `grit.app`
  starts under the lock; closing it, or losing the lock, stops the sweep, then DBOS, waits
  for running bodies, and releases the lock last), `SqlDesk` (an edge's registration, live
  while its own connection holds its lock, which also listens for requests), `TurnStatus`, and
  `SqlInbox`, which records a message, and who wrote it (`grit.inbound`), and enqueues its turn in one transaction (opening
  the conversation's next period when none is open), and sends a turn the answer to its
  gated call (`DBOSClient.send`); `Sweeper`,
  the sweep `Engine.sweepEvery` runs: every open period whose deadline has come has its
  attempt on that deadline enqueued, under an id naming the deadline, so a moved deadline
  is a new attempt and no workflow is ever deleted to run again, and every other one quiet
  for the settle window has its question enqueued, under an id naming its quiet stretch; every enabled plugin
  behind the newest closed period has a run enqueued from its cursor, up to
  `PostRef.Attempts` runs from one cursor; what did not finish its work is logged as stuck;
  and every plugin with a cursor but not enabled is marked for
  deletion; then `Collector` collects every tombstone whose kind's window has passed (ADR
  0014): the workflows it names, unless one is still queued or running, then its rows.
  `Transact` holds the sweep's short transactions. ← `sql`, `workflow`

`sql` and `workflow` are siblings and never name each other. No source file sits at the
root, and `scripts/enola-law.sh` fails on a new import cycle.
