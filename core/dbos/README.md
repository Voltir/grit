# grit.dbos

The DBOS quarantine (ADR 0001): the only module that names DBOS, JDBC or the Postgres
driver, translated into core's seams here. `resources/schema.sql` is the schema.

- **`sql`** — Postgres behind core's store seams: `DbConfig` (where the database is),
  `Opener` (each transaction's clearance as it opens: a subject resolved from the rows it
  names, or maintenance's for this module's own transactions, ADR 0030), `SqlDb`, `SqlJot`, `SqlEntryStore`, `SqlConversationStore` (and each conversation's place and room, `grit.places`, the account that began it, one of `grit.identities`, and the label it was created at), `SqlIdentities` (each account an action came through, `grit.identities`, kept as a new person's one account when first seen, its person's id minted by Postgres under a lock on the account, and read back as the principal it is linked to now, ADR 0032), `SqlLinks` (every move of an account between people: a deployment's declaration made so at each start, merging into a declared person the people its accounts leave), `SqlUsageLedger`,
  `SqlModelProfileStore`, `SqlModelSettingStore`, `SqlToolSets`, `SqlPromptStore`, `SqlToolRequests` and
  `SqlEdgeDirectory` (hosted tool calls and the edges serving them, ADR 0017), `SqlPeriodStore` (a conversation's periods,
  each seal numbered in commit order), `SqlLifecycleStore` (the settings in force, one
  row), `SqlVoiceStore` (the voice, one row), `SqlPrincipals` (the name each account goes by, and who wrote each inbound entry, through `grit.authors`), `SqlDeliveries` (the replies an edge has yet to post outside grit), `SqlAcknowledgements` (the messages an edge marks as being answered while their turns run), `SqlPluginDocs`, `SqlCacheDocs` and `SqlPluginCursors` (each plugin's documents, as read and
  as posted from one closing, and its cursor), `SqlDocuments` (every plugin's versioned documents, ADR 0028: each plugin's shelf and keeper, the search windows draw on, and the terms each start declares), `SqlTombstones` (what is to be deleted, ADR 0014), `SqlSchedules` (a job's schedules, `grit.schedules`, ADR 0029, and each plugin's desk over them, which reads a call's asker and address from its turn's first entry and delivery), `SqlTriageStore`
  (what triage made of each heard message, deleted with its entry), `SqlTriageShadows` (what
  each declared shadow variant made of a heard message, `grit.triage_shadows`, deleted with its
  entry), `SqlSpeechStore` (each
  heard message's reach, and grit's decisions to speak or not, kept with their period's usage),
  `SqlRoomReads` (what a room said before a time, as a pool reads it), `SqlClearance` (a transaction's clearance
  as SQL: the one filter every read of labelled rows puts in its `WHERE`, inside any ranked or
  limited subquery, ADR 0030), `SqlLabels` (labels as
  `grit.labels` interns them, the one translation through `LabelParts`, and the compartment sets
  the database has run under, ADR 0030), `SqlReviews` (every
  heard message a review considered, its prompt and the verdict standing on it,
  `grit.reviews`, kept with its conversation, ADR 0024).
  Imports nothing else in dbos.
- **`workflow`** — DBOS behind `Durable`: `DbosDurable`, `DurableWorkflow` (registers a
  body under the fixed class name `grit.workflow`, so moving it strands no workflow row, and
  counts it in `Running` while it runs),
  `Turns` (how a turn is known to DBOS: its workflow name and queue), `Closes`, `Settles`,
  `Triages` and `Runs` (the close, settle and triage workflows, and a job's run, on the same queue under the
  conversation's partition, so none runs beside one of its turns), `Posts` (the posting workflow, on a `posts` queue partitioned by
  plugin), `Shadows` (the shadow workflow, on a `shadows` queue of its own, one at a time,
  never the turns' queue), `Stitches` (an opening's placement, on a `stitches` queue
  partitioned by room, one at a time in the order queued, ADR 0023). Imports nothing else
  in dbos.
- **`engine`** — both, composed: `Link` (an edge's view of the engine: inbox, reads,
  streams, turn status, the holder, its registered edge; `Engine` is one, and
  `Link.attach` another for a process refused the lock), `EngineLock` (the database's one engine, ADR 0015: a
  session advisory lock, its `grit.engines` row and heartbeat), `Build` (the grit build this process runs, from the `grit/build.properties`
  Mill writes into this module's jar; each engine start is recorded with it in
  `grit.engine_starts`), `Engine` (what `grit.app`
  starts under the lock, refused as `Unopened` when the lock is held or its deployment drops a
  compartment the database ran under; closing it, or losing the lock, stops the sweep, then DBOS, waits
  for running bodies, and releases the lock last; `every` runs a pass on a thread of its own, one
  at a time, until the engine closes, and the sweep is one; `unfinished` counts the workflows still
  queued or running, which `grit backfill` waits out; `placements` waits for an opening's
  placement, through `DbosPlacements`), `SqlDesk` (an edge's registration, live
  while its own connection holds its lock, which also listens for requests), `TurnStatus`, and
  `SqlInbox`, which records a message, and the account it came through (`grit.inbound`), and enqueues its turn in one transaction (opening
  the conversation's next period when none is open), enqueues an opening's placement, then
  a heard message's triage, once it is recorded, and sends a turn the answer to its
  gated call (`DBOSClient.send`); `Sweeper`,
  the sweep `Engine.sweepEvery` runs: every open period whose deadline has come has its
  attempt on that deadline enqueued, under an id naming the deadline, so a moved deadline
  is a new attempt and no workflow is ever deleted to run again, and every other one quiet
  for the settle window has its question enqueued, under an id naming its quiet stretch; every enabled plugin
  behind the newest closed period has a run enqueued from its cursor, up to
  `PostRef.Attempts` runs from one cursor; every declared shadow variant none of whose shadows
  is queued or running has its oldest unshadowed messages enqueued, as many as the rest of its
  day's cap covers (`grit.core.triage.Shadowing`), passing over any whose shadow ended keeping
  nothing; what did not finish its work is logged as stuck;
  and every plugin not enabled with a cursor, documents or document terms is marked for
  deletion; then `Collector` collects every tombstone whose kind's window has passed (ADR
  0014), a document version's by its plugin's declared retention: the workflows it names,
  unless one is still queued or running, then its rows.
  `Transact` holds the sweep's short transactions. ← `sql`, `workflow`
- **`internal`** — what `grit.dbos` lends to the eval harness alone (ADR 0030; rule 1h in
  `enola-intent.yaml`): `Reader` (a database's stores, engine starts and DBOS's workflow
  records read without an engine or its lock, on sessions Postgres keeps read-only, under the
  compartments the database last ran under, and `all`, which reads every row whatever its
  label, for capturing a database whole), and
  `EngineStarts` (`grit.engine_starts` read back). ← `sql`, `workflow`, `engine`

`sql` and `workflow` are siblings and never name each other. No source file sits at the
root, and `scripts/enola-law.sh` fails on a new import cycle.
