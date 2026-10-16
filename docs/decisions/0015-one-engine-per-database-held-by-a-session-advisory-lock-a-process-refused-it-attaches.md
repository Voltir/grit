# 0015. One engine per database, held by a session advisory lock; a process refused it attaches

Status: accepted (2026-09-27); revised (2026-10-02)

Context: every grit process runs DBOS with executor id `local`, and DBOS resumes every
pending workflow of its own executor id at launch, with the queue shared through Postgres.
Two engines on one database would run each other's turns. The alternatives: unique executor
ids (a crashed engine's turns are then never adopted); a lease row with an expiry (it needs
clocks that agree and an expiry to tune, and a paused process outlives its lease and runs
beside the next holder); or the lock on a pooled connection (a pool may close or recycle it,
silently releasing the lock).

Decision:

- **One engine per database, by `pg_try_advisory_lock` on a connection of its own**
  (`EngineLock`), under one constant key: advisory locks are scoped to their database, so two
  databases on one server never contend. The lock is taken before anything else: the holder
  then applies grit's schema, writes its row, starts its heartbeat, and only then builds
  DBOS, so recovery always runs under the lock.
- **The holder applies `schema.sql` at every start**, from the classpath, idempotently
  (`CREATE … IF NOT EXISTS`); DBOS migrates its own schema. No migration tool: a change to an
  existing table reaches an existing database only by a hand-written `ALTER`.
- **The executor id stays `local`**, so a new holder recovers a dead one's turns as its own;
  recovery puts them back on their queue, and the holder's poll runs them.
- **The holder is the database's latest application version** (`Engine.launch`): DBOS
  dequeues a workflow enqueued with no version, as every grit enqueue is, only on the latest,
  so the one engine runs them whatever versions the database has seen (ADR 0004).
- **`grit.engines` describes the holder; the lock is the truth.** One row: machine, pid, the
  lock connection's backend pid, epoch, start, heartbeat. A process refused the lock reads it
  joined to `pg_locks`, so a row a dead engine left is never named as the holder.
- **A process refused the lock attaches as an edge** (`Link.attach`): no DBOS executor, only
  its client, the stores and the inbox, all through Postgres (ADR 0002). Its TUI serves its
  own directory (ADR 0017), says it is attached and to whom, and says `engine gone` while no
  engine holds the lock: its messages are kept, and their turns run when one does. It never
  takes the lock over. An edge of another epoch than the engine's is refused. An attach needs DBOS's
  schema migrated: on a database never started by this DBOS version it fails, and the next
  engine to start migrates it under the lock.
- **The heartbeat is every 2 s** (`EngineLock.Beat`), on the lock's connection. An update
  that fails or matches no row means the lock is lost, and the engine stops.
- **Stopping** (on close, or when the lock is lost): the sweeper stops, DBOS shuts down, grit
  waits up to 30 s (`Engine.BodiesWithin`) for the workflow bodies still running, which it
  counts itself (`Running`), since DBOS's shutdown does not wait for them, and the lock is
  released last.

Consequences:

- **The two-engine window** is one beat plus the step then running, and opens only after the
  server dropped the holder's session. For a model call the window is its whole duration: the
  residual risk is a doubled model call, which the ledger records twice. In that window DBOS
  makes the loser of a duplicated step yield (a plain step's conflict is not persisted as the
  workflow's result; a transaction step's loser rolls back and returns the winner's output),
  and a gated call runs at most once by its attempt marker (ADR 0017, "The call's step"). A
  per-transaction fence is not built: the window is bounded, and what it would guard is
  already guarded.
- A tool that reads the database without running workflows (`PromoteModelSettings`) takes
  the lock too, and so is refused while grit runs.
- No takeover: when the engine stops, attached processes wait; the next grit started takes
  the lock and recovers the waiting turns.
- Enforced by `EngineLockTests` (a second engine refused and the holder named, a dead row not
  named, two databases not contending, a lost session stopping the engine) and
  `AttachLiveTests`.
