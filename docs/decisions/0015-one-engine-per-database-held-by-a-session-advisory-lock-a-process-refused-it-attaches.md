# 0015. One engine per database, held by a session advisory lock; a process refused it attaches

Status: accepted (2026-09-27)

Context: every grit process ran DBOS with executor id `local`, and DBOS resumes every
pending workflow of its own executor id at launch, with the `turns` queue shared through
Postgres. Two grits on one database therefore ran each other's turns: the second resumed
the first's in-flight turn, and either dequeued any conversation's next one. The
alternatives: unique executor ids (a crashed engine's turns are then never adopted); a lease
row with an expiry (it needs clocks that agree and an expiry to tune, and a paused process
outlives its lease and runs beside the next holder); or the lock on a pooled connection (a
pool may close or recycle it, silently releasing the lock).

Decision:

- **One engine per database, by `pg_try_advisory_lock` on a connection of its own**
  (`EngineLock`), under one constant key: advisory locks are scoped to their database, so two
  databases on one server never contend. The lock is taken before anything else: the holder
  applies the schema, writes its row, starts its heartbeat, and only then builds DBOS, so
  recovery always runs under the lock.
- **The executor id stays `local`**, so a new holder recovers a dead one's turns as its own.
- **`grit.engines` describes the holder; the lock is the truth.** One row: machine, pid,
  the lock connection's backend pid, epoch, start, heartbeat. A process refused the lock
  reads it joined to `pg_locks`, so a row a dead engine left is never named as the holder.
  Where grit runs (a directory) is an edge's, and not in the row.
- **A process refused the lock attaches as an edge** (`Link.attach`): no DBOS executor, only
  its client, the stores and the inbox, all through Postgres (ADR 0002). Its TUI serves its
  own directory (ADR 0017) and shows its conversations; its header says it is attached and
  to whom, and its status line says `engine gone` while no engine holds the lock: its
  messages are kept, and their turns run when one does. It does not take the lock over.
  An edge of another compatibility epoch than the engine's does not register.
- **The heartbeat is every 2 s, on the lock's connection.** An update that fails or matches
  no row (the row taken, or deleted) means the lock is lost, and the engine stops.
- **Stopping** (on close, or when the lock is lost): the sweeper stops, DBOS shuts down, grit
  waits up to 30 s for the workflow bodies still running, and the lock is released last.
  DBOS's shutdown interrupts its workflow threads and does not wait for them (transact
  1.0.0, `DBOSExecutor.close`), so grit counts its own bodies (`Running`).

Consequences:

- **The two-engine window** is one beat plus the step then running, and opens only after
  the server dropped the holder's session (a server restart; a partition keeps the session,
  and the lock, until the server notices). For a model call the window is its whole
  duration: the residual risk is a doubled model call, whose cost the ledger records twice.
- **In that window DBOS makes the loser of a duplicated step yield** (read from the 1.0.0
  jar): a plain step recorded twice throws `DBOSWorkflowExecutionConflictException`, which
  the workflow task rethrows without persisting it as the workflow's result; a transaction
  step's loser rolls back its writes and returns the winner's output. A gated tool call runs
  at most once by its attempt marker (ADR 0009). A per-transaction fence (every write
  checking the holder) is not built: the window is bounded, and what it would guard is
  already guarded.
- A tool that reads the database without running workflows (`PromoteFacts`) takes the lock
  too, and so is refused while grit runs.
- No takeover: when the engine stops, attached processes wait; a grit started next takes
  the lock and recovers the waiting turns.
- Enforced by `EngineLockTests`: a second engine refused and the holder named, a dead row
  not named, two databases not contending, the heartbeat advancing, a running turn stopped
  (and left pending) when the lock's session is terminated, nothing dequeued after, the lock
  released only after running bodies return, and a deleted row stopping the engine; and by
  `AttachLiveTests`: an attached link's turn run by the engine, the holder named and then
  gone, an edge of another epoch refused.
