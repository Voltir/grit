# grit.job

Jobs (ADR 0029): a job's run, and what starts it. Written against `grit.core`'s traits and
flows and `grit.act`'s moves, never DBOS, so its tests run over core's in-memory fakes
(`InMemoryDurable`, `InMemoryInbox`, `InMemorySchedules`). It names no `grit.turn`: a run is a turn of its slot's
own conversation, but its body is a job's, not the model's.

- **`run`**: `Run`, the `run` workflow, a flow, one per turn of a slot's conversation, on the
  turns' queue under that conversation. It reads which slot it runs and the version it was
  started at, then under the job's current version runs the job as a program, whose moves are made for its
  schedule's principal and start believed at its schedule's integrity (ADR 0036), each ask
  admitted against the day's cap, no call asking a person first.
  `reply` first looks for the turn's reply by its fixed id, so a rerun after a later deploy
  returns the reply already kept; otherwise, under the current version, it writes the reply, its
  await where the schedule reports, and the slot marked replied in one transaction, and under
  another version, or with the job gone, writes nothing. A run resumed under another version
  after it made moves ends in error at `reply`, and the clock edge supersedes it (ADR 0034:
  moves are not reused across versions). Its histories, under `test/histories/`, are replayed
  by `JobReplayTests` under the engine's epoch, and read over its flow by `JobReadTests`.
- **`clock`**: `ClockEdge`, grit's clock edge, which hosts no place. Each pass starts, through
  the inbox and at its job's version, the schedules with a run in flight and those with a slot
  due, at most `ClockEdge.Batch` of each, so neither starves the other, as any edge starts a turn
  (ADR 0002). One the inbox fails is tried again next pass. The engine that holds the lock runs
  its passes one after another, every `ClockEdge.Every`.

The packages are siblings: neither imports the other.
