# grit.job

Jobs (ADR 0029): a job's run, and what starts it. Written against `grit.core`'s traits and
`Durable`, never DBOS, so its tests run over core's in-memory fakes (`InMemoryDurable`,
`InMemoryInbox`, `InMemorySchedules`). It names no `grit.turn`: a run is a turn of its slot's
own conversation, but its body is a job's, not the model's.

- **`run`** — `Run`: the `run` workflow, one per turn of a slot's conversation, which the
  inbox started with the slot's opening (`grit.core.inbox.Inbox.startSlot`), on the turns'
  queue under that conversation (`grit.dbos.workflow.Runs`): `read-slot`, which slot it
  runs, its schedule's parameters and report, and the version it was started at (its
  opening's source, `grit.core.job.Slot.source`), recorded as a `SlotRead`; then `reply`,
  which first looks for the turn's reply by its fixed id, so a rerun after a later deploy
  returns the reply already kept; otherwise, under the job's current version, writes the
  job's reply as the turn's reply, its await where the schedule reports, and the slot marked
  replied, in one transaction, and under another version, or with the job gone, writes
  nothing. `RunJournal` holds the steps' recorded forms; `RunEnv` and `RunRecords` are what
  it works with. Its histories, under `test/histories/`, are replayed by `JobReplayTests`
  under the engine's epoch.
- **`clock`** — `ClockEdge`: grit's clock edge, which hosts no place. Each pass reads the
  schedules with a run in flight and those with a slot due at its clock's now
  (`grit.core.job.ScheduleStore`'s `inFlight` and `due`, at most `ClockEdge.Batch` of each, so
  neither starves the other) and starts each through the inbox
  at its job's version (`Inbox.startSlot`), as any edge starts a turn (ADR 0002). One the
  inbox fails is tried again next pass. The engine that holds the database's lock runs its
  passes one after another, every `ClockEdge.Every` (`grit.dbos.engine.Engine.every`).

The packages are siblings: neither imports the other.
