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
