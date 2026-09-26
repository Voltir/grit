# grit.lifecycle

The engine's own workflows besides the turn (ADR 0011). Written against `grit.core`'s
seams and `Durable`, never DBOS, so its tests run them over core's in-memory fakes
(`InMemoryDurable`, `InMemoryPeriodStore`). Not in `grit.turn`: that module is the turn's
body, and its epoch's replay gate is the turn's; each workflow here has its own recorded
histories, replayed under the engine's epoch (`Turn.Epoch`).

- **`close`** — `Close`: one attempt to close a period, run on the turns' queue under its
  conversation (`grit.dbos.workflow.Closes`): `check` it is due and no turn came in,
  `gate` which sections its closing needs (`CloseGate`, one classifier call), `summarise`
  it (`ClosingSummary`: the summary role's prompt and a tolerant reader, falling back to
  the per-turn summaries), `seal` it with its closing entry. `CloseEnv` is what it works
  with; `CloseReplayTests` and `RecordCloseHistories` are its replay gate. Imports nothing
  else in lifecycle.

No source file sits at the module's root, and the test tree mirrors it.
