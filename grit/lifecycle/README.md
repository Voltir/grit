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
  with. Imports nothing else in lifecycle.
- **`post`** — `Posting`: one run posting closed periods to a plugin from its cursor
  (`grit.core.plugin`), on a queue of its own partitioned by plugin (`grit.dbos.workflow.Posts`):
  each step posts the next closed period and moves the cursor past it in one `Jot`
  transaction, so a plugin's `Left` rolls back what it wrote and leaves the cursor; at most
  `MaxPerRun` a run. `PostEnv` is what it works with. Imports nothing else in lifecycle.

`close` and `post` never name each other. Their replay gate, `LifecycleReplayTests`, with
`RecordLifecycleHistories` writing its histories, is in the test tree's `replay`, which
covers both.

No source file sits at the module's root, and the test tree mirrors it, with `replay` beside.
