# grit.lifecycle

The engine's own workflows besides the turn (ADR 0011). Written against `grit.core`'s
seams and `Durable`, never DBOS, so its tests run them over core's in-memory fakes
(`InMemoryDurable`, `InMemoryPeriodStore`). Not in `grit.turn`: that module is the turn's
body, and its epoch's replay gate is the turn's; each workflow here has its own recorded
histories, replayed under the engine's epoch (`Turn.Epoch`).

- **`transcript`** — `PeriodTranscript`: a period's own entries, the transcript a
  classifier or the summary model reads, and what its windows showed from other
  conversations (`elsewhere`), which a close treats as known, never as its own. Imports
  nothing else in lifecycle.
- **`close`** — `Close`: one attempt to close a period, run on the turns' queue under its
  conversation (`grit.dbos.workflow.Closes`): `check` its deadline is still the attempt's,
  `gate` what of its closing is new beside the balance it opened with (`CloseGate`, one
  classifier call), `summarise` it (`ClosingSummary`: the summary role's prompt, shown the
  balance as already known under labels, and a tolerant reader of its flows and edits;
  with nothing new, or when the model fails, the per-turn summaries as the prose and no
  model call; the edits and the topics' applied to the balance, held to the cap in force), `seal` it with its closing entry. `CloseEnv` is what it works
  with. ← `transcript`
- **`settle`** — `Settle`: the one question whether anyone is waiting on a quiet period, asked
  once per quiet stretch, on the turns' queue under its conversation
  (`grit.dbos.workflow.Settles`): `check` the period is still quiet as the question was
  made and still to be asked, `ask` the classifier (`SettleQuestion`: nobody, the person, or
  something else), `record` the verdict, which one of nobody at or above the threshold turns into the period's deadline (`grit.core.period.Deadline`).
  A classifier that fails is a verdict too, so nothing is asked again before new activity.
  `SettleEnv` is what it works with. ← `transcript`
- **`post`** — `Posting`: one run posting closed periods to a plugin from its cursor
  (`grit.core.plugin`), on a queue of its own partitioned by plugin (`grit.dbos.workflow.Posts`):
  each step posts the next closed period and moves the cursor past it in one `Jot`
  transaction, so a plugin's `Left` rolls back what it wrote and leaves the cursor; at most
  `MaxPerRun` a run. `PostEnv` is what it works with. Imports nothing else in lifecycle.

`close`, `settle` and `post` never name each other. Their replay gate,
`LifecycleReplayTests`, with `RecordLifecycleHistories` writing its histories, is in the
test tree's `replay`, which covers all three.

No source file sits at the module's root, and the test tree mirrors it, with `replay` beside.
