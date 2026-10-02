# grit.lifecycle

The engine's own workflows besides the turn (ADR 0011). Written against `grit.core`'s
seams and `Durable`, never DBOS, so its tests run them over core's in-memory fakes
(`InMemoryDurable`, `InMemoryPeriodStore`). Not in `grit.turn`: that module is the turn's
body, and its epoch's replay gate is the turn's; each workflow here has its own recorded
histories, replayed under the engine's epoch (`Turn.Epoch`).

- **`transcript`** — `PeriodTranscript`: a period's own entries, the transcript a
  classifier reads, and what its windows showed from other conversations (`elsewhere`),
  which a close treats as known, never as its own; `Labelled`, the transcript the summary
  model reads and cites into, each line labelled (`u`, `h` for a heard one, `a`, `t`, a
  tool call as one clipped line), a person's under their name, and the ground a citation gives (ADR 0018). Imports nothing else in
  lifecycle.
- **`close`** — `Close`: one attempt to close a period, run on the turns' queue under its
  conversation (`grit.dbos.workflow.Closes`): `check` its deadline is still the attempt's,
  and whether it earned a written closing (`grit.core.triage.Earning`; one that did not
  closes `Unearned`, with no classifier or model call and a fixed line as its prose),
  `gate` what of its closing is new beside the balance it opened with (`CloseGate`, one
  classifier call), `summarise` it (`ClosingSummary`: the summary role's prompt, shown the
  balance as already known under labels and the period's labelled transcript, and a
  tolerant reader of its flows and edits, each Standing item grounded by the lines it
  cites (ADR 0018);
  with nothing new, or when the model fails, the per-turn summaries as the prose and no
  model call; a period grit only heard that earned its closing written by the heard pin,
  as reported speech, and nothing resting only on heard lines kept as Standing; the edits and the topics' applied to the balance, held to the cap in force), `seal` it with its closing entry, and the tombstones on its raw entries, on the closing it
  replaces and on its conversation going quiet (ADR 0014). `CloseEnv` is what it works
  with. ← `transcript`
- **`settle`** — `Settle`: the one question whether anyone is waiting on a quiet period, asked
  once per quiet stretch, on the turns' queue under its conversation
  (`grit.dbos.workflow.Settles`): `check` the period is still quiet as the question was
  made and still to be asked, `ask` the classifier (`SettleQuestion`: nobody, the person, or
  something else), `record` the verdict, which one of nobody at or above the threshold turns into the period's deadline (`grit.core.period.Deadline`).
  A classifier that fails is a verdict too, so nothing is asked again before new activity.
  `SettleEnv` is what it works with. ← `transcript`
- **`triage`** — `Triage`: what a heard message is, asked once per message, on the turns'
  queue under its conversation (`grit.dbos.workflow.Triages`), so ahead of any later close
  of it: when it is its thread's first message, `stitch` it to an exchange in its room and
  `record-stitch` the placement (`grit.core.stitch`, ADR 0023), then `ask` the classifier
  (`TriageQuestion`: its kind, and whether someone waits on it, whether it states something
  worth keeping, whether a reply would help, asked in a `Wording`, the shipped one in
  `Wording.Shipped`) over the message, who said it and the thread
  before it, its strand first, then `record` the tags (`grit.core.triage`), then
  `consider` whether grit drafts a reply (`Speak`, over `grit.core.speech`) and, when it does,
  `start` the heard message's own turn (ADR 0022). A classifier that fails leaves unanswered
  tags, and no draft. It writes no entry, so it never moves a deadline. `TriageEnv` is what
  it works with. ← `transcript`
- **`post`** — `Posting`: one run posting closed periods to a plugin from its cursor
  (`grit.core.plugin`), on a queue of its own partitioned by plugin (`grit.dbos.workflow.Posts`):
  each step posts the next closed period and moves the cursor past it in one `Jot`
  transaction, so a plugin's `Left` rolls back what it wrote and leaves the cursor, and marks the runs from
  the cursor it started at for deletion; at most
  `MaxPerRun` a run. `PostEnv` is what it works with. Imports nothing else in lifecycle.

`close`, `settle`, `triage` and `post` never name each other. Their replay gate,
`LifecycleReplayTests`, with `RecordLifecycleHistories` writing its histories, is in the
test tree's `replay`, which covers them all.

No source file sits at the module's root, and the test tree mirrors it, with `replay` beside.
