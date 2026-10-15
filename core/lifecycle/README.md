# grit.lifecycle

The engine's own workflows besides the turn (ADR 0011). Each is a flow of step values
(ADR 0038), its `body` that flow run over `Durable`, never DBOS, so its tests run them over
core's in-memory fakes, and its histories are read over the same flow. Not in
`grit.turn`: that module is the turn's body, and its epoch's replay gate is the turn's; each
workflow here has its own recorded histories, replayed under the engine's epoch
(`Turn.Epoch`). Each workflow's steps and the environment it works with are the code's
(`scripts/outline area lifecycle`); this file holds the package order and what the steps'
signatures do not say. A workflow's classifier call is a move (`grit.act.moves.Judging`):
admitted under the deployment's daily cap, unavailable when over it, and its cost kept in the
ledger, charged to its turn under the workflow's own id. The close's summary model call is not
yet one, nor are `Mentions`' asks: the turn makes them inside its own `weigh` and `route` steps
and keeps their costs in the ledger under itself.

- **`transcript`**: a period's entries as a classifier and the summary model read them. What
  its windows showed from other conversations a close treats as known, never as its own; a
  citation's line label is its ground, but for a line believed below `Outside`, which grounds
  nothing (ADR 0018). Imports nothing else in lifecycle.
- **`close`**: one attempt to close a period on its deadline, on the turns' queue under its
  conversation. A period that did not earn a written closing closes with no classifier or model
  call; a job's run's period closes with its opening as its prose. With nothing new, or when the model fails, the per-turn summaries
  are the prose. A period grit only heard is written as reported speech, and nothing resting
  only on heard lines is kept as Standing. Sealing marks its raw entries, the closing it
  replaces and a conversation going quiet for deletion (ADR 0014). ← `transcript`
- **`settle`**: the one question whether anyone is waiting on a quiet period, asked once per
  quiet stretch; a verdict of nobody at or above the threshold becomes the period's deadline. A
  classifier that fails is a verdict too, so nothing is asked again before new activity.
  ← `transcript`
- **`stitch`**: the placement of a thread's first message among its room's exchanges, one at a
  time per room in the order the inbox queued them (ADR 0023). Imports nothing else in
  lifecycle.
- **`triage`**: what a heard message is, asked once per message on the turns' queue, so ahead of
  any later close of it (ADR 0020), and whether grit drafts a reply, or answers one put to it by
  name (ADR 0022). A thread's first message waits for its placement first. A classifier that
  fails leaves unanswered tags, and no draft. It writes no entry, so it never moves a deadline.
  Each question set it has asked stays in the code (`TriageQuestions`), so recorded tags stay
  readable; `TriageQuestions.shipped` is the one live triage asks. `Mentions` puts that set to a
  message said to grit, waiting a bounded time for its placement and its answer, and keeps no
  tags; it also asks a turn's root the task questions (`TaskQuestions`) over the same state, for
  the turn's routing. ← `transcript`
- **`shadow`**: a declared variant of triage's question asked of a message already triaged, on a
  queue of its own so it never delays a triage, recorded and never acted on. It rebuilds
  triage's state from the store as it stands now. How many run each day is the sweep's, within
  the variant's cap. ← `triage`
- **`post`**: one run posting closed periods to a plugin from its cursor, at most `MaxPerRun`.
  Each step finds the next period in one transaction, then posts it and moves the cursor in a
  second, opened for that conversation, so what it keeps is kept at the conversation's label,
  and a plugin's `Left` rolls back what it wrote and leaves the cursor. Imports nothing else in
  lifecycle.

`close`, `settle`, `stitch`, `triage` and `post` never name each other; `shadow` names
`triage` alone, whose question it asks again. Their replay gate, `LifecycleReplayTests`, with
`RecordLifecycleHistories` writing its histories, is in the test tree's `replay`, beside
their read gate, `LifecycleReadTests`.

No source file sits at the module's root, and the test tree mirrors it, with `replay` beside.
