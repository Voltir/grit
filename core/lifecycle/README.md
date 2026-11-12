# grit.lifecycle

The engine's own workflows besides the turn (ADR 0011). Each is a flow of step values
(ADR 0038), its `body` that flow run over `Durable`, never DBOS, so its tests run them over
core's in-memory fakes, and its histories are read over the same flow. Not in
`grit.turn`: that module is the turn's body, and its epoch's replay gate is the turn's; each
workflow here has its own recorded histories, replayed under the engine's epoch
(`Turn.Epoch`). Each workflow's steps and the environment it works with are the code's
(`scripts/scout area lifecycle`); this file holds the package order and what the steps'
signatures do not say. A workflow's classifier calls and the close's summary model call are
moves (`grit.act.moves.Judging`): admitted under the deployment's daily cap, unavailable when
over it, and each one's cost kept in the ledger, charged to its turn under the workflow's own id.
`Mentions`' asks are not: the turn makes them inside its own `weigh` and `route` steps and
keeps their costs in the ledger under itself.

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
  time per room in the order the inbox queued them (ADR 0060). Imports nothing else in
  lifecycle.
- **`triage`**: what a heard message is, asked once per message on the turns' queue, so ahead of
  any later close of it (ADR 0020), and whether grit drafts a reply, or answers one put to it by
  name (ADR 0022). A thread's first message waits for its placement first. A classifier that
  fails leaves unanswered tags, and no draft. It writes no entry, so it never moves a deadline.
  Recorded tags stay readable by the names and gates of each question set it has asked, kept
  in core (`Tags`); `TriageQuestions.shipped` is the one live triage asks. `Mentions` puts that set to a
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

## The close's steps

1. `check`: the period, under its conversation's lock: already closed; abandoned when its
   deadline (`Deadline`, under the settings in force) is no longer the attempt's, because a turn
   came in, a turn's entries were written or the settings changed, so the sweep makes a new
   attempt on the new deadline; otherwise due, for its reason, with the balance it opened with
   and the cap its closing's balance is held to. The reason is `Ran`, whatever the deadline's,
   when the period's conversation is a job's run (`Slot.of` its origin); otherwise `Unearned`,
   whatever the deadline's, when the period did not earn a written closing (`Earning`, over what
   triage made of its heard messages). The attempt is taken to be enqueued once its deadline had
   come.
1. `ask:gate`: what of the closing is new beside the balance the period opened with and what its
   windows showed from other conversations (`CloseGate`); every part when the classifier does not
   answer; none, with no classifier call, when unearned or ran.
1. `ask:summary`: the closing: its flows written by the catalog's summary pin
   (`ClosingSummary`), shown what the period drew from elsewhere as known, and the balance it
   opened with after the writer's edits and the items' (`EntryAgenda.edits`), held to the cap
   (`Balance`); when the model fails, writes nothing readable or is cut off at its token limit,
   or when the gate found nothing new, the day's spend reached its cap or no pin of its role is
   cleared for what its period reads now (`Routed`; and then no model is called), the period's
   per-turn summaries joined as the prose, and the balance carried with the agenda's edits alone.
   A period grit only heard (`PeriodTranscript.overheard`) that earned its closing is written by
   the catalog's heard pin, as reported speech, asking only its prose and outcome; one that did
   not is closed with no model call, its prose `Heard 4 messages; nothing kept.` and its balance
   carried. A run's period is closed with no model call: its prose the run's opening, its outcome
   the run's reply verbatim (`No reply.` without one), and its balance carried unchanged. Its
   model is admitted by the day's spend as the gate's classifier is (`Judging`), and every reply
   is charged, however it reads, in `ask:summary:record`; a store that cannot record it ends the
   close there, sealing nothing.
1. `seal`: under the lock again: the closing entry, the period closed, and the tombstones on its
   raw entries, on the closing it replaces and on its conversation going quiet (`Target`),
   together; abandoned, writing nothing, when a turn came in meanwhile.

## The triage's steps

1. `stitched`: when the heard message is an `Opening`, its placement waited for
   (`Placing.placed`), so the message is asked about with its strand, and only once every opening
   heard before it in its room is placed; then, opening or not, until `Triage.Wait` after it was
   said. Its conversation's later triages queue behind it meanwhile, so its author's next message
   said by then is seen in `consider`.
1. `ask:triage`: one classifier call over the heard message, who said it, and the thread before
   it: its strand's, then its own (`TriageInput.heard`), asked the questions live triage asks
   (`env.questions`, `TriageQuestions.shipped` in a deployment), one per corpus covering its
   conversation where the set asks so; a move (`Judging`): admitted against the day's cap, so one
   over it is unanswered; an absent or failing classifier, or an answer that does not read, is
   `Unanswered` tags. Nothing is asked when the message cannot be read or is gone, and a message
   that says nothing (`Payload.saysNothing`) is `Unanswered` ("nothing was said") unasked.
   `ask:triage:record`: when the classifier replied, its cost in the ledger, charged to the heard
   message's turn; a store that fails it fails the triage.
1. `record`: the tags kept (`TriageStore.record`); ignored when the message is gone or already
   tagged.
1. `consider`: when it was tagged now, whether grit drafts a reply to it, or answers it as said
   to grit (`Speech.decide`, against the speech ledger), the decision kept, held, drafting or
   answering, in one transaction, with, when answering, its reply awaited and its mark wanted at
   its reply address; a deployment that does not speak keeps none but on a message put to grit.
1. `start`: when drafting or answering, the heard message's own turn started, a turn rooted on it
   (`Payload.Heard`).
1. `unstarted`: when drafting and its start failed, the decision settled as failed
   (`SpeechStore.drafted`).

## Layout

No source file sits at the module's root, and the test tree mirrors it, with `replay` beside.
