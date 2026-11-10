# grit.eval.harness

The eval harness: a recorded turn read back as its model was sent it, the coder bench and the proposer of amendments to its standing text, and the lost-tool measure.
It measures; it gates nothing. It runs only through `scripts/eval` and `scripts/integrity-loss`,
never from a test or `scripts/check`, and its tests test the instrument, not a model.

It names grit's code alone (core, `grit.dbos`'s `Reader`, and its `Engine` to keep the bench's
trials alone, the turn's reading of its records, the assembler's estimate and the shipped
linear assembler, the lifecycle's triage builders, and the models' Anthropic pricing and Jev
client), but for the kit's `.env` loader (`DotEnv`), through which a run reads its key, as grit
does, and the local host, which only `main` names.

Every file it writes is text-free (ids, digests, numbers, lengths and field names) but those for
a person or a proposer to read: the review files `scripts/eval context` writes under
`.local/eval/review/`, and the checkout `scripts/eval propose` writes, which shows a kept
trial's tool results. The bench keeps each trial's transcript and diff in its own database and
blob store.

A heard message's triage request, rebuilt for `scripts/eval context`, reads its pool through
`Reader`, which reads every row: a restored database records no deployment's groups, so the
records near a message that it shows may be ones the live read was not cleared for. Those lines
reach only the review file above.

Every price it prints is as core's `Cost.cents` writes it (`0.29¢`).

Its types are the code's (`scripts/scout area eval`); this file holds the package order and
the rules each package keeps that its signatures cannot state.

Packages, each importing only those above it:

- **`stats`**: proportions with their intervals, clustered, over Wilson's interval. Pure,
  naming nothing of grit's.
- **`bench`**: the coder bench's keeping: each trial of a harness at a frozen task, its
  transcript, its diff and, for a grit trial, the standing text its brief began with, by digest, kept through `grit.dbos.internal`'s `TrialRows` over a blob store that is the
  bench's alone, one per bench database; a frozen task (`eval/bench/tasks/<name>/`: its base,
  its commits, its checks and its brief), and its checkout, a repository holding only the base's
  history, through core's `Processes`; the rig, which runs a coder on a task there and measures
  its trial (a Claude Code coder, whose `stream-json` output is the transcript it reads and
  keeps), priced by the seed's prices (`grit.models`' `AnthropicRates.pricing`), the one pricing of both arms. Only
  `main` names `grit.host`, the harness's composition root (the law's
  `harness-names-host-from-main-alone`). It reports cents per passing trial, reading the trials
  through a read-only `Reader`, and gates nothing. Its summary counts cost over failed trials
  too, takes distance over passing trials only, and does not score quality yet.
- **`capture`**: what `sent` and `loss` share and the declarations `scripts/eval context` reads:
  how a read through `Reader` failed, said alike by both, and a deployment's persona and
  corpora, read from a hand-written `persona.json` and `knowledge.json` (grit's database keeps
  no declaration).
- **`sent`**: one recorded turn as its model was sent it, text and all, for a person to read
  (`scripts/eval context`), and what Jev was asked of a heard message, rebuilt through the
  shipped builder since triage keeps no request. It measures each call too: its cache
  misses priced as reads through `grit.models`' `AnthropicRates` (its second namer after
  `bench`), its input by part, and what is unreferenced (no later output refers to it), a
  proxy only: a part can steer a model without being quoted, so cutting it and rerunning the
  task decides. A task's boundaries can be redrawn by this build's assembler, each first call
  rebuilt under the window drawn afresh, so a change to what a boundary draws is seen in
  tokens before a model is paid to run it. ← `capture`
- **`evidence`**: what a proposer of amendments to the coder's standing text is given to read: a
  frozen task blind to its grading, an evidence checkout's files, and the patterns an amendment
  says its coder will be seen acting on, matched over a kept trajectory's lines. Nothing a grade
  reads reaches a proposer but through `Shown`, whose scan refuses the bench's own grade shapes:
  a file the checkout is to hold is built into one, never written beside it. A proposer's checkout
  shows each tool result of a run cut where `sent` cuts a call's; `scripts/eval propose` writes one
  of one task's chosen grit trials, keeping the newest three. ← `bench`, `sent`
- **`loss`**: the lost-tool measure (`scripts/integrity-loss`): over a window of a database's
  turns, read from their recorded gate steps by the turn's own codec, which conversations had a
  tool withheld, a tool that writes out withheld, and a turn lost, one whose root asks for a
  write out, as Jev judges the root alone, asked only when a write-out tool was withheld; and the
  items each turn left hidden, closing records apart. It reads, never writes, and keeps nothing.
  ← `stats`, `capture`
- **`main`**: the command line `scripts/eval` and `scripts/integrity-loss` run. ← every package
  above

No source file sits at the module's root.
