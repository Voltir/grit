# grit.eval.harness

The eval harness: cases captured from a database, labelled by a person, run
through a variant of a shipped call, and scored. It measures; it gates nothing. It runs only
through `scripts/eval` and `scripts/integrity-loss`, never from a test or `scripts/check`, and its tests test the
instrument, not a model.

It names grit's code alone (core, `grit.dbos`'s `Reader`, and its `Engine` to write the
synthetic reference's database and the stitch lab's alone, the turn's reading of its records,
the assembler's estimate and the shipped retrieval assembler, the lifecycle's builders, the
models' Jev and OpenRouter clients, the knowledge plugin's reading of a topic's own leaves,
and `grit.eval`'s hand-written cases and their layout),
but for the kit's `.env` loader (`DotEnv`), through which a run reads Jev's key, and a rebuild
OpenRouter's, as grit does.

Every file it writes is text-free (ids, digests, numbers, lengths and field names) but the
files for a person to read: the review files `scripts/eval inputs` and `scripts/eval context`
write under `.local/eval/review/`, the stitch lab's report, whose rows of openings show a
synthetic room's own text, and the reply reviews `scripts/eval replies` writes under
`.local/eval/replies/`, which print only ids and counts unless asked to show the file, the
summary comparison `scripts/eval summaries` writes under `.local/eval/review/`, and the
search queries a rebuild's window-only cases had written, each kept in the cache under
its request's key and never printed. A capture's text stays in Postgres, in the database the
capture was restored to.

A rebuilt triage input reads its pool through `Reader`, which reads every row: a restored
database records no deployment's groups, so the records near a message that it shows may be
ones the live read was not cleared for. Those lines reach only the review files above; the
run logs and the bound sweep keep their digests alone.

Its types are the code's (`scripts/scout area eval`); this file holds the package order and
the rules each package keeps that its signatures cannot state.

Packages, each importing only those above it:

- **`stats`**: estimates and proportions with their intervals, clustered, over Student's t and
  Wilson's interval. Pure, naming nothing of grit's. A report shows a price as core's
  `Cost.cents` writes it (`0.29¢`).
- **`bench`**: the coder bench's keeping: each trial of a harness at a frozen task and its
  transcript, kept through `grit.dbos.internal`'s `BenchRows` over a blob store that is the
  bench's alone, one per bench database; a frozen task (`eval/bench/tasks/<name>/`: its base,
  its commits, its checks and its brief), and its checkout, a repository holding only the base's
  history, through core's `Processes`; the rig, which runs a coder on a task there and measures
  its trial (a Claude Code coder, whose `stream-json` output is the trajectory it reads and
  keeps), priced by `grit.models`' `AnthropicRates`, the one function that prices both arms. Only
  `main` names `grit.host`, the harness's composition root (the law's
  `harness-names-host-from-main-alone`). It reports and gates nothing yet.
- **`capture`**: a capture, text-free: each heard message's case and each recorded turn, with the
  digests of the inputs the shipped builders rebuild for them, and the capture's manifest. Read
  from a restored database through `Reader` and the turn's own reading of its records, rebuilt
  through the shipped builders, never through SQL of its own. A file of an older format is
  refused: recapture. The corpora a capture's deployment declares are read from the
  `knowledge.json` written beside it (grit's database keeps no declaration), and only when the
  harness asks a turn.
- **`sent`**: one recorded turn as its model was sent it, text and all, for a person to read
  (`scripts/eval context`), and what Jev was asked of a heard message, rebuilt through the
  shipped builder since triage keeps no request. It reads its own records rather than through
  `capture`, whose readers keep numbers, never text. ← `stats`, `capture`
- **`label`**: a person's labels, kept apart from the cases so a recapture keeps them: case
  labels, replies' labels (a person's note is never read into one), review verdicts, and churn
  questions by id: each its gold (strings a right reply holds) and its asking in each round. Labels judge the inputs a call was given, verdicts the speech decision at its
  moment: the two are reported side by side, never merged. ← `capture`
- **`log`**: a run's log, text-free, a classifier's answers by position, never by key; and the
  cache of answers under their request's key, so an unchanged request is never paid for twice.
  ← `capture`
- **`pull`**: what a deployment's database kept of a capture's heard messages, read as run logs:
  live triage's tags, each declared shadow's answers, the verdicts standing, and each case's
  message rebuilt as live's speech decision weighed it, with the cues it kept. ← `capture`,
  `label`, `log`
- **`loss`**: the lost-tool measure (`scripts/integrity-loss`): over a window of a database's
  turns, read from their recorded gate steps by the turn's own codec, which conversations had a
  tool withheld, a tool that writes out withheld, and a turn lost, one whose root asks for a
  write out, as Jev judges the root alone, asked only when a write-out tool was withheld; and the
  items each turn left hidden, closing records apart. It reads, never writes, and keeps nothing.
  ← `stats`, `capture`
- **`jev`**: Jev's suite: the variants a run can name, how a question is put through the shipped
  call, a case's questions rebuilt and their drift from the capture, and a run's spend under its
  cap. ← `capture`, `log`
- **`reply`**: replies and windows. A reply review shows at most five turns, each with at most
  three window parts, cut at 800 characters, never the turn's own thread, a tool result,
  reasoning or the query. A rebuild draws a recorded turn's window again through the shipped
  retrieval assembler over the database as it stood when the turn's `assemble` step started,
  its query replayed, and of a turn that recorded its search, only what its recorded verdicts
  admit packed, at the turn's pass mark and count judged or lower ones a run names, no
  classifier asked. The pointer-lines variant, a turn's request again with a line naming each
  document its judge rejected and the call that reads it, is the eval's alone: nothing shipped
  shows a rejected candidate. Turn variants apply a `TurnRecipe` as a deployment declaring it would; a
  variant past the reply model's context is refused. The reference judges builds by ids alone,
  from reply labels, a hand-written file and `grit.eval`'s synthetic cases. The recall replay
  searches a turn's recorded query again over the database as it stood at the dump, its
  documents too (a turn draws them as of its root, which would hide every document written
  since), and ranks each document the turn followed by its current version, matched by plugin
  and key, since a resync supersedes every version it rewrites. It reads every row: a restored
  database records no deployment's groups, so it cannot read as the turn was cleared to. The
  duplication measure draws each window again with its documents as a database holds them
  now, its search run again whatever the turn recorded of it, since a recorded search could
  not find a topic written later. A summary comparison asks a turn's summary request again,
  rebuilt by the turn's own builder over its conversation as it stood when its summary step
  started, and writes the answers beside the live one for a person to read. ← `stats`, `capture`, `label`, `log`, `jev`
- **`run`**: a run's calls asked of Jev through the shipped calls, answered from the cache when
  it holds them, else asked, timed and kept, within the cap. ← `capture`, `log`, `jev`
- **`score`**: what a run's log says, beside the labels: calibration and cost scorers clustered
  by exchange and by author, adoption rules written before the run they judge, draft and
  review comparisons, a capture's turns read structurally, turn variants against shipped, and
  churn; a synthetic room's placings against the truth its topics give; and the bound sweep,
  named draft gates each decided by the shipped speech decision itself, never a copy of it. grit records no price, so each model's rates are read by least squares from its priced
  ledger rows. ← `stats`, `capture`, `label`, `log`
- **`report`**: a run scored, or two compared, as markdown with every mean's and rate's interval;
  text-free. With no case labelled, only what needs no label. ← `stats`, `reply`, `capture`,
  `label`, `log`, `pull`, `score`
- **`lab`**: the stitch lab: a synthetic room (`grit.eval`'s `Room`) heard through an engine's
  inbox into its own database, which `scripts/eval lab stitch` creates and drops, then each
  opening placed in the order said through the shipped stitch ask (`Stitching.placing`, the
  workflow's own), Jev asked one call at a time through `run`'s cache and cap, and each
  placement kept, so a later opening sees what an earlier one linked. ← `capture`, `log`,
  `jev`, `run`, `score`
- **`main`**: the command line `scripts/eval` runs. ← every package above

No source file sits at the module's root.
