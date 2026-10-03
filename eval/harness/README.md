# grit.eval.harness

The eval harness: a corpus of cases captured from a database, labelled by a person, run
through a variant of a shipped call, and scored. It measures; it gates nothing. It runs only
through `scripts/eval`, never from a test or `scripts/check`, and its tests test the
instrument, not a model.

It names grit's code alone, but for the kit's `.env` loader (`DotEnv`), through which a run
reads Jev's key as grit does.

Every file it writes is text-free (ids, digests, numbers, lengths and field names) but one: the
review file `scripts/eval inputs` writes under `.local/eval/review/`, for a person to read. A
corpus's text stays in Postgres, in the database the corpus was restored to.

Packages, each importing only those above it:

- **`stats`** — `Estimate`, a mean with its standard error clustered (CR1), its 95% interval
  and the least difference a paired comparison of its precision detects, over Student's t;
  pure, naming nothing of grit's.
- **`corpus`** — a corpus, text-free: `CaseId`, a heard message's identity outside any one
  database (its Slack channel and ts); `Case`, what a heard message was live, the digests of
  the inputs the shipped builders rebuild for it, and how it clusters; `Manifest`, the
  corpus as a whole (its dump, the settings and tuning in force, the builders' constants,
  the build that captured it); `CorpusJson`, their files' JSON; `Capture`, a `Corpus` read
  from a restored database through `grit.dbos`'s `Reader` and rebuilt through the shipped
  builders (`TriageInput`, `TriageQuestion`, `Stitching`), never through SQL of its own.
  `Fields`, the reader every file's JSON is read through.
- **`label`** — a person's labels of a corpus's cases, kept apart from the cases so a
  recapture keeps them: `Labels`, read from `labels.json` as the labelling tool writes it, and
  each case's `Labelled` (its kind, the three yes/no tags, `Context`, whether it states a fact,
  and its `Place`). ← `corpus`
- **`log`** — a run's log, text-free and generic over what a call returned: `Header` (the
  corpus, variant, build, cap and repeats a run was started with), `Row` (one request: its
  case, digest, cache key, `Outcome`, usage, latency, and the focus its message was said at),
  `Footer` (what it spent), and `Weights`, a classifier's answer by position, never by key; `LogJson`, their JSON lines;
  `CacheKey` and `Cache`, answers kept as files under their request's key, so an unchanged
  request is never paid for twice. ← `corpus`
- **`pull`** — `Pull`, what a deployment's database kept of the heard messages a corpus
  holds, read through `Reader` as run logs the scorers read beside offline runs: live
  triage's tags (variant `kept`, which keeps only the likeliest kind's probability), and
  each declared shadow's answers (`grit.lifecycle.shadow`); with the messages no
  corpus holds yet, and each shadow's that ended keeping nothing. ← `corpus`, `log`
- **`jev`** — the first suite, Jev's: `Variant`, what a run changes from the shipped call
  (the model, triage's wording or recipe, or the tuning), and `Variants`, those a run can name;
  `Inputs`, a case's questions rebuilt through the shipped builders, and `Drift`, how a
  rebuilt state compares to the corpus's; `Review`, a case's questions as rebuilt, text and
  all, for a person to read beside it (`scripts/eval inputs` writes them under
  `.local/eval/review/`, the one file the harness writes text to, and prints only counts);
  `Spend`, what a request costs before it is sent, and `Budget`, a run's spend under its
  cap. ← `corpus`, `log`
- **`run`** — `Run`, a run's calls asked of Jev through the shipped calls
  (`Classifier.around`): answered from the cache when it holds them, else asked, timed and
  kept, within the cap; `Repeats`, how many times each is asked. ← `corpus`, `log`, `jev`
- **`score`** — what a run's log says: `Answers`, its answers by case, each averaged over its
  repeats with their spread; `Scoring`, those answers beside the cases and their labels, by
  `Tag` (waiting, durable, helps), kind and place; the scorers over them (`Brier`,
  `Reliability`, `Sweep`, and `Cost`, a curve over the cost-of-error ratio), each a mean
  `Clustered` by exchange and by author and `Split` by the label `context`; `Agreement`,
  `Repeated` and `Spending`, what needs no label; `Order`, cases in labelling order, by the difference between two runs or by a run's repeat spread, the unlabelled first; `Rule`, an adoption rule written before the run it judges, and `Decision`, what it makes of two runs, refused under the paired MDE or the rule's least; `Comparison`, two runs' cases by what became of each (`Changes`: fixed, broken, moved); `Changed`, the cases two runs asked triage of by different requests, `Size`, how large a run's triage calls were, and `Apart`, what Jev's repeat noise alone reads as a difference; `Spread`, how far
  apart its repeats answered, and how far its answers are from what was kept live; `Noise`,
  Jev's repeat spread on each triage question, and `MovedOn`, the cases whose state moved on
  since live triage asked, where a replica shadow stands from live beyond it.
  ← `stats`, `corpus`, `label`, `log`
- **`report`** — `Report`, a run scored (`Scored`: its log, its corpus's cases, the labels in
  force) and two runs compared, as markdown with every mean's interval; text-free. With no case
  labelled, only what needs no label: spend and latency (`score.Spending`), the repeats' spread
  (`score.Repeated`) and agreement with what was kept live (`score.Agreement`). ← `stats`,
  `corpus`, `label`, `log`, `pull`, `score`
- **`main`** — `Main`, the command line `scripts/eval` runs. ← every package above

No source file sits at the module's root.
