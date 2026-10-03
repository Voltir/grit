# grit.eval.harness

The eval harness: a corpus of cases captured from a database, labelled by a person, run
through a variant of a shipped call, and scored. It measures; it gates nothing. It runs only
through `scripts/eval`, never from a test or `scripts/check`, and its tests test the
instrument, not a model.

It names grit's code alone (core, `grit.dbos`'s `Reader`, and its `Engine` to write the
synthetic reference's database alone, the turn's reading of its records,
the assembler's estimate and the shipped retrieval assembler, the lifecycle's builders, the
models' Jev and OpenRouter clients, and `grit.eval`'s hand-written cases and their layout),
but for the kit's `.env` loader (`DotEnv`), through which a run reads Jev's key, and a rebuild
OpenRouter's, as grit does.

Every file it writes is text-free (ids, digests, numbers, lengths and field names) but the
files for a person to read: the review file `scripts/eval inputs` writes under
`.local/eval/review/`, and the reply reviews `scripts/eval replies` writes under
`.local/eval/replies/`, which print only ids and counts unless asked to show the file; and
the search queries a rebuild's window-only cases had written, each kept in the cache under
its request's key and never printed. A corpus's text stays in Postgres, in the database the
corpus was restored to.

Packages, each importing only those above it:

- **`stats`** — `Estimate`, a mean with its standard error clustered (CR1), its 95% interval
  and the least difference a paired comparison of its precision detects, over Student's t;
  and `Proportion`, how many items hit, with Wilson's 95% interval on their effective number
  (n over the clustered design effect), none under `Proportion.MinClusters` clusters; pure,
  naming nothing of grit's.
- **`corpus`** — a corpus, text-free: `CaseId`, a heard message's identity outside any one
  database (its Slack channel and ts); `Case`, what a heard message was live, the digests of
  the inputs the shipped builders rebuild for it, and how it clusters; `Manifest`, the
  corpus as a whole (its dump, the settings and tuning in force, the builders' constants,
  the build that captured it); `CorpusJson`, their files' JSON; `Capture`, a `Corpus` read
  from a restored database through `grit.dbos`'s `Reader` and rebuilt through the shipped
  builders (`TriageInput`, `TriageQuestions`, `Stitching`), never through SQL of its own.
  `TurnCase`, one turn the database recorded, keyed by its workflow, TUI and task turns as
  well as Slack ones (`Said`, the message it answers by id): its offer, its window by part,
  its tool loop's rounds (each call by the tool it named: an offered one, the turn's own
  `topic` tool, or neither), how it ended, its ledger rows by role and what became of a heard
  root's draft, read by `TurnCapture` through `Reader` and the turn's own reading of its
  records (`grit.turn.TurnRecord`), each window part costed by `CharEstimate` over `Shown`
  (`TurnCapture.costed` costs a rebuilt window the same way, and `TurnCapture.schema` a set of
  tool definitions);
  `Support`, how much of a reply a part carries, computed where the text is and kept as the
  number, the line at which a part counts as used (`Support.Used`), and a window's most
  supported parts (`Support.top`); `TurnJson`, a turn's line of `turns.jsonl`. `Fields`, the reader every file's JSON
  is read through.
- **`label`** — a person's labels of a corpus's cases, kept apart from the cases so a
  recapture keeps them: `Labels`, read from `labels.json` as the labelling tool writes it, and
  each case's `Labelled` (its kind, the three yes/no tags, `Context`, whether it states a fact,
  and its `Place`); and `Verdicts`, the verdict a review's rater left standing on a picked
  message (`Rated`: the shadow it was picked against, why, the verdict, the rater's id and
  when), as `pull` writes them. Labels judge the inputs a call was given, verdicts the speech
  decision at its moment: the two are reported side by side, never merged. And replies'
  labels, kept by the workflow of the turn that wrote each (`ReplyLabels`, read from and
  written to `reply-labels.json`): each a `ReplyLabel` under a `ReplyGuide` this build knows,
  of the reply's `Quality`, where its answer was `Found` (in records named by `Locator`, a
  window part by ids alone, or the thread, elsewhere, nowhere, or nothing asked), and whether
  grit should have spoken; a person's note is never read into one; `Locator.held`, whether a
  window's parts show what a locator names. ← `corpus`
- **`log`** — a run's log, text-free and generic over what a call returned: `Header` (the
  corpus, variant, build, cap and repeats a run was started with, and a question set's
  names), `Row` (one request: its case, digest, cache key, `Outcome`, usage, latency, and the
  focus its message was said at), `Footer` (what it spent), and `Weights`, a classifier's
  answer by position, never by key, and `Log.named`, a log by position read under its
  questions' names; `LogJson`, their JSON lines, and `LogJson.named`, a question set's
  answers under their names as live's and a shadow's rows keep them;
  `CacheKey` and `Cache`, answers kept as files under their request's key, so an unchanged
  request is never paid for twice. ← `corpus`
- **`pull`** — `Pull`, what a deployment's database kept of the heard messages a corpus
  holds, read through `Reader` as run logs: live triage's tags (variant `kept`), under the
  names its question set asked them by, the rows of another set's names left out and counted,
  and each declared shadow's answers (`grit.lifecycle.shadow`) as a `ShadowLog`: a wording's by
  position, a question set's under its names, or refused when a name kept both; with the
  messages no corpus holds yet, and each shadow's that ended keeping nothing; and
  `Pull.verdicts`, the verdicts standing on reviewed messages, by case. ← `corpus`, `label`,
  `log`
- **`jev`** — the first suite, Jev's: `Variant`, what a run changes from the shipped call
  (the model, triage's wording, its recipe and the words it is asked in, or the tuning), and `Variants`, those a run can name;
  `Asking`, how a question is put to Jev through the shipped call: v1 in a wording,
  stitching's, or a question set's with the knowledge sources it asks about;
  `QuestionSet`, a question set a comparison names (its gate, and its questions read as
  durable and as `to`), and `Sets`, those it can name (`v1`, the set live triage asked before
  v2, and `v2`, live's since);
  `Inputs`, a case's questions rebuilt through the shipped builders, and `Drift`, how a
  rebuilt state compares to the corpus's; `Review`, a case's questions as rebuilt, text and
  all, for a person to read beside it (`scripts/eval inputs` writes them under
  `.local/eval/review/` and prints only counts);
  `Spend`, what a request costs before it is sent, and `Budget`, a run's spend under its
  cap. ← `corpus`, `log`
- **`reply`** — `ReplyReview`, a few turns' replies for a person to label: turns named or
  picked (`Ask`, `By`), at most five, refused past that; each shown as the message it
  answers, its reply's text, and at most three of its window's parts best supported first,
  cut at 800 characters, never the turn's own thread, a tool result, reasoning or the query;
  rendered as a file of label stubs, and the labels read back from it once filled.
  `Rebuild`, a recorded turn's window drawn again by the shipped retrieval assembler, built as
  `Assembled` says, at a `Width`, over `AsOf`: the restored database's stores as they stood
  when the turn's `assemble` step started (rows written later left out, the periods open
  elsewhere and the closings kept as they were then), its query replayed from the one it
  recorded; `Drift`, the rebuilt window against the recorded one by ids (same, a section from
  elsewhere changed, own entries changed, both, or gone to a purge). `WindowOnly`, a heard
  message live triage read as asking that no turn answered, its window rebuilt as of
  triage's answer; `Queries`, its query writes, each kept in a `Cache` under its request's
  key and asked under a `Budget`. `TurnVariant`, a named way to build a turn other than as
  shipped: the window each kind of turn is drawn at (`Sizing`; a variant past the reply
  model's context is refused), and what it is offered of its tools (`Offering`): a service's
  tools withheld when triage answered every knowledge source the service supplies below a
  line, the link from source to service given as `Supplies`, since grit keeps none.
  `Reference`, what turns' builds are expected to do, by ids alone (`Expect`: a window holds
  what a locator names, a service's tools are offered, or withheld), from a person's reply
  labels (an answer found in records shown) and a hand-written file, each turn `Judged` under
  a variant. `TurnTriage`, live triage's question set put to the message a recorded turn
  answers, built by the shipped builder over the database as it stood when the turn started,
  a message said to grit read as one heard (`TurnAsk`). `Synthetic`, `grit.eval`'s cases as a
  reference: each case in each variant a turn said to grit in the synthetic database, found by
  its conversations' origins as `grit.eval.Layout` lays them out (`Asked`), each conversation's
  `[must]` entries expected held, a case labelling none left out; its window drawn as `Rebuild`
  draws one, over the database as it stands, searched with the case's own query, within the
  case's scope (`AsOf.settled`, the lifecycle settings read as given). ← `corpus`, `label`,
  `log`, `jev`
- **`run`** — `Run`, a run's calls asked of Jev through the shipped calls
  (`Classifier.around`): answered from the cache when it holds them, else asked, timed and
  kept, within the cap; `Repeats`, how many times each is asked. ← `corpus`, `log`, `jev`
- **`score`** — what a run's log says: `Answers`, its answers by case, each averaged over its
  repeats with their spread; `Scoring`, those answers beside the cases and their labels, by
  `Tag` (waiting, durable, helps), kind and place; the scorers over them (`Brier`,
  `Reliability`, `Sweep`, and `Cost`, a curve over the cost-of-error ratio), each a mean
  `Clustered` by exchange and by author and `Split` by the label `context`, or a rate, its
  `Proportions` by exchange and by author; `Agreement`,
  `Repeated` and `Spending`, what needs no label; `Order`, cases in labelling order, by the difference between two runs or by a run's repeat spread, the unlabelled first; `Rule`, an adoption rule written before the run it judges, on one question or triage's four's mean (`Measure`), over every case or one context, with lines B may not cross elsewhere (`Guard`, judged by `Guarded`), and `Decision`, what it makes of two runs, refused under the paired MDE or the rule's least, or for a guard broken; `Comparison`, two runs' cases by what became of each (`Changes`: fixed, broken, moved); `Changed`, the cases two runs asked triage of by different requests, `Size`, how large a run's triage calls were, and `Apart`, what Jev's repeat noise alone reads as a difference; `Drafting`, one side of a draft comparison (a log's named answers, its set's gate, and the
  questions read as durable and as `to`); `Drafts`, a question set's draft against live
  triage's, each by its own set's gate (`Cells`, the 2×2 by focus), each of its
  probabilities' mean and spread (`Column`), and its `durable` against live's; `Judgement`, a
  review's verdicts against both drafts and each side's `to`, counts by the reason each
  message was picked; `Spread`, how far
  apart its repeats answered, and how far its answers are from what was kept live; `Noise`,
  Jev's repeat spread on each triage question, and `MovedOn`, the cases whose state moved on
  since live triage asked, where a replica shadow stands from live beyond it. `Structure`, a
  corpus's recorded turns read structurally over a `Slice` (all, a root, or where said): the
  turns that did not reply, the pass rate, rounds, tools offered against called (the turn's
  `topic` tool and unnamed calls apart), prompt and window tokens and the first call's
  estimate against the ledger (each as `Quantiles`), cost by what it paid for, the drafts'
  outcomes with their post and hold rates (each a `Proportion` clustered by thread), the review verdicts
  joined by case, and the window parts a reply `Used`. `Recipe`, a turn variant against
  shipped over the turns paired (`TurnPair`, what each is `Given` both ways): tools offered and
  their definitions' tokens saved, called-tool recall (of the tools a turn called, those the
  variant still offers), window tokens per part, used-section recall (of the parts a reply
  used, those the variant's window holds, `Locator.held`), and the turns it changed.
  ← `stats`, `corpus`, `label`, `log`
- **`report`** — `Report`, a run scored (`Scored`: its log, its corpus's cases, the labels in
  force) and two runs compared, as markdown with every mean's and rate's interval; text-free. With no case
  labelled, only what needs no label: spend and latency (`score.Spending`), the repeats' spread
  (`score.Repeated`) and agreement with what was kept live (`score.Agreement`). A question set's pulled
  shadow against live's log, each by its own set's gate (`Report.Side`), and against the
  verdicts given: `Report.drafts`, counts only. A corpus's recorded turns read structurally
  (`score.Structure`) beside the verdicts standing: `Report.turns`, the pass rate first, then
  each section over every turn, by root and by where said, then a line a turn by cost, with
  the used-part threshold stated as a heuristic and used-section recall stated undefined where
  no reply said anything. Turn variants against shipped: `Report.recipes`, each `Varied`
  variant's tools (called-tool recall first), window tokens by part, used-section recall
  (stated undefined when no reply used a part), the turns it changed, and the reference
  turns' pass rate under shipped and each variant. The synthetic reference judged under
  shipped and each variant (each turn a `CaseJudged`): `Report.synthetic`, each variant's pass
  rate clustered by case, and its failing turns by name. ← `stats`, `reply`,
  `corpus`, `label`, `log`, `pull`, `score`
- **`main`** — `Main`, the command line `scripts/eval` runs. ← every package above

No source file sits at the module's root.
