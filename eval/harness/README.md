# grit.eval.harness

The eval harness: a corpus of cases captured from a database, labelled by a person, run
through a variant of a shipped call, and scored. It measures; it gates nothing. It runs only
through `scripts/eval`, never from a test or `scripts/check`, and its tests test the
instrument, not a model.

Every file it writes is text-free: ids, digests, numbers, lengths and field names. A corpus's
text stays in Postgres, in the database the corpus was restored to.

Packages, each importing only those above it:

- **`corpus`** — a corpus, text-free: `CaseId`, a heard message's identity outside any one
  database (its Slack channel and ts); `Case`, what a heard message was live, the digests of
  the inputs the shipped builders rebuild for it, and how it clusters; `Manifest`, the
  corpus as a whole (its dump, the settings and tuning in force, the builders' constants,
  the build that captured it); `CorpusJson`, their files' JSON; `Capture`, a `Corpus` read
  from a restored database through `grit.dbos`'s `Reader` and rebuilt through the shipped
  builders (`TriageInput`, `TriageQuestion`, `Stitching`), never through SQL of its own.
  `Fields`, the reader every file's JSON is read through.
- **`log`** — a run's log, text-free and generic over what a call returned: `Header` (the
  corpus, variant, build, cap and repeats a run was started with), `Row` (one request: its
  case, digest, cache key, `Outcome`, usage and latency), `Footer` (what it spent), and
  `Weights`, a classifier's answer by position, never by key; `LogJson`, their JSON lines;
  `CacheKey` and `Cache`, answers kept as files under their request's key, so an unchanged
  request is never paid for twice. ← `corpus`
- **`main`** — `Main`, the command line `scripts/eval` runs. ← `corpus`

No source file sits at the module's root.
