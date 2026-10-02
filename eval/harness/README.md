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
  the build that captured it); `CorpusJson`, their files' JSON.
- **`main`** — `Main`, the command line `scripts/eval` runs.

No source file sits at the module's root.
