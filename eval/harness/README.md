# grit.eval.harness

The eval harness: a corpus of cases captured from a database, labelled by a person, run
through a variant of a shipped call, and scored. It measures; it gates nothing. It runs only
through `scripts/eval`, never from a test or `scripts/check`, and its tests test the
instrument, not a model.

Every file it writes is text-free: ids, digests, numbers, lengths and field names. A corpus's
text stays in Postgres, in the database the corpus was restored to.

Packages, each importing only those above it:

- **`main`** — `Main`, the command line `scripts/eval` runs.

No source file sits at the module's root.
