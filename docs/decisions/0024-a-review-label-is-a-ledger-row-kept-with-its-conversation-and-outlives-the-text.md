# 0024. A review label is a ledger row kept with its conversation, and outlives the text it was given on

Status: accepted (2026-10-02)

Context: grit asks a person, by a reaction on a prompt posted in a private place, whether a
reply to a heard message would have been welcome (`grit.core.review`). The prompt links to
the message and never shows it. Each verdict is a label for measuring a shadow's gate
against live triage's, and a corpus of them is built over weeks. Where the label lives was a
real choice:

- **Beside its entry, as triage and the shadows' answers are**: a row keyed by the entry,
  deleted with it when its period's raw entries are purged (ADR 0014). Turned down: a label
  costs nothing to keep and is gathered slowly, one tap at a time, so losing it with the raw
  window loses the corpus.
- **Only the picked messages kept**: turned down. Picks are not a random sample (reasons
  have shares of the day, both-quiet messages are sampled), so a rate measured over verdicts
  can be weighted back only if every message considered, picked or not, is kept with its
  reason.

Decision: every message a review considers is a row of `grit.reviews`, keyed by its entry's
id with no foreign key to `grit.entries`, and a foreign key to its conversation that
cascades. It holds the reason the two gates gave, when it was considered and picked, where
its prompt was posted, and the verdict standing with its rater; never the message's text
nor a draft's. Its retention is the ledger's: it goes when its conversation does, not with
its period's raw entries. A label is read back by its entry id, which names the message's
conversation and its source's own id (`InboundId`), so a corpus can name its case after the
text is gone.

Consequences: verdicts, and the population they were drawn from, outlive the raw window.
A prompt has no field for text (`Prompt`), and a review cannot recover the message's words
once its entry is purged, so a prompt not yet posted then is not offered. The schema's foreign keys enforce the retention, and the review contract's
test (`ReviewContract`) runs it against the in-memory store and the SQL one.
