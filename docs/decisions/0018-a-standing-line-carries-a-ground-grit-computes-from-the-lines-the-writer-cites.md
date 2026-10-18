# 0018. A Standing line carries a ground grit computes from the lines the writer cites

Status: accepted (2026-09-27); revised (2026-10-14, 2026-10-18)

Context: a closing's Standing lines are what later periods treat as settled (ADR 0011). A
closing writer that read only the period's messages could not tell a fact a tool showed from
one the assistant made up, and kept a model's invented definitions as Standing. A prompt rule
("an unconfirmed claim is not Standing") did not change what it kept, and a forged grit tag in
a message is believed whatever the prompt says, so the prompt cannot guard provenance. The
alternatives: a prompt rule; the writer stating each item's source itself (the model vouching
for its own claims); dropping unconfirmed claims (losing decisions the assistant proposed and
nobody contradicted).

Decision: each Standing line records its ground: `Person`, `Tool` or `Claimed`.

- **The writer cites labels.** Its transcript labels every line it is shown (`u` a person's
  words to grit, `h` a person heard where grit listens, `a` the assistant's, `t` a tool call
  and its result on one line: up to 4,000 characters when it can ground, else 120, and cut back
  to 120 before an earlier line is dropped from what the writer is shown), and it cites the labels that established each Standing
  item. grit maps the citation to the ground (`Labelled.ground`): a person's line wins, then a
  tool line whose result succeeded, otherwise `Claimed`. A tool line whose result is an error
  (failed, declined, interrupted, gone) grounds nothing, and the writer is told to cite where
  the person stated a thing, never where they asked.
- **The writer's answer can only lower it.** It also answers, per item, who established it (by
  person, by tool, by assistant); the ground is the weaker of that answer and what the citation
  supports (`Ground.min`), and a missing or unreadable answer is `Claimed`.
- **Heard and untrusted lines ground nothing.** A heard line, or one below `Integrity.Outside`
  (ADR 0036), supports nothing: an item whose citations name only such lines is not kept.
- Grounds are computed only over the lines the writer was shown. An uncited item or an unknown
  label is `Claimed`, never dropped. A ground is copied verbatim when its line is carried, and
  never changed by a touch.
- **An add records what its citation named** (`Cited`): the ids of the entries its labels name,
  whatever ground they gave, and the tools those entries ran, by their stored calls' names. It
  lives on the add in the closing's flows (`Change.Added`), never on the line, which the cap may
  evict and every closing copies; it is bounded by the writer's reply, which its model profile's
  output token limit caps. It does not depend on how much of a result the writer was shown. The
  references dangle once their entries are purged; nothing reads them yet.
- The window's record lists `Claimed` Standing apart, each line an open question (`Open:
  whether "…" (the assistant said so; nothing confirmed it)`), and the base prompt says to check
  it before relying on it. Closings are stored as version 3; version-2 Standing lines read as
  `Claimed`.

Consequences: a later turn can tell what it may rely on from what it should check. Grounds are
only as good as the writer's citations and answers: neither can raise an item above the
strongest real, shown, non-error line the citation names. A writer that cites a question as
the person's word and answers "by person" can still over-ground an item; so can a successful
but empty result cited "by tool". Citations alone, without the answer, graded most unconfirmed
claims as confirmed. Labels are grit's structure, not text: a message that forges a label is
still one person or tool line, so forgery never raises a ground above what the period's real
lines offer. Open lines and agenda lines carry no ground; upgrading on a touch, and evicting
`Claimed` first, are deferred. Enforced by the types (`Line.of` refuses a Standing line without
a ground, or another with one) and by `ClosingTests` and `ClosingSummaryTests`.
