# 0043. Prose is grit's stored tree: nested in memory, flat as stored JSON, written per consumer

Status: accepted (2026-10-22)

Context: `grit.prose` began as a reply's structure, markdown in and blocks out for each edge to
draw. Files, pieces of them and topics now need one form for text grit keeps: a reader turns a
document into it, a store keeps it, and the model, the terminal, Slack and an index each read
it their own way. Turned down: markdown as the stored form (a table's numbers and dates lose
their kind, and every consumer would parse it again); a flat tree in memory, Portable Text's
shape (every renderer, and the streaming parser, would rebuild the nesting a list and a quote
already have); pandoc's whole AST (blocks and inlines no reader here emits and no consumer
reads); keys on stored blocks (nothing refers to one, and an ordinal says no more than a
position); and each keeper of prose with its own codec, as topics had.

Decision:

- **One tree, nested in memory.** `Doc`, `Block`, `Item` and `Text`, spans carrying marks and
  pure notes of `A`, as pandoc nests them. A table's cells are typed (`Cell`), each row as
  wide as its columns (`Block.table`): a number or a date keeps its value beside the text its
  source showed, and `Cell.printed` is the one text every renderer prints for a cell, never
  reformatted. The
  tree gains a construct only when a reader emits it and a consumer reads it.
- **Flat as stored.** `grit.prose.json.ProseJson` writes a doc as `{"v", "notes", "blocks"}`:
  a list of records, a list item's kind and level as fields, a quote's blocks with their
  depth, a list with no items as one record, notes written once each by the keeper's pure
  codec and named by index. A list or quote right after another of its kind marks its first
  record `"new"`: `true` on an item, which is always its own list's; on a quote a number,
  which of the quotes around it opens afresh, since one quote's record also stands for each
  quote it is in ([the module's README](../../core/prose/README.md) has the records). Reading
  rebuilds the nesting, and refuses another version, naming it. A keeper's own data that holds
  prose versions itself and stores each doc this way; it never keeps a codec of its own.
- **Written per consumer.** `Renderer[Out]` is the one writer trait: the terminal's
  `ProseLook`; Slack's `RichText`. The model's markdown is `Markdown.write`, total over any
  doc, whose output `Markdown.parse` reads back exactly for a doc it made; it becomes a
  `Renderer` value with its first caller that takes one, as an index's writer will. A
  renderer is a function of the doc and what it was built with.
- **What is rendered at keep time stays so.** A document's `body` is its text as the model
  sees it, written when it is kept, since search ranks that text; its tree, when it has one,
  is in its data.

Consequences: a reader, a store and a renderer meet at one type, and a new consumer is one
`Renderer`. Changing the stored form is a new `ProseJson.Version` and a version of each
keeper's data; topics moved onto it at their data's version 2. A topic of version 1 is not
read: the dream's scan withdraws it, and a nightly run opens it again only from the strands
spoken in within its `recent`; one written from older strands comes back only through a run
for their past days, which is `scripts/eval backfill` (`TopicBackfill`), and that runs over a
copy of a captured database, so on a live one such a topic stays withdrawn. A topic of a later
version than its reader's is left as it is and counted, so an older engine wipes nothing. Only
the json package names ujson (a grep in `scripts/enola-law.sh`, which sees a fully qualified
name where an import rule would not); a note codec that captures a capability does not compile (a probe, `ProseJsonCaptureTests`).
The stored form is pinned by `ProseJsonTests`.
