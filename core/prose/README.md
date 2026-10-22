# grit.prose

Prose as grit keeps it: a reply's, a document's or a topic's paragraphs, headings, lists,
quotes, code and tables as plain data that no consumer owns ([ADR 0043](../../docs/decisions/0043-prose-is-grits-stored-tree-nested-in-memory-flat-as-stored-json-written-per-consumer.md)).
Nested in memory, flat as stored, and written per consumer: the model reads markdown, the
terminal and Slack each draw the same blocks their own way, so nothing in the form knows about
widths or colours. Names no other grit module; ujson, for its json package alone, is its one
library.

In dependency order:

- **`form`** -- the blocks (`Doc`, `Block`, `Item`, `Level`), a table's `Column`s and typed
  `Cell`s, their inline text (`Text`, `Span`, `Mark`), and `Renderer`, the trait each
  consumer's output mode implements. A span may carry notes, pure data its writer says of it
  that no renderer shows: a doc with notes of `A` is a `Doc[A]`, and plain prose, which
  renderers take, a `Doc[Nothing]`. A cell keeps its value and the text its source showed;
  `Cell.printed` is what every renderer prints for it. Imports nothing in prose.
- **`markdown`** -- `Markdown.parse` for a whole reply and `Markdown.parsePrefix` for one
  still streaming, which holds back what the next token may still change so that each
  prefix renders as the start of the whole. `Markdown.write` is the way back, the model's
  text: total over any doc, and read back exactly by `parse` for a doc `parse` made.
  Hand-written, total, and a subset: what it reads and writes is in `Blocks` and `Inlines`.
  ← `form`
- **`json`** -- `ProseJson`, the stored form: a flat list of records under a schema version,
  notes written once by the keeper's codec. ← `form`

  Its records, by `"type"`: `para`, `heading`, `code`, `table` and `rule`, one block each; a
  list as its items, each an `item` record (`"list"`: `bullet` or `number`, `"level"`, its
  first paragraph as `"text"` when it opens with one), its other blocks after it a level
  deeper, a nested list's items as items and any other block as an `item-more` holding it;
  a list with no items as one `list` record; a quote as its blocks, each a `quote` record
  (`"depth"`) holding it, a quote inside one a depth deeper. Records that follow on from one
  another join one block, so a list or quote directly after another of its kind marks its
  first record `"new"`. On an item it is `true`: an item record is always its own list's, at
  its level, so it needs no more. On a quote it is a number: a quote's record also stands for
  every quote around it, so `"new": n` says which of them opens afresh, the `n`th counted from
  the outermost, those outside it continuing (two quotes side by side inside one quote are
  `"depth": 2, "new": 2` at the second's first record).

`markdown` and `json` name nothing of each other. No source file sits at the module's root,
and the test tree mirrors it. The terminal's renderer is `grit.app.look.ProseLook`, beside the
theme it draws in; Slack's is `grit.slack.text.RichText`.
