# grit.prose

Prose as structure: a reply's paragraphs, headings, lists, quotes, code and tables as
plain data that no edge owns. Markdown in, blocks out. A model writes markdown by habit;
each edge -- the terminal now, Slack later -- renders the same blocks its own way, so
nothing in the form knows about cells, colours or widths. Names no other grit module and
has no dependencies.

In dependency order:

- **`form`** -- the blocks (`Doc`, `Block`, `Item`, `Level`), their inline text (`Text`,
  `Span`, `Mark`), and `Renderer`, the trait an edge's output mode implements. Imports
  nothing in prose.
- **`markdown`** -- `Markdown.parse` for a whole reply and `Markdown.parsePrefix` for one
  still streaming, which holds back what the next token may still change so that each
  prefix renders as the start of the whole. Hand-written, total, and a subset: what it
  reads is in `Blocks` and `Inlines`. ← `form`

No source file sits at the module's root, and the test tree mirrors it. The terminal's
renderer is `grit.app.look.ProseLook`, beside the theme it draws in.
