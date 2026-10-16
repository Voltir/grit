# 0007. Replies are an edge-neutral prose form, rendered once per edge

Status: accepted (2026-09-24)

Context: models write their replies in markdown, and grit shows them on more than one edge
(ADR 0002): the terminal and Slack. Each edge has its own idea of formatting. The terminal
has cells, a theme and no italic that grit may rely on; Slack has its rich-text blocks, with
their own limits. A reply is also rendered many times while it streams (ADR 0006), from
prefixes that end mid-construct. Three alternatives were turned down:

- rendering markdown straight to terminal cells, which leaves nothing for Slack to reuse;
- asking the model for structured output (Portable Text JSON), which models write less
  reliably than markdown, and which cannot be shown usefully while it is half-written;
- a Java markdown library, which would be a new quarantined dependency (ADR 0001), and
  which is built for whole documents rather than streaming prefixes.

Decision: markdown is parsed into grit's own prose form (`grit.prose.form`: paragraphs,
headings, lists, quotes, code, tables and rules, with inline spans marked strong, emphasis,
code or link). The form knows nothing of cells, colours, widths or any edge. Each edge
renders it through a `Renderer[Out]` of its own: the terminal's is `ProseLook` (in
`grit.app.look`), drawn in the theme, and Slack's is `RichText` (in `grit.slack.text`).
`grit.prose` depends on no other module, and every edge may depend on it. The parser is
hand-written for a subset, and `Markdown.parsePrefix` renders a streaming prefix whose shape
only grows: an unclosed marker is styled to the end of the text, and a line that could still
become a marker is held back.

Consequences: an edge adds one renderer and no parsing. The subset is grit's to extend
(strikethrough, task lists and syntax highlighting are not in it) and to test, including
every prefix of a reply. Constructs outside the subset show as their text. Enforced by the
law (`prose-names-no-other-module`) and `MarkdownTests`, which checks every-prefix stability.
