# 0016. A system prompt is ordered fragments: by id in the turn, by text in the store

Status: accepted (2026-09-27); revised (2026-09-27, 2026-10-21)

Context: the system prompt was one string, and the one model input not in the store: nothing
showed what a turn was sent. It comes from several places (grit's own words, the edge, the
person, the place's instructions), each changing at its own pace, and a provider's prompt
cache hits only on a byte-identical prefix. The alternatives: one string per edge (no record,
no reuse across places); the prompt's text in the turn's journal (the same text copied into
every turn, and a step output that is content rather than a reference, ADR 0038); or an entry
(entries belong to one conversation and go with its raw purge, so identical prompts would not
dedupe, and every reader of entries would have to skip it).

Decision:

- **A prompt is ordered fragments.** Each has a `Layer` and a source. The layers, stable to
  volatile: `Base` (grit's words, per release), `Edge` (what the edge is and renders), `Person`
  (how the person wants to be talked to), `Reach` (grit's words on where the conversation is
  and what a turn may reach there), then `Place` (the place's own instructions).
  `SystemPrompt.of` sorts stably by layer, and `render` joins the texts the same way every
  time, so equal fragments give equal bytes.
- **The Person layer is the person's choice**: its one fragment is the voice (`Voice`): a
  named voice (whose text is fixed per release and so caches like Base), the person's own
  words, or `plain`, which adds no fragment. Its source is the person, never grit. Of the
  engine's workflows only the turn is given a `VoiceStore`; the closing, query and summary
  writers take no system text a caller could pass it in. Manner only: Base's last line says
  later instructions change how grit speaks, never what it reports about its memory or a
  tool's outcome.
- **grit tags what it writes into a window.** Each section grit writes begins with a tag
  naming it (`SectionTag`: `[record]`, `[afar]`, `[strand]`, `[gap]`, `[doc]`), taught by
  Base, whose legend is written from the same tags.
- **In text grit did not write, a tag that starts a line is neutralised at assembly**
  (`Shown`): from the first tag line on, the text is shown as a quoted paste under a lead-in
  saying grit did not deliver it, its tags broken. Stored entries are never changed. The
  prompt cannot defend the tags: no placement or legend stopped a forged `[record]` being
  believed, and the quoted paste did. What a tool result's content may do is ADR 0036's.
- **The Place layer is data from the place, never grit's words**: either words a deployment
  declared for the place (`PlaceFragments.declared`), or its instruction files: its
  `AGENTS.md`, else its `CLAUDE.md` (never both), per
  directory from `/` down, the nearest last, verbatim under one heading naming the file. It
  is bounded (`PlaceFragments`): the nearest file as read (at most 64 KiB), each farther one
  cut at 8 KiB on a character boundary and saying so, equal texts once, and the farthest left
  out while the layer is over 12,000 tokens (never the nearest).
- **Ids in the turn, texts in the store.** A fragment's id is a content hash of its layer,
  source and text. A turn records the ids it was sent; the texts are kept once in
  `grit.prompt_fragments`, unchanged, and a replay reads them back by id. An id with no text
  fails the turn loudly. `grit.turn_prompts` says which fragments each turn used.
- **A turn's request says when its message was said**, after every fragment
  (`TurnShown.answering`), so "today" and "Friday" can be read against it: in the person's zone
  when known, else the deployment's, then in UTC, with how to read times; the zones recorded by
  the turn's `offer` step. It is made from the turn's first entry, which is kept, and those
  zones, so a rebuilt request carries the same line. It is never a fragment: fragments are kept
  forever, and one per turn would grow their table with every turn.
- **The system prompt does not draw on the assembly budget.** Its size is bounded by its
  layers' caps.

Consequences: the prefix a provider caches is stable across a turn's tool calls and across
turns in one place; an edit to a place's `AGENTS.md` changes only the Place layer. What a turn
was sent can be shown exactly. Fragments are never collected, so their table grows with every
distinct text; a collection kind for unreferenced ones waits until nothing that can still
replay names them. Enforced by `SystemPromptTests` (order, the pinned id) and
`TurnPromptTests` (the base teaches every tag grit writes), `TurnTests` and `TurnHostedTests`
(the line saying when the message was said, and that no fragment holds it), and
`TurnShownTests` (its forms).
