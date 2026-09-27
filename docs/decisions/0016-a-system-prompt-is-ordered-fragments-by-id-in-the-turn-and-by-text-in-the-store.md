# 0016. A system prompt is ordered fragments: by id in the turn, by text in the store

Status: accepted (2026-09-27)

Context: the system prompt was one string written in `Main`, and the one model input that
was not in the store: nothing showed what a turn was sent, and nothing but the code said
what it was. It is going to come from several places (grit's own words, the edge, the
person, the place's instruction files), each changing at its own pace, and a provider's
prompt cache hits only on a byte-identical prefix. The alternatives: keep one string per
edge (no record, no reuse across places); record the prompt's text in the turn's journal
(the same text copied into every turn, and a step output that is content rather than a
reference, ADR 0009); or record it as an entry (entries belong to one conversation and go
with its raw purge, so identical prompts would not dedupe, and every reader of entries
would have to skip it).

Decision:

- **A prompt is ordered fragments.** Each has a `Layer` and a source. The layers, stable to
  volatile: `Base` (grit's words, per release), `Edge` (what the edge is and renders),
  `Reach` (grit's words on where the conversation is and what a turn may reach there), then
  `Place` (the place's own instruction files). A person layer comes later, between `Edge`
  and `Reach`. `SystemPrompt.of` sorts stably by layer, and `render` joins the texts the
  same way every time, so equal fragments give equal bytes.
- **The Place layer is data from the place, never grit's own words.** It is the place's
  `AGENTS.md`, else its `CLAUDE.md` (never both), per directory from `/` down, the nearest
  last, verbatim under one heading naming the file. It is bounded (`PlaceFragments`): the
  nearest file up to 64 KiB, each farther one cut at 8 KiB, every cut on a character
  boundary and saying so, equal texts once, and the farthest left out while the layer is
  over 12k tokens (never the nearest).
- **Ids in the turn, texts in the store.** A fragment's id is a content hash of its layer,
  source and text. A turn records the ids it was sent; the texts are kept once in
  `grit.prompt_fragments`, forever and unchanged, and a replay reads them back by id. An id
  with no text fails the turn loudly. `grit.turn_prompts` says which fragments each turn
  used, for the turn panel and the eval.
- **The system prompt does not draw on the assembly budget.** Its size is bounded by its
  own layers' caps.

Consequences: the prefix a provider caches is stable across a turn's tool calls and across
turns in one place; an edit to a place's `AGENTS.md` changes only the Place layer. What a
turn was sent can be shown exactly. Fragments are never collected, so their table grows
with every distinct text; a collection kind for unreferenced ones waits until nothing that
can still replay names them. Enforced by `SystemPromptTests` (order, the pinned id),
`PlaceFragmentsTests`, `LocalInstructionsTests`, `TurnPromptTests` (the markers the base
names are the ones grit writes) and the store contract (`PromptStore`).
