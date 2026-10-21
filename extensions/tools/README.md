# grit.tools

Its tools are `Tool` and `Hosted` values (`grit.core.tool`), which an edge serves at a place
(ADR 0017; what a deployment can supply:
[`docs/extending.md`](../../docs/extending.md#extension-points)).

Each tool is written once, as the `Hosted` description the engine offers and the `Tool` an
edge runs, over `grit.core.host`'s capabilities; it names no implementation of them.

One package, `grit.tools`:

- `Coding` — read, list, search, write, edit and run in a checkout; write, edit and run
  ask a person first.
- `Tuning` — `propose_model_setting`: a measured setting of a model, kept once a person
  approves it.
- `Probes` — `probe_pair`: a battery of calls measuring a (model, upstream) pair.
- `About` — `about`: who the assistant is (its deployment's persona) and what grit is and how
  it works, from the docs in `resources/about`, one per topic (`About.Section`): grit itself,
  memory, markers, periods, places, and security (who may see what, and how a person is known
  across sources).
- `Cleared` — `clearance`: what the person asking is cleared for and why, answered only in a
  direct message with them (anywhere else, the room's own label and one fixed line telling the
  model to mention the direct message only if the person asked about their own clearance), over
  the store and the engine's askers, which only the kit holds.
- `MessageIds` — `message_ids`: this conversation's id, and the asker's first or last message
  in the room (every conversation in it counted: a channel's threads, a person's direct
  messages), by its entry id, its conversation, when it was said and the link its edge recorded
  it with; over the store and the engine's askers, which only the kit holds.
- `Names` — every name above, which a plugin's tool may not take (`Deployment.of` refuses it).
