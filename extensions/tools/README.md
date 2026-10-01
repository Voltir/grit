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
- `About` — `about`: what grit is and how it works, from the docs in `resources/about`.
