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
- `About` — `about`: who the assistant is (its deployment's persona) and what grit is and how it works, from the docs in `resources/about`.
- `Cleared` — `clearance`: what the person asking is cleared for and why, answered only in a
  direct message with them (one constant line anywhere else), over the store and the engine's
  askers, which only the kit holds.
- `Names` — every name above, which a plugin's tool may not take (`Deployment.of` refuses it).
