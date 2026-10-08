# grit.act

What an actor's moves do (ADR 0034): an ask of a model and a call of a hosted tool, as phases
written over `grit.core`'s traits and `Durable`, never DBOS, so its tests run over core's
in-memory fakes. A phase takes its caller's step names and recorded codecs, so each caller's
steps keep their own names and recorded forms. It names core alone (enola-intent.yaml's
`act-names-core-alone`).

- **`phase`** — `Asking`: a model's reply, retried while its provider is unavailable
  (`Asking.Retries`) and told to a `Hearing` as it is generated, a fresh `Heard` for each
  attempt; whether an acting's `Allowance` admits an ask; and an ask's cost recorded in the
  ledger, once.

One package, so it has no order to keep.
