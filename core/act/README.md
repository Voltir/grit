# grit.act

What an actor's moves do (ADR 0034): an ask of a model or of the deployment's classifier, and
a call of a hosted tool, as phases written over `grit.core`'s traits and `Durable`, never DBOS,
so its tests run over core's in-memory fakes (`InMemoryDurable`, `InMemoryEdges`,
`InMemoryUsageLedger`, `InMemorySchedules`). A phase takes its caller's step names, the form
its failures are recorded in (`Faults`) and its recorded codecs, so each caller's steps keep
their own names and recorded forms. It names core alone (enola-intent.yaml's `act-names-core-alone`).

- **`phase`**: the steps the turn and jobs share: `Asking` (a model's reply, retried while its
  provider is unavailable, admitted by the acting's allowance, its cost recorded once),
  `Classifying` (a judgment's answers, retried on the same waits), `Shaping` (a JSON ask: a
  reply that does not read is told why and asked again, as often as its caller allows) and
  `Calling` (a hosted call: its gate, its request to the live edge serving its place, the wait
  for that edge, a person's approval when gated, and the answer read).

- **`moves`**: `DurableMoves`, a planner's `Moves` and a keeping job's `Keeping` over the phases.
  Each move is made at most once per run, as the steps `MoveSteps` names; it is refused by name,
  by its kind's limit, and once a rerun's input differs from what the run recorded. An ask's
  typed reply is read from the step's record on every run. A keep is one transaction under a
  savepoint.

`moves` imports `phase`; `phase` imports nothing in `grit.act`.
