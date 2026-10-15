# grit.act

What an actor's moves do (ADR 0034): an ask of a model or of the deployment's classifier, and
a call of a hosted tool, as phases written over `grit.core`'s traits and its flows, never DBOS,
so its tests run over core's in-memory fakes (`InMemoryDurable`, `InMemoryEdges`,
`InMemoryUsageLedger`, `InMemorySchedules`). A phase takes its caller's step names, the form
its failures are recorded in (`Faults`) and its recorded codecs, so each caller's steps keep
their own names and recorded forms. It names core alone (enola-intent.yaml's `act-names-core-alone`).

- **`phase`**: the steps the turn and jobs share: `Asking` (a model's reply, retried while its
  provider is unavailable, admitted by the acting's allowance, its cost recorded once),
  `Classifying` (a judgment's answers, retried on the same waits), `Shaping` (a JSON ask: a
  reply that does not read is told why and asked again, as often as its caller allows) and
  `Calling` (a hosted call: its gate, its request to the live edge serving its place, the wait
  for that edge and a person's approval when gated, each a flow, and the answer read).

- **`moves`**: `DurableMoves`, a planner's `Moves` and a keeping job's `Keeping` over the phases,
  made through a program's `Steps`, so a run's history is read by running its planner again.
  Each move's steps are `Act`'s flows, recorded under the names `ActName` gives; whether a move
  may be made, and what its records come to, are `DurableMoves`'. Each move is made at most once
  per run; it is refused by name,
  by its kind's limit, and once a rerun's input differs from what the run recorded. An ask's
  typed reply is read from the step's record on every run. A keep is one transaction under a
  savepoint. What a move answers carries the integrity it is believed at (ADR 0036): a run
  starts at its acting's, a keep of what is believed below `Outside` is refused, and the moves
  hold the two endorsements core's `Moves` raises with, an extraction's (`extract`) and the
  integrity tree's (`vet`), which no other code here reaches. `Judging` is an engine workflow's
  judgment as a move: its pose reads what to ask and returns the request with a pure reader of
  the reply, and the move's own step admits it, asks once and charges every reply.

`moves` imports `phase`; `phase` imports nothing in `grit.act`.
