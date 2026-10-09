# grit.act

What an actor's moves do (ADR 0034): an ask of a model and a call of a hosted tool, as phases
written over `grit.core`'s traits and `Durable`, never DBOS, so its tests run over core's
in-memory fakes (`InMemoryDurable`, `InMemoryEdges`, `InMemoryUsageLedger`,
`InMemorySchedules`). A phase takes its caller's step names, the form its failures are recorded
in (`Faults`) and its recorded codecs, so each caller's steps keep their own names and recorded
forms. It names core alone (enola-intent.yaml's `act-names-core-alone`).

- **`phase`** — `Asking`: a model's reply, retried while its provider is unavailable
  (`Asking.Retries`) and told to a `Hearing` as it is generated, a fresh `Heard` for each
  attempt; whether an acting's `Allowance` admits an ask; and an ask's cost recorded in the
  ledger, once. `Classifying`: a classifier's answers to a judgment's request, retried while
  it is unavailable, on the same waits as a reply. `Shaping`: a JSON ask's request, which
  requires a call of its one reply tool, that call's arguments checked and read, and a reply
  that does not read answered and asked again (`Shaped`). `Calling`: what an acting's `Gates` make of a call (`Gated`), whom its request
  is made for, the request itself, sent to the live edge serving its place, the wait for that
  edge (`Awaited`, its silent steps named by `WaitSteps`, `Calling.ServeWithin` then
  `Calling.RunWithin`), a person's approval of a gated call, and the answer read from the
  request once its edge rang.

- **`moves`** — `DurableMoves`: a planner's `Moves` (`grit.core.act`) over the phases, and a
  keeping job's `Keeping`, whose keep is one transaction under a savepoint, each
  move made at most once per run as the steps `MoveSteps` names (`move:{name}`, then
  `move:{name}:{phase}`), refused by name, by its kind's limit, and once a rerun's input
  differed from what the run recorded; `MovesEnv` and `MoveRecords`, what they are made with;
  `MovesJournal`, their recorded forms and the versioned digests of their inputs.

`moves` imports `phase`; `phase` imports nothing in `grit.act`.
