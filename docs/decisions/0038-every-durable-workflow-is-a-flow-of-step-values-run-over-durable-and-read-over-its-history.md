# 0038. Every durable workflow is a flow of step values, run over Durable and read over its history

Status: accepted (2026-10-15)

Context: the turn, a job's run and the lifecycle workflows each wrote their steps by hand as
nested `Durable` calls, so each fact about a step (its name, order, patch, read-back, the
watcher's next step) was kept in several places, the tool loop's state lived in mutable maps
and a cell read by closures, and ideas shared across workflows (an ask then its record, a
placement wait, a hosted call) were written per workflow. grit is direct style in its effects
and stays so: a step returns its output, not an effect value; capabilities are parameters,
never an effect type parameter; and DBOS is the durable runtime, whose workflow context is
thread-affine, so the interpreter blocks rather than suspends and no IO runtime sits beside
it. Turned down: a step algebra for the turn alone; step values that carry their capture set,
which a read could not use without a live environment; recording each round's request;
keeping the turn's own step names.

Decision:

- **A step is a pure value**: its recorded name (a fixed name, or a family and key), its
  output's codec (`Journaled`), and a body over its input given the environment the run hands
  it. A flow composes steps with pure glue: sequence, an early end, a finite choice, a loop
  whose state crosses rounds on the wire, each-of, a patch (ADR 0004), a wait, a narrowing of
  the environment, and a program (`Flow.program`): code that makes its steps one at a time, as
  a planner does, the one monadic escape. The combinators take standard names where one fits,
  and their laws are tested.
- **Two interpreters read one description**: `Flows.run` over `Durable`, the only holder of
  it, and `Flows.read` over a recorded history, which runs no step's body, needs no
  environment, and gives the next step, how the flow ended, and what each step's input showed;
  a program's own code runs again over the results the history holds. Step names, step order,
  a running workflow's step, a recorded turn's requests and the replay gate are derived from
  these, not kept by hand.
- **What a step records** is a reference or a bounded outcome, never content: an id of what it
  wrote to grit's store, a decision, a count. A workflow takes references as its input (ADR
  0002). An output is a value, never `Unit`: a type with no `Journaled` instance does not
  compile as a step's. Each step family's recorded bytes have a budget, and so has a history
  in all (`RecordedSizes`, checked over every history beside the replay gate).
- **Whatever decides a flow's control is on the wire**: a round records the kinds of the calls
  it bound. A turn's integrity level is a value of its loop's state, the meet of what each
  gate recorded (ADR 0036).
- **One step grammar** for the turn and jobs: `ask:<key>`, `call:<key>`, `keep:<key>`, phases
  after a colon (ADR 0034). The lifecycle's asks are moves.

Consequences: a new step is one value in one place; a rename, reorder or unreadable output
fails both the replay gate and the read gate. A workflow written as a flow takes more lines
than its nested body did (about twice as many). A program's steps are not in the static names.
A body sees its whole environment, not a narrower capability set. Enforced by the types (flows
are pure, a body captures only its environment, glue is pure, a `Tx` and a program's `Steps`
cannot escape), capture probes (`FlowCaptureTests`), the replay and read gates over every history of the epoch,
the law (`act-names-core-alone`), and `DurableFlowLawTests`.
