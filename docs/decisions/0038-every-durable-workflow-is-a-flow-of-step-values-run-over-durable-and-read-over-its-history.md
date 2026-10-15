# 0038. Every durable workflow is a flow of step values, run over Durable and read over its history

Status: accepted (2026-10-15)
Context: The turn, a job's run and the lifecycle workflows each wrote their steps by hand as
nested `Durable` calls, so each fact about a step (its name, order, patch, read-back, the watcher's
next step) was kept in several places, the tool loop's state lived in mutable maps and a cell
read by closures, and ideas shared across workflows (an ask then its record, a placement wait,
a hosted call) were written per workflow. Turned down: a step algebra for the turn alone; step
values that carry their capture set, which a read could not use without a live environment;
recording each round's request; keeping the turn's own step names (ADR 0034).
Decision:
- A step is a pure value: its recorded name (a fixed name or a family and key), its output's
  codec, and a body over its input given the environment the run hands it. A flow composes
  steps with pure glue: sequence, an early end, a finite choice, a loop whose state crosses
  rounds on the wire, each-of, a patch (ADR 0004), a wait, a narrowing of the environment, and a
  program: code that makes its steps one at a time, as a planner does.
- Two interpreters read one description: `run` over `Durable`, the only holder of it, and
  `read` over a recorded history, which runs no step's body, needs no environment, and gives
  the next step, how the flow ended, and what each step's input showed; a program's own code
  runs again over the results the history holds. Step names, step order, a running workflow's
  step, a recorded turn's requests and the replay gate are derived from these, not kept by hand.
- Whatever decides a flow's control is on the wire: a round records the kinds of the calls it
  bound. A turn's integrity level is a value of its loop's state, the meet of what each gate
  recorded (revises ADR 0036's cell).
- One step grammar for the turn and jobs: `ask:<key>`, `call:<key>`, `keep:<key>`, phases
  after a colon (revises ADR 0034's "the turn keeps its names"). Lifecycle's asks are moves.
- A plugin adds steps only as a flow at a declared point, never handed `Durable`.
Consequences: a new step is one value in one place; a rename, reorder or unreadable output
fails both the replay gate and the read gate. A workflow written as a flow takes more lines
than its nested body did (both spikes measured about twice as many). A program's steps are not
in the static names. A body sees its whole environment, not a narrower capability set. Enforced
by the types (flows are pure, a body captures only its environment, glue is pure, a `Tx` and a
program's `Steps` cannot escape), capture probes, the replay and read gates over every history
of the epoch, and the law (lifecycle may name the moves' module).
