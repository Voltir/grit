# 0009. A tool call is its own step, and a change never runs twice

Status: accepted (2026-09-25)

Context: a turn's tool loop runs tools the model calls: reads, and changes (writing or
editing a file, running a command). `Durable` runs a step cut short by a crash again from
the start, and records whatever a step returns in the journal. The alternatives turned
down: the whole loop as one step, which a crash would rerun call by call, commands
included; a tool's output in the journal, which breaks "only references cross a step" and
copies large output into every replay; and running a change again after a crash on the
grounds that most are idempotent, which `run` is not.

Decision: each call is settled in a step of its own, `tool:n:j`, named by its place in the
loop (`TurnTools.Slot`), after the reply that made it is kept (`record-call:n`), in order.

- **Fixed-id results.** The step keeps the call's result as an entry whose id the slot
  fixes (`Slot.resultId`), written through `Jot` from inside the step, and returns only
  that id and whether it failed. A rerun that finds the result returns it and runs nothing.
- **Reads may run twice.** A free call cut short between its run and its result runs again.
- **Changes never run twice.** A gated call records an attempt marker (`Slot.attemptId`,
  `Payload.Attempt`) in a short transaction before it starts. A rerun that finds the marker
  and no result answers `Outcome.Interrupted` (it may have partly run) without running it,
  and the model checks what it did.
- **Every failure is a result.** An unknown tool, unreadable arguments, a refused path, a
  declined or unanswered approval, a timeout: each is an `Outcome` the model reads, and the
  loop goes on. Only a store that cannot be written, or a provider that fails, fails the
  turn.
- A call's arguments are read (`TurnTools.read`) before its step, so what the step does is
  decided by pure code the replay reruns identically.

Consequences: a crash mid-loop reruns at most one read, never a change, and the journal
stays small. A write, edit or command interrupted by a crash is reported, not retried, so
the model sees a partial change rather than a doubled one. `Jot` writes are not atomic with
the step, which is why every one is keyed by an id fixed before the step. Enforced by
`TurnToolsTests` (a crash between run and result, free and gated), `TurnLoopTurnTests`,
and the `loop-crashed-in-tool` history in `TurnReplayTests`.
