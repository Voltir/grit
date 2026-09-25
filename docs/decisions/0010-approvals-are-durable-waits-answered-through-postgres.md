# 0010. Approvals are durable waits, answered through Postgres, and fail closed

Status: accepted (2026-09-25)

Context: `write`, `edit` and `run` wait for a person's approval before each call. The wait
can last hours and must survive a restart, and ADR 0002 already says an edge answers a
turn's durable waits by sending messages. What was open: where the wait sits among the
steps, how an answer is keyed, and what a missing or garbled answer means. Turned down:
asking inside the tool's step (a step body may not wait on a message, and a rerun would ask
again); an approvals table the workflow polls (DBOS's `send`/`recv` already give a durable,
notified, exactly-once mailbox); and standing approvals kept across restarts (parked; tacit
calls its own denylist "not a security boundary").

Decision: for a call bound to a gated tool, the turn

1. keeps a `Payload.Ask(call, shown)` entry in the transact step `ask:n:j`, so every edge
   sees what it is asked to approve as it sees any entry;
2. waits with `Durable.recv` on the topic `Approval.topic(call)`, outside any step, up to
   `TurnTooling.answerWithin` (`TurnTools.AnswerWithin`, a day, by default). DBOS records
   the wait's end as it begins and the answer as it arrives, so a restarted turn waits only
   for what is left, and a replay gets the same answer without asking again;
3. settles the call in `tool:n:j` with that answer (ADR 0009).

An edge answers with `Inbox.answer(workflow, call, approval)`: `DBOSClient.send` under an
idempotency key fixed by the workflow and the call, so only the first answer counts. The
turn fails closed: no answer in time is `TimedOut`, an answer that does not decode is
`Declined` with the reason, and both are `Outcome.Denied`, a result the model reads.

Consequences: the TUI, and later Slack, approve through the same row-and-notify path, from
any process. A turn left waiting holds a DBOS workflow, not a thread's worth of state, and
a restart mid-wait neither asks twice nor loses the answer. A run with no edge to answer
cannot offer gated tools (`Main` refuses `GRIT_TOOLS=all` there). Two calls with the same id
in one turn would share a topic and a key; the model's call ids are taken to be distinct.
Enforced by `AnswerLiveTests` (once only, early answer, timeout, restart mid-wait),
`ApprovalLiveTests` (approve, decline, timeout, restart, against the real edit tool), and
the `loop-approved` history in `TurnReplayTests`.
