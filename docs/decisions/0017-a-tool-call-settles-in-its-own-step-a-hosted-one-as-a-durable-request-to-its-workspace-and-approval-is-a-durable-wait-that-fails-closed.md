# 0017. A tool call settles in its own step, a hosted one as a durable request to its workspace, and approval is a durable wait that fails closed

Status: accepted (2026-09-27); revised (2026-10-21)

Context: a turn's loop runs the tools its model calls. A step cut short by a crash runs again
from the start, and its output is journaled. With one engine per database (ADR 0015), an
engine running every tool over its own directory ran them wherever it started. Some tools wait
for a person's approval, for hours, across restarts. Turned down: the whole loop as one step (a
crash reruns it call by call); a tool's output in the journal (large); rerunning a change after
a crash because most are idempotent (`run` is not); routing a call to an edge (the request dies
with it); approval inside the tool's step (a step cannot wait on a message); an approvals table
the workflow polls; standing approvals kept across restarts.

Decision:

**The call's step.** Each call is settled in a step of its own, `call:n:j`, named by its round
and index, after the reply that made it is kept (`ask:n:record`). Its arguments are read
before the step, by pure code a replay reruns identically. The step keeps the call's result as
an entry whose id the slot fixes, written from inside the step, and returns only that id and
whether it failed; a rerun that finds the result runs nothing. Every failure is a result the
model reads (an unknown tool, unreadable arguments, a refused path, a denial, a timeout); only
a store that cannot be written, or a provider that fails, fails the turn.

- **An engine tool** (one that touches no machine: reading grit's store, a model probe) runs
  inside that step. A read cut short may run twice. A gated one records an attempt marker
  first; a rerun that finds the marker and no result answers `Interrupted` without running it.
- **A hosted tool** acts on a machine, so it is a request row (`grit.tool_requests`), keyed by
  its slot (`CallSlot.key`), addressed to a **workspace, never an edge**: the conversation's
  directory, or a service place (ADR 0031) its deployment links it to (`WorksIn`). An edge
  registers the places it hosts (`grit.edges`, `grit.edge_places`); `Edges.authorize`, when an
  edge claims, is the one routing decision. A request records its conversation, workspace,
  principal (ADR 0034), permit, retry, protocol version and claiming edge, and moves only by
  conditional updates: `open`, `claimed`, `answered` or `expired`. The claim is the attempt
  marker: an orphaned claim is settled by the tool's declared `Retry` (`Rerun` claims it again;
  `Interrupt` answers `Interrupted`), never by its gate. The edge writes the answer on the row
  and rings the turn; the turn reads it from the row in `call:n:j`, so a lost ring loses
  nothing. Unclaimed within `Calling.ServeWithin` (10 s), or unanswered within `RunWithin`
  more, the request is settled by `call:n:j:expire` or `call:n:j:abandon`. The offer records
  what an edge advertises, so a replay rebuilds it without the edge.
  An addressed turn is also offered the tools at each service its place reaches (`Reaches`);
  a turn rooted on a message grit heard is offered none of them. A service's edge holds a call
  there to exact argument bounds, failing closed, never by words to the model.

**Approval.** For a gated call the turn keeps a `Payload.Ask` entry (`call:n:j:approve`), so
every edge sees what it is asked, then waits with `Durable.recv` on `Approval.topic(call)`,
outside any step, up to the call's own wait: its tool's (`Gate.Ask`'s `within`, by default
`Gate.AnswerWithin`, a day), at most its loop's, met in `Calling.gate`; the ask records when it
ends (`until`). The wait is labelled `call:n:j:wait`. DBOS records the wait's end and the
answer, so a restarted turn waits only for what is left, and a replay gets the same answer. An
edge answers with `Inbox.answer`, naming the account that answered: only an account of the
turn's asker (`Askers.of`) may answer (`NotAsker`), and only while the ask is open, its call
unsettled and its `until` ahead by the database's clock (`Closed`). The answer goes under an
idempotency key fixed by the workflow and call: only the first answer counts. It fails closed:
no answer in time is `TimedOut`, an answer that does not decode is `Declined`, and both deny
the call.

Consequences: a crash mid-loop reruns at most one read, never a change; an interrupted change
is reported, not retried. No open edge means no access, which the turn is told. A hosted call
costs a round trip through Postgres instead of a function call. A turn left waiting holds a
DBOS workflow, not a thread. An edge says what it does with an ask (`ServedEdge.asks`, of type
`ServedEdge.Asks`): one that answers lists a turn's open asks (`Deliveries.asks`) under that
turn's subject. While an edge shows none (`Unanswered`), the kit offers no administering tool
(ADR 0033); a service's (`Never`) holds nothing back.
Requests are journal-class, purged with their period (ADR 0014). Enforced by the replay gate
and the contract suite `EdgesContract`, with `TurnToolsTests`.
