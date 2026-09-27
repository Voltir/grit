# 0017. A tool call is a durable request addressed to a workspace, and run by an edge hosting it

Status: accepted (2026-09-27). Extends 0002 and 0010; supersedes 0009's in-step run for the
tools an edge hosts.

Context: the engine ran every tool call in its own step, over the directory the engine's
process ran in. With one engine per database (ADR 0015) and conversations in many
directories, that ran a conversation's tools wherever the engine happened to start, and a
turn recovered after a restart elsewhere ran them there. Choosing a host per turn from the
conversation's place still had the engine reading and changing files. The engine should
touch no files: a tool that acts on a machine belongs to whatever sits on that machine for
the conversation, which is an edge. The alternatives: route by edge (a request then dies
with the edge that was meant to take it), or keep tools in the engine and pass it
directories (the engine then needs the machine).

Decision:

- **A hosted tool call is a request row** (`grit.tool_requests`), keyed by its slot
  (`CallSlot.key`, `tool:{workflow}:{round}:{index}`), written by the turn and never
  rewritten but by its phases: `open`, `claimed`, `answered`, or `expired`. Each phase is one
  conditional `UPDATE`, so the first claim wins, an expired request cannot be claimed, and
  an answer after the turn stopped waiting fails.
- **Requests are addressed to a workspace, never to an edge.** For a TUI the workspace is the
  conversation's place. An edge registers the places it hosts (`grit.edges`,
  `grit.edge_places`) and is live while its desk's connection holds its advisory lock.
- **Routing is one decision, `Edges.authorize`**, run when an edge claims: an edge serves
  only the places it registered, and only a directory place has a root to run tools over.
  The permission model will plug in there. Every request records its conversation, its
  workspace, the principal it acts for (a placeholder, `local`), how it was let through (its
  permit), and the claiming edge.
- **The claim is the attempt marker.** A claimed request whose edge died is an orphan,
  settled by the retry its tool declares (`ToolSpec.retry`), never by its gate: `Rerun`
  claims it again for the edge that finds it, in one statement; `Interrupt` answers it
  `Interrupted`, and it is never run again. The gate is permission; the retry is idempotency.
- **The answer is written on the row, then sent** to the waiting turn under the slot's key as
  topic and idempotency key (ADR 0010's mechanism); the row is the truth when a send is lost.
- **A protocol version rides on each request**; an edge answers a later one that it is too
  old.
- **Engine tools stay in the step**: a tool that touches no machine (reading grit's store, a
  model probe) runs in the turn as before, under ADR 0009.

Consequences: no open edge means no access, which the turn is told. A tool call costs a
round trip through Postgres (a dispatch, a NOTIFY, a claim, the run, an update, a send, the
turn's wake) instead of a function call. Requests are journal-class: `PeriodStore.purge`
deletes a period's turns' requests with their entries. Enforced by `EdgesContract` (in
memory and SQL), `EdgesTests`, `ServerTests`, `EdgeLiveTests` and `PeriodContract`.
