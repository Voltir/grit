# 0017. A tool call is a durable request addressed to a workspace, and run by an edge hosting it

Status: accepted (2026-09-27); amended (2026-09-30): service places; amended (2026-10-01): a
service place's reach; amended (2026-10-01): services an addressed turn reaches; amended
(2026-10-02): the turn is rung, and reads the answer from the row; amended (2026-10-08): a
request records whom its actor acts for. Extends
0002 and 0010; supersedes 0009's in-step run for the tools an edge hosts.

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
  workspace, the principal it acts for, how it was let through (its permit), and the claiming
  edge. (Amended 2026-10-08; ADR 0034.) The principal is whom its actor acts for, read in the
  transaction that writes it: a turn's asker, a job's run's schedule's principal; when none can
  be read, the request is not sent. The placeholder `local` is gone.
- **The claim is the attempt marker.** A claimed request whose edge died is an orphan,
  settled by the retry its tool declares (`ToolSpec.retry`), never by its gate: `Rerun`
  claims it again for the edge that finds it, in one statement; `Interrupt` answers it
  `Interrupted`, and it is never run again. The gate is permission; the retry is idempotency.
- **The answer is written on the row, then the waiting turn is rung** under the slot's key as
  topic and idempotency key (ADR 0010's mechanism), with a constant that carries no outcome;
  the turn reads the answer from the row in the step that keeps it, so the row is its one
  transport copy, and the truth when a ring is lost.
- **A protocol version rides on each request**; an edge answers a later one that it is too
  old.
- **A workspace is a directory or a service place (later, a repository place).** An
  outside service's tools are hosted by an edge at a `service:` place it registers;
  `Edges.authorize` routes a request there to the place itself, and refuses any registered
  place that is neither a directory nor a service. A conversation with no directory of its
  own works in the service place its deployment links it to (`WorksIn`), first link first;
  a stored per-conversation link will override that default. One workspace per turn: it is
  where the conversation works, and its hosted calls go there, but for one exception. An
  addressed turn is also offered the tools advertised at each service its deployment links
  the conversation's place to (`Reaches`), tools that act beyond the conversation, such as
  posting elsewhere; less any whose name an earlier tool has. The offer records which tool
  goes to which place, and a call to a reached tool is a request addressed to that place.
  A turn rooted on a message grit heard is offered none of them: its draft runs tools, and
  only its reply is gated by whether grit may speak unprompted. So anyone who can address
  the deployment may ask it to act at a reached service, bounded by what that service's edge
  allows (where it may act, how often); the principal a request records is where a bound on
  who will plug in. A service place's reach is bounded at the edge hosting it, by exact
  argument bounds and attributed answers, failing closed: a tool it cannot hold to them is
  not offered, and a call outside them is not sent. Words to the
  model are an aid, never the guard; nor is the service's token, which may reach further.
- **The engine offers what an edge advertises.** A hosted tool the engine does not
  describe is offered as the serving edge advertised it, when it does not ask first; its
  arguments are read by the edge. The offer records which tools it took from the advert,
  its workspace's or a reached service's, so a replay rebuilds them without the edge.
- **Engine tools stay in the step**: a tool that touches no machine (reading grit's store, a
  model probe) runs in the turn as before, under ADR 0009.

Consequences: no open edge means no access, which the turn is told. A tool call costs a
round trip through Postgres (a dispatch, a NOTIFY, a claim, the run, an update, a ring, the
turn's wake and its read) instead of a function call. Requests are journal-class:
`PeriodStore.purge` deletes a period's turns' requests with their entries. Enforced by `EdgesContract` (in
memory and SQL), `EdgesTests`, `ServerTests`, `EdgeLiveTests`, `PeriodContract`, for a
service place's reach, `McpScopeTests` and `McpClientTests`, and, for a reached service,
`TurnOfferTests` and `TurnHostedTests`.
