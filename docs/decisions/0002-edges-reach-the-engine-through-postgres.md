# 0002. Edges reach the engine only through Postgres

Status: accepted (2026-09-23)

Context: grit runs three ways: a local TUI harness, a cloud agent chatted with from Slack,
and triggered tasks such as a scheduled run. The engine (the durable turn, memory and
assembly) is the same in all three; what differs is how work arrives, where output goes,
where tools run, and the configuration. In the local mode the engine and the TUI can share
one process, and the alternative was to let the TUI call turn orchestration directly there.
It was turned down because the Slack and schedule edges cannot work that way, which would
leave two paths into a turn, and would give durability a second path: output that exists
only in memory before it reaches a row.

Decision: an *edge* is anything that brings work in or carries output out: the TUI, Slack,
a schedule, an MCP server's host. It talks to the engine only through Postgres:

1. It records the inbound message as a row, idempotent on the source's own id.
2. It starts or enqueues the turn by its deterministic workflow id,
   `{conversationId}:{turnSeq}` (`TurnRef.workflowId`). The workflow receives that reference
   alone, never content: DBOS stores every workflow's inputs and every step's output.
3. It reads the turn's durable events and streams (ADR 0006), and NOTIFY for liveness.
4. It answers the turn's durable waits, such as approvals, by sending messages, and the
   requests addressed to the places it hosts by writing their rows (ADR 0017).

The edge side needs only the database, so an edge can run in another process against the
same database (ADR 0015). In local mode the engine still runs in the TUI's process, but the
TUI never calls into it.

Consequences:
- A TUI can attach to a cloud instance's database, and Slack, a schedule and the TUI start a
  turn in exactly the same way.
- The source's own id makes redelivery harmless (Slack's retries, a schedule's backfill), and
  the deterministic id makes starting a turn twice run it once.
- The cost is local latency and indirection: every submit goes through a row and a workflow
  start, not a method call.
- Only the kit opens an engine (`Engine.open` is `private[grit]`); an edge works through core's
  traits (`Inbox` and the stores) over the database.
- Enforced by the law: `slack-names-core-prose-and-edge`, `mcp-names-core-alone` and
  `tui-names-core-alone` keep the edge modules off the turn and the engine.
