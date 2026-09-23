# 0002. Edges reach the engine only through Postgres

Status: accepted (2026-09-23)

Context: grit is meant to run three ways:

- a local TUI harness on a local Postgres;
- a cloud agent that is chatted with and triggered from Slack;
- a lightweight cloud mode for structured, triggered tasks, such as a scheduled run whose
  result is then judged.

The engine is the durable turn, memory and assembly, and it is the same in all three. What
differs is how work arrives, where output goes, where tools run, and the configuration. In
the local mode, the engine and the TUI can share one process. The alternative was to let
the TUI call turn orchestration directly there. It was turned down because the Slack and
schedule edges cannot work that way, which would leave two paths into a turn. It would also
give durability a second path: output that exists only in memory before it reaches a row.

Decision: an *edge* is anything that brings work in or carries output out: the TUI, Slack,
a schedule or a webhook. It talks to the engine only through Postgres:

1. It records the inbound message as a row, idempotent on the source's own id.
2. It starts or enqueues the turn by its deterministic workflow id.
3. It reads the turn's durable events, and NOTIFY for liveness.
4. It answers the turn's durable waits, such as approvals, by sending messages.

The edge side needs only the database, so an edge can run in another process against the
same database. In local mode the engine still runs in the TUI's process, but the TUI never
calls into it.

Consequences:
- A TUI can attach to a cloud instance's database, and Slack, a schedule and the TUI start
  a turn in exactly the same way.
- The source's own id makes redelivery harmless. This covers Slack's retries and schedule
  backfills.
- The workflow receives only ids, so real workflow arguments are never needed.
- The cost is local latency and indirection: every keystroke that submits goes through a
  row and a workflow start, not a method call.
- Enforcement: none mechanical yet. When edges have code, an enola rule forbidding
  edge modules from importing `grit.turn` is the candidate check.
