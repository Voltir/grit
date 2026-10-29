# 0051. An edge waits on the store through one listener, woken by notices its writers send on commit, and reads as its asker

Status: accepted (2026-10-29)

Context: a session working beside grit, a harness's or later a grit agent's, waits for
something in the store to happen: an artifact reaching a state, a commit recorded in a
worktree. Polling its rows costs a read per waiter per interval and answers late. Turned down:

- Triggers that notify on every row: they fire per row, not per transaction, and hide what is
  sent from the code that writes.
- One listening connection per waiter: a connection is the scarcest thing a deployment holds,
  and the pool may not lend one that keeps a `LISTEN`.
- Sharing the desk's connection (ADR 0017): it holds the edge's registration lock, so a
  listener reconnecting would drop the edge's registration.

Decision: `EdgeWatch.until` waits for a `WatchFor`, data a request carries: a plugin's
document holding one of some refs, or a commit at or after an instant by a session's holder or
in a worktree.

- **Writers notice as they commit.** A write that keeps a document's body sends `plugin/key`
  (a withdrawal, which no watch can meet, sends none), and a recording that logs a commit sends
  an empty notice, inside the writer's own transaction, so a rolled-back write is never heard.
  A new writer of either sends the same.
- **One listener per engine or link,** on a connection of its own, rings only the watches a notice may
  meet. A watch registers, the listener is up, and only then does the watch read; the listener
  rings every watch each time it begins listening, so no commit after a watch begins goes
  unread. While it is down, watches read again on a fixed interval.
- **Each read is the asker's:** a read-only transaction for the person the account is linked
  to, in the room (`Subject.Person`), as a command runs (ADR 0048). A write that person does
  not read never meets their watch, and the notice says nothing a watch reports.
- **Bounded:** a fixed number of watches at once, the next refused, and a longest wait. A watch
  blocks its caller's thread for its whole wait.

Consequences: waiting costs a read per notice that may meet it, not per interval. No table, no
recorded step and no workflow is added, so replay is unaffected. Every writer of a document's
body or a session's commit must notice; one that does not leaves its watches waiting until
they lapse. The desk's own wait (ADR 0017) still listens on its registration connection: a
second listener per process, folded into this one when grit's own turn waits on the store.
Enforced by `SqlNoticesTests` and `SqlWatchTests`.
