# 0013. A conversation has a place, and open periods near it in scope are retrieval candidates

Status: accepted (2026-09-26); revised (2026-10-16)

Context: grit runs in several directories and sessions at once, and in Slack channels and
threads, and what one conversation is doing is often what another needs. A conversation keyed
by its origin alone was one conversation in every directory, and a window drew only on its
own conversation. The choices: where a local conversation is (the directory, the checkout
root, the git identity); how places relate (a tree, a graph, a distance); which other
conversations' entries a window may draw on (any, the recent, the open); and how those
compete with the conversation's own.

Decision:

- **A conversation has a place**: a path in one containment tree whose root is everywhere,
  one namespace per source. A TUI session's place is the real path of the directory it runs
  in, under `fs`; a Slack thread's is under `slack`, a task's run under `task`. The place is
  a function of the origin, and a TUI session's directory is part of its origin: the same
  session name in another directory is another conversation. A git repository is not a level
  of the tree.
- **Places are materialized paths** (`text[]`, segments verbatim). One is within another when
  the other's path is a prefix of its own, defined once, outside SQL.
- **Near is a scope, not a distance**: a set of place prefixes, each a place or the
  conversation's own **room** (the place it shares with its neighbours: a session's
  directory, a thread's channel, a task's name), held as data, everywhere by default. An
  empty scope turns cross-place recall off.
- **Open periods and closings.** The raw entries of other conversations' open periods in scope
  are retrieval candidates. So is each such conversation's kept closings, as one candidate
  shown as its newest kept record, under its place and close date, with its Standing lines'
  grounds (ADR 0018). A conversation with open turns among the candidates is shown by those
  alone. Nothing else a close wrote elsewhere is a candidate.
- **One pool.** They rank by BM25 with the conversation's own candidates, whose scores are
  multiplied by a weight (data, at least 1), in one budget with no reserved share. The first
  thirty are judged for relevance, and only those passed or unjudged are packed (ADR 0040).
- **The model is shown them as one section per conversation**, labelled with its place, never
  as turns of its own conversation, ahead of the conversation's closing and turns.
- **A close is shown what its period drew from elsewhere** as known, and never records it as
  its own: a place's balance holds only what happened there.
- **Locality is not visibility**: a place, a scope and a room rank what a window holds, and
  grant or hide nothing. What a turn may read is ADR 0030's.

Consequences:

- Recall across sessions, repositories and threads needs no link or configuration. A turn
  writes a search query (ADR 0040) whenever a search could find what its window would leave
  out: one small model call, then up to three relevance calls.
- A thread can answer from what a closed thread settled. A record costs its whole text when it
  wins a place in the window.
- Ranking across conversations relies on the index's corpus statistics being global (ADR
  0005): one query's scores are comparable across conversations, and anything that must join
  the pool has to be in that index.
- A window names entries of other conversations, which can be purged before a crashed turn
  resumes; the turn then shows what remains of them.
- A balance elsewhere is reached only as a whole record, ranked by its flows, and judged
  whole; judging its lines one by one is deferred, not ruled out.
- Narrowing the default is a setting, not a migration: a deployment seeds the scope (the
  reference deployment, serving an edge, seeds `room`).
- Enforced by `PlaceTests` (scopes and rooms) and `RetrievalAssemblerTests`, over the period
  store's `openElsewhere` and `closedElsewhere`.
