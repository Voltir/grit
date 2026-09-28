# 0013. A conversation has a place, and open periods near it in scope are retrieval candidates

Status: accepted (2026-09-26), revised (2026-09-28)

Context: grit runs in several directories and sessions at once, and later in Slack channels
and threads, and what one conversation is doing is often what another needs. A conversation
was keyed by its origin alone, so a TUI session of one name was one conversation in every
directory, and a window drew only on its own conversation and its newest closing. The
choices: where a local conversation is (the directory, the checkout root, the git identity);
how places relate (a tree, a graph, a distance); which other conversations' entries a window
may draw on (any, the recent, the open); and how those compete with the conversation's own.

Decision:

- **A conversation has a place**: a path in one containment tree whose root is everywhere,
  one namespace per source. A TUI session's place is the real path of the directory it runs
  in, under `fs`; a Slack thread's is under `slack`, a task's run under `task`. The place is
  a function of the origin, and a TUI session's directory is part of its origin: the same
  session name in another directory is another conversation. A git repository is not a
  level of the tree.
- **Places are materialized paths** (`text[]`, segments verbatim). One is within another
  when the other's path is a prefix of its own, defined once, outside SQL.
- **Near is a scope, not a distance**: a set of place prefixes, each a place or the
  conversation's own **room** (the place it shares with its neighbours: a session's
  directory, a thread's channel, a task's name), held as data, everywhere by default. An
  empty scope turns cross-place recall off.
- **Open periods and closings.** The raw entries of other conversations' open periods in
  scope are retrieval candidates. So is each such conversation's kept closings, as one
  candidate shown as its newest kept record, under its place and close date, with its
  Standing lines' grounds as a record shows them (ADR 0018). A conversation with open turns
  among the candidates is shown by those alone. Nothing else a close wrote elsewhere is a
  candidate.
- **One pool.** They rank by BM25 with the conversation's own candidates, whose scores are
  multiplied by a weight (data, at least 1), in one budget with no reserved share.
- **The model is shown them as one section per conversation**, labelled with its place,
  never as turns of its own conversation, ahead of the conversation's closing and turns.
- **A close is shown what its period drew from elsewhere**, turns and records, as known, and
  never records it as its own: a place's balance holds only what happened there.
- **Locality is not visibility**: a place ranks what a window holds, and grants nothing.

Consequences:

- Recall across sessions, repositories and threads needs no link or configuration. Retrieval
  writes its search query on a period's first turn whenever anything in scope is open or has
  a closing kept: one small model call.
- A thread can answer from what a closed thread settled. A record costs its whole text when
  it wins a place in the window.
- Ranking across conversations relies on the index's corpus statistics being global
  (ADR 0005): one query's scores are comparable across conversations, and anything that must
  join the pool has to be in that index.
- A window names entries of other conversations, which can be purged or collected before a
  crashed turn resumes; the turn then shows what remains of them.
- A balance elsewhere is reached only as a whole record, ranked by its flows, beside raw
  turns in one BM25 pool. Judging a balance's lines one by one, as the relevance judge will,
  is deferred to that judge, not ruled out; the pooled record is what runs until then.
- Every open period and closing in scope competes for the window; narrowing the default is a
  setting, not a migration (`grit serve` seeds `room`). Links and visibility labels can be
  added without reshaping the tree. The room narrows what a window draws, and grants or
  hides nothing: it is not visibility.
- Enforced by `PlaceTests` (scopes and rooms), the period contract (`openElsewhere`,
  `closedElsewhere`), `SearchLiveTests` (one scale across conversations, closings
  included), `RetrievalAssemblerTests`, `ShownTests` and the replay gate.
