# 0028. A ledger document is versioned, scoped to a place, ranked beside entries, and counted when placed

Status: accepted (2026-10-05), revised (2026-10-05), revised (2026-10-11)

Context: ADR 0011 gives a plugin cache and ledger documents and built only the cache. A
knowledge index needs the ledger: short documents (a summary pointing to where the full text
lives, a topic, a rule) that outlive their closings, change as their sources change, are
drawn on by windows as entries are, and are let go when unused. The close alternative was
updating a document in place: a window could then no longer be rebuilt as it was, which
replay and the eval both need. Counting use from searches was turned down: a search hit is a
candidate, not a use.

Decision:

- **A ledger document has a plugin, a key and a version.** A new version supersedes the one
  before and never edits it; at most one version per key is current. Withdrawing a document
  is a version with no body. A plugin's version moves its posting cursor, never its ledger.
- **A ledger document has a place** (ADR 0013), and a window's candidates include the current
  documents whose place its scope reaches. They rank in the entries' pool and budget, their
  scores multiplied by a weight their plugin declares (data, as ADR 0013's own-conversation
  weight), because a second index has statistics of its own (ADR 0005). The model is shown
  them as documents under their plugin's label, never as turns.
- **A document is shown wherever its place is in scope, to whoever is there** (ADR 0013:
  locality is not visibility). A plugin keeps nothing whose source is readable by fewer
  people than its place's, until visibility is a semantic of its own.
- **A plugin declares its ledger's retention window and bound.** A superseded or withdrawn
  version is kept for the retention window, so a window rebuilt as of a past turn shows what
  that turn saw, then deleted through a tombstone (ADR 0014). Over the bound, current
  documents are withdrawn least recently placed first, a document counting as placed when
  written.
- **A document may hold a pointer: the one hosted call that reads what it stands for.** It
  names the tool as the model is offered it (an MCP server's tool under its server's prefix),
  so a window shows the line only to a turn offered that name, and a call follows it by
  comparing names alone. Its arguments never name a destination (ADR 0031): a pointer names a
  read, and a call naming its destination writes.
- **Core counts placement and follows.** The step that records a window records each document
  it holds by version and, in the same transaction, each one's placement count and last
  placement. The step that appends a turn's reply, or its draft for a turn rooted on a heard
  message, counts in its own transaction each of the window's documents whose pointer one of
  the turn's calls followed and that succeeded: a refused or failed call follows nothing. Any
  other measure of use is its plugin's own.
- **A withdrawal at a label below its writer's floor is accepted.** A run withdraws a key at
  the label it is kept at, which may be lower than the run's own: the bound's least recently
  placed, or a document whose source's label was lowered. A withdrawal holds no body, so it
  discloses nothing of what the run read.
- **A document's source content may go to the model that summarises it, at any label**, by the
  deployment's declaration of that model, until trust routes models.

Consequences:

- A plugin can keep bounded knowledge that outlives every conversation it came from, and see
  which documents earn their place, and which are read through; forgetting a pointer loses
  nothing it pointed to.
- The weight makes what documents add measurable on the eval; a window's record says which
  versions it showed.
- Recording a window's documents changes what `assemble` records and `record-window`
  writes, not the turn's steps: no patch (ADR 0004). A window without documents is recorded
  as before, and a recorded turn whose window held one replays with the rest
  (`TurnReplayTests`, `documented.json`).
- Counting follows adds writes inside `append` and changes neither its name nor its output:
  no patch, and a replayed turn counts nothing again.
