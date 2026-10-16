# 0028. A ledger document is versioned, kept at a label, scoped to a place, ranked beside entries, and counted when placed

Status: accepted (2026-10-05); revised (2026-10-16)

Context: ADR 0011 gives a plugin cache and ledger documents. A knowledge index needs the
ledger: short documents (a summary pointing to where the full text lives, a topic, a rule)
that outlive their closings, change as their sources change, are drawn on by windows as
entries are, and are let go when unused. The close alternative was updating a document in
place: a window could then no longer be rebuilt as it was, which replay and the eval both
need. Counting use from searches was turned down: a search hit is a candidate, not a use.

Decision:

- **A ledger document has a plugin, a key, a label and a version** (`grit.documents`). A new
  version supersedes the one before under its key and label and never edits it; at most one
  version per key and label is current, so a key may hold variants at several labels.
  Withdrawing a document is a version with no body. A plugin's version moves its posting
  cursor, never its ledger.
- **A document is kept at its label joined with its writer's floor** (`DocumentKeeper.write`,
  `Clearance.floor`, ADR 0030), never below what its writer could read, in the writer's own
  room or none, and believed at the integrity its writer gives (ADR 0036). A plugin reads
  only what its transaction is cleared for (`DocumentShelf`).
- **A ledger document has a place** (ADR 0013), and a window's candidates include the current
  documents whose place its scope reaches and whose label the window reads. They rank in the
  entries' pool and budget, their scores multiplied by a weight their plugin declares,
  because a second index has statistics of its own (ADR 0005). The model is shown them as
  documents under their plugin's label, never as turns.
- **A plugin declares its ledger's retention window and bound.** A superseded or withdrawn
  version is kept for the retention window, so a window rebuilt as of a past turn shows what
  that turn saw, then deleted through a tombstone (ADR 0014). Over the bound, counted over
  every label, current documents are withdrawn least recently placed first, a document
  counting as placed when written.
- **A withdrawal at a label below its writer's floor is accepted**: a withdrawal holds no
  body, so it discloses nothing of what the run read.
- **A document may hold a pointer** (`Pointer`): the one hosted call that reads what it
  stands for. It names the tool as the model is offered it (an MCP server's tool under its
  server's prefix), so a window shows the line only to a turn offered that name, and a call
  follows it by comparing names alone. Its arguments never name a destination (ADR 0031): a
  pointer names a read.
- **Core counts placement and follows.** The step that records a window records each document
  it holds by version and, in the same transaction, each one's placement count and last
  placement. The step that appends a turn's reply, or its draft, counts in its own
  transaction each of the window's documents whose pointer one of the turn's calls followed
  and that succeeded. Any other measure of use is its plugin's own.
- **A document's source content goes to a model only as any send does**: to a party cleared
  for it (ADR 0031).

Consequences:

- A plugin can keep bounded knowledge that outlives every conversation it came from, and see
  which documents earn their place and which are read through; forgetting a pointer loses
  nothing it pointed to.
- The weight makes what documents add measurable on the eval; a window's record says which
  versions it showed.
- Recording a window's documents and counting follows add writes inside `assemble`,
  `record-window` and `append`, not steps: no patch (ADR 0004), and a replayed turn counts
  nothing again. Enforced by the replay gate (`TurnReplayTests`, `documented.json`) and
  `DocumentContract`.
