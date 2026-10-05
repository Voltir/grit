# 0028. A ledger document is versioned, scoped to a place, ranked beside entries, and counted when placed

Status: accepted (2026-10-05)

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
- **Core counts placement.** The step that records a window records each document it holds
  by key and version and, in the same transaction, each one's placement count and last
  placement. Any other measure of use is its plugin's own.

Consequences:

- A plugin can keep bounded knowledge that outlives every conversation it came from, and see
  which documents earn their place; forgetting a pointer loses nothing it pointed to.
- The weight makes what documents add measurable on the eval; a window's record says which
  versions it showed.
- Recording a window's documents changes a turn step: a patch (ADR 0004).
