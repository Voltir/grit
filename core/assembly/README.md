# grit.assembly

The `ContextAssembler`s: what builds each turn's window.

In dependency order:

- **`estimate`**: `CharEstimate`, the fallback `TokenEstimator`. Imports nothing in assembly.
- **`linear`**: `LinearAssembler`, the closing entries that open the turn's period (as
  many as the settings in force say, paid for first), then the most recent whole turns of
  the period that fit. The baseline every other assembler is measured against.
  ← `estimate`
- **`retrieval`**: `RetrievalAssembler`, the same closing entry, the recent tail, plus
  what a query finds, which the turn has a model write from `QueryWriter`'s request: the period's earlier turns, and the
  turns of other conversations' open periods its turn may read and their kept closings
  (ADR 0013), the enabled plugins' documents it may read, current at the turn's
  root (ADR 0028), and their kept files' pieces likewise, at most eight of one file (ADR 0046),
  ranked by an `EntrySearch`, a `DocumentSearch` and a `PieceSearch` in one pool, its own
  weighted up and each document or piece by its plugin's weight; those from elsewhere shown as their
  own sections, one per conversation; and a thread that begins with grit's post, the turn that
  asked for it. ← `linear`

Neither makes a call: a `ContextAssembler` is pure, its window drawn in reads a turn records
apart (`prepare`, `search`, `briefs`, `assemble`), the query written and what it found judged
between them (ADR 0040).
Each draws its window as wide as it was built to, or within the budget (and, for retrieval,
the hits per search) a request's `Width` names. Neither reaches past its own period (ADR 0011): what came before the turn's is its closing
entry. Only retrieval draws on other conversations.

No source file sits at the module's root, and the test tree mirrors it. The assembly eval
lives in its own module, `grit.eval`.
