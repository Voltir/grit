# grit.assembly

The `ContextAssembler`s: what builds each turn's window.

In dependency order:

- **`estimate`**: `CharEstimate`, the fallback `TokenEstimator`. Imports nothing in assembly.
- **`linear`**: `LinearAssembler`, the closing entries that open the turn's period (as
  many as the settings in force say, paid for first), then the most recent whole turns of
  the period that fit. The baseline every other assembler is measured against.
  ← `estimate`
- **`retrieval`**: `RetrievalAssembler`, the same closing entry, the recent tail, plus
  what a query `QueryWriter` has a model write finds: the period's earlier turns, and the
  turns of other conversations' open periods its scope holds and their kept closings
  (ADR 0013), ranked by an `EntrySearch` in one pool, its own weighted up; those from
  elsewhere shown as their own sections, one per conversation; and a thread that begins with
  grit's post, the turn that asked for it, whatever the scope. ← `linear`

Each draws its window as wide as it was built to, or within the budget (and, for retrieval,
the hits per search) a request's `Width` names. Neither reaches past its own period (ADR 0011): what came before the turn's is its closing
entry. Only retrieval draws on other conversations.

No source file sits at the module's root, and the test tree mirrors it. The assembly eval
lives in its own module, `grit.eval`.
