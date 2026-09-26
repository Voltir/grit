# grit.assembly

The `ContextAssembler`s: what builds each turn's window.

In dependency order:

- **`estimate`**: `CharEstimate`, the fallback `TokenEstimator`. Imports nothing in assembly.
- **`linear`**: `LinearAssembler`, the closing entries that open the turn's period (as
  many as the settings in force say, paid for first), then the most recent whole turns of
  the period that fit. The baseline every other assembler is measured against.
  ← `estimate`
- **`retrieval`**: `RetrievalAssembler`, the same closing entries, the recent tail, plus
  the period's earlier turns that match a query `QueryWriter` has a model write, ranked by
  an `EntrySearch` bounded to the period. ← `linear`

Both see only the turn's own period (ADR 0011): what came before it is its closing
entries.

No source file sits at the module's root, and the test tree mirrors it. The assembly eval
lives in its own module, `grit.eval`.
