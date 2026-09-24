# grit.assembly

The `ContextAssembler`s: what builds each turn's window.

In dependency order:

- **`estimate`**: `CharEstimate`, the fallback `TokenEstimator`. Imports nothing in assembly.
- **`linear`**: `LinearAssembler`, the most recent whole turns that fit. The baseline
  every other assembler is measured against. ← `estimate`
- **`retrieval`**: `RetrievalAssembler`, the recent tail plus the earlier turns that match
  a query `QueryWriter` has a model write, ranked by an `EntrySearch`. ← `linear`

No source file sits at the module's root, and the test tree mirrors it. The assembly eval
lives in its own module, `grit.eval`.
