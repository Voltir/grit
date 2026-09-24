# grit.assembly

The `ContextAssembler`s: what builds each turn's window.

In dependency order:

- **`estimate`**: `CharEstimate`, the fallback `TokenEstimator`. Imports nothing in assembly.
- **`linear`**: `LinearAssembler`, the most recent whole turns that fit. The baseline
  every other assembler is measured against. ← `estimate`

No source file sits at the module's root. The test tree mirrors it; the assembly eval
(`eval`) is test-only.
