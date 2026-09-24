# grit.assembly

Assembly: builds every turn's context window from scratch. `LinearAssembler` (everything
before the turn, oldest first) is the baseline; retrieval, relevance checks and LSP
context are later `ContextAssembler`s. Read-only: it sees seams, never an implementation.

Design: `roadmap/mechanisms/assembly.md`.
