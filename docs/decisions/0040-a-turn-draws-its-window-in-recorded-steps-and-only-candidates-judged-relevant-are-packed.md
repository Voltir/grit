# 0040. A turn draws its window in recorded steps, and only candidates judged relevant are packed

Status: accepted (2026-10-16)

Context: a turn's window was drawn in one `assemble` step, with its search query written by a
model call inside that step and nothing recorded between. A crash mid-assembly paid the query
writer again, the pool the search ranked was never kept, and nothing judged whether what BM25
ranked high was about the turn at all: whatever fit the budget was packed. Turned down: judging
relevance in a workflow of its own before the turn, rather than as the turn's own step between
the assembler's reads (mentions never pass triage, ADR 0025, so it would need a second enqueue
path); one `assemble` step recording the pool and verdicts in its output (a crash
pays the writer and the classifier again); the turn's own call-then-cost steps, as `weigh` and
`offer` make them, instead of moves (no daily-cap admission, a second cost path); an assembler
holding `Models` or a `Classifier` and making the calls itself (it hides calls from the record);
judging every ranked candidate (up to about 120 a turn).

Decision:

- **The assembler reads; the turn calls.** A `ContextAssembler` is pure (`caps.Pure`) and
  offers four reads under the turn's `Db`: `prepare` (a window drawn with no search, or the
  request a query writer is asked), `search` (what a written query finds, ranked), `briefs`
  (each found candidate cut for a judge) and `assemble` (the window packed from what the turn
  admitted). Every model and classifier call between them is the turn's.
- **Each call is a move of its own.** `ask:query` writes the query, and `ask:relevance-0` to
  `ask:relevance-2` judge the found candidates, each a judgment (ADR 0034): admitted against the
  day's cap, recorded with its cost, made at most once. Between them `search` records the first
  60 found and counts the rest. A turn resumed between steps repeats no call before.
- **The first 30 found are judged, ten to a call**, each passing at a probability of 0.5 or
  more. None after the first 30 is ever packed.
- **Unjudged is admitted.** A candidate the classifier could not judge (none cleared, a failed
  call, gone before it was briefed) is packed as if it passed. That fail-open is the window's
  rule only: anything else gating on relevance reads a candidate's verdict, passed alone.
- **What a turn counted followed is recorded in `append`'s output**, beside the reply, so a
  reader rebuilds a turn's retrieval and follows from its steps alone.

Consequences:

- Up to three classifier calls and one query call a turn, each in the usage ledger under its
  move; a turn with nothing to search records `ask:query` and `assemble` alone.
- The window the model sees is what passed, so a high BM25 rank that is off topic no longer
  costs window space; the eval can re-judge or re-pack the recorded 60 offline.
- The prompt, the pass mark, the briefs' length and the batch size change freely; judging past
  30 adds step names, so it is a patch (ADR 0004); a lower cap is the eval's to measure.
- Enforced by the compiler (`caps.Pure` on `ContextAssembler`, `AssemblerCaptureTests`) and the
  replay gate (`TurnReplayTests`).
