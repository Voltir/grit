# 0004. Workflows evolve by patch within a compatibility epoch

Status: accepted (2026-09-23); amended (2026-10-02): the engine's epoch is DBOS's latest

Context: DBOS resumes a workflow only on an executor whose application version equals the
one the workflow was started under, and it replays recorded step outputs by position,
throwing `DBOSUnexpectedStepException` when a step's name differs (seen live for `step`
and `txStep`). Its default version is a hash of the registered method alone. grit
registers every workflow through `DurableWorkflow.run`, which no change to a turn body
touches, so that hash had never changed: every build resumed every turn in flight, and
nothing checked that it could. The alternatives were to bump a version on every
incompatible change (and drain or abandon the turns in flight each time), to derive the
version from a hash of typed step descriptors (blind to control flow, and more machinery
in core), or to use DBOS 1.0.0's own patching, which records a marker so that a workflow
in flight takes the old branch. Semver was considered for the version and turned down:
DBOS routes by exact string, so a "compatible" minor bump strands the turns in flight like
any other.

Decision: grit sets the app version itself to a **compatibility epoch**, a date string
(`Turn.Epoch`), and enables DBOS patching (`Engine.open`).

- Within an epoch, a change to a workflow's steps must replay every history recorded
  under that epoch. A change that would not is guarded with `Durable.patch(name)`, old
  steps on its false branch, and later retired with `Durable.deprecatePatch(name)`.
- The epoch changes only for a break a patch cannot carry. Turns in flight under the old
  epoch are then never resumed by the new engine. What happens to them is policy, not yet
  decided.
- **The engine holding the lock (ADR 0015) makes its epoch the database's latest version**
  (amended 2026-10-02, transact 1.2.0). Since DBOS 1.1 a workflow enqueued with no version,
  as every grit enqueue is, is dequeued only on the latest version. It has no steps yet, so
  any epoch may run it; recovery still takes only the engine's own epoch's workflows.
- The gate is a replay test: `TurnReplayTests` runs today's turn body over every history
  in `core/turn/test/histories/{epoch}/` through `InMemoryDurable`, which models DBOS's
  replay and patch semantics, and fails on a renamed, reordered or dropped step, an output
  the codec cannot read, or a body that ends before its history does. Histories come from
  `RecordTurnHistories` (one per shape a turn can leave behind) and from Postgres through
  `scripts/capture-history.sh`, and are never overwritten.

Consequences: a planted rename or an unguarded new step fails `./mill __.test`; the same
change behind a patch passes (both seen). Changing a workflow is now a decision per
change: patch it or start an epoch. Patch branches accumulate until deprecated, and nothing
yet tells when no turn of the old branch remains. The gate is only as good as its
histories: a shape no fixture records is not checked. Starting an epoch means recording
its first histories. Forking a turn (`DBOS.forkWorkflow`) is ruled out until a turn's
identity no longer comes from its workflow id, since a fork gets a new one.
