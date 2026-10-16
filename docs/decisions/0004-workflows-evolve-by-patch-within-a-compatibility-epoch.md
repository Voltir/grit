# 0004. Workflows evolve by patch within a compatibility epoch

Status: accepted (2026-09-23); revised (2026-10-15)

Context: DBOS resumes a workflow only on an executor whose application version equals the
one it was started under, and replays recorded step outputs by position, throwing when a
step's name differs. Its default version hashes the registered method alone, which no change
to a workflow body touches, so every build would resume every workflow in flight unchecked.
The alternatives: bump a version on every incompatible change (draining or abandoning what
is in flight each time); derive the version from a hash of typed step descriptors (blind to
control flow, and more machinery in core); or DBOS's own patching, which records a marker so
that a workflow in flight takes the old branch. Semver was turned down: DBOS routes by exact
string, so a "compatible" minor bump strands workflows in flight like any other.

Decision: grit sets the application version itself to a **compatibility epoch**, a date
string (`Turn.Epoch`), and enables DBOS patching (`Engine.open`).

- **Within an epoch**, a change to a workflow's steps must replay every history recorded
  under it. A change that would not is guarded with `Durable.patch(name)`, the old steps on
  its false branch, and later retired with `Durable.deprecatePatch(name)`.
- **The epoch changes only for a break a patch cannot carry.** Workflows in flight under the
  old epoch are then never resumed by the new engine; what happens to them is policy, not
  yet decided. A new epoch starts with its first recorded histories.
- **The engine holding the lock (ADR 0015) makes its version the database's latest.** A
  workflow enqueued with no version, as every grit enqueue is, is dequeued only on the
  latest version; it has no steps yet, so any epoch may run it, while recovery takes only the
  engine's own version's workflows.
- **A deployment's engines open under the epoch joined with its own version**
  (`Deployment.runsUnder`, `Turn.under`: `{epoch}+{version}`). The kit owns the epoch; a
  deployment adds no workflow or step, so its version (`DeploymentVersion.First` until it
  changes) changes only for a change of its own that a workflow in flight could not survive,
  and strands its workflows as an epoch change does. Histories and their gate are the
  epoch's alone (`Turn.epochOf`).
- **The gate is replay**: today's workflow bodies run over every history of the epoch
  (`core/{turn,lifecycle,job}/test/histories/{epoch}/`) through
  `InMemoryDurable`, which models DBOS's replay and patch semantics, and fail on a renamed,
  reordered or dropped step, an output the codec cannot read, or a body that ends before its
  history does. Histories are recorded by the suites that make each shape a workflow can
  leave behind, and from Postgres by `scripts/capture-history.sh`, and are never
  overwritten. The read gate (ADR 0038) reads the same histories.

Consequences: changing a workflow is a decision per change: patch it or start an epoch.
Patch branches accumulate until deprecated, and nothing yet tells when no workflow of the
old branch remains. The gate is only as good as its histories: a shape no fixture records is
not checked. Forking a workflow (`DBOS.forkWorkflow`) is ruled out while a turn's identity
comes from its workflow id, since a fork gets a new one. Enforced by the replay gate,
`TurnReplayTests` and `LifecycleReplayTests`.
