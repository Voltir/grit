# 0003. `Durable` is an exclusive capability, and separation checking enforces it

Status: accepted (2026-09-23)

Context: the turn and assembly run durable operations through a `Durable` capability in
`grit.core` (step, `transact`, later `recv`). A step's body must not use the `Durable` it
runs on: a nested step records operations inside a step's function id, and DBOS refuses
`recv` inside a step only at runtime (`DBOSExecutor`: "must not be called from within a
step"). Capture checking alone can't express "any capability but this one", so the
alternatives were a runtime check with a test, or a pure body type, which would also
forbid the `Provider` and `Tx` a step needs. Scala's experimental separation checking can
express it: a parameter typed `() => A` hides the body's captures, and a call is rejected
when they overlap the method prefix's. A spike on 3.9.0 compiled a body capturing a
`Provider`, sequential steps, the `using` form and a `Tx` body, and rejected a nested
step, the same closure bound to a `val` first, and a step inside `transact`.

Decision: `Durable` extends `caps.ExclusiveCapability`, and its step methods take their
body as `() => A` (or `(Tx^) ?=> A`). `-language:experimental.separationChecking` is on in
every module through `GritModule.separationChecking`, except `grit.tui` and its examples.
Their FFM terminal seam hands byte arrays to Java, which separation checking treats as
`Mutable` against pure Java signatures, and under ADR 0002 an edge never holds a `Durable`.

Consequences:
- A nested step, or `recv` inside a step, is a compile error where the turn and assembly
  are written, not a runtime surprise.
- Other capabilities may be captured freely by step bodies; only `Durable` is exclusive.
- It stacks a second experimental feature on capture checking. Code that aliases mutable
  state now needs `Stateful`/`Mutable` or `@untrackedCaptures`, and an array handed to
  Java needs `caps.unsafe.unsafeAssumePure`, only in quarantine modules.
- If `grit.tui` ever needs a `Durable`, this is violated silently: the flag is off there.
  Turning it on in tui is parked (`.local/backlog/tui-separation-checking.md`).
- Enforcement: the compiler, pinned by `grit.core.durable.SeparationTests`, which compiles probes
  against core with core's own flags: the three rejected forms must fail, the allowed forms
  must compile, and the nested step must compile with the flag off. A compiler change that
  stops rejecting a nested step fails the build (watched failing with `Durable` shared).
  The suite is temporary: it is deleted once separation checking leaves experimental
  status, and until then re-read at every Scala upgrade.
