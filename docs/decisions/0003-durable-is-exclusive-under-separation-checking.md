# 0003. `Durable` is an exclusive capability, and separation checking enforces it

Status: accepted (2026-09-23)

Context: durable operations run through a `Durable` capability in `grit.core` (a step,
`transact`, `recv`). A step's body must not use the `Durable` it runs on: a nested step
records operations inside a step's function id, and DBOS refuses `recv` inside a step only at
runtime. Capture checking alone can't express "any capability but this one", so the
alternatives were a runtime check with a test, or a pure body type, which would also forbid
the `Provider` and `Tx` a step needs. Scala's experimental separation checking can express
it: a parameter typed `() => A` hides the body's captures, and a call is rejected when they
overlap the method prefix's.

Decision: `Durable` extends `caps.ExclusiveCapability`, and its step methods take their body
as `() => A` (or `(Tx^) ?=> A`). `-language:experimental.separationChecking` is on in every
module through `GritModule.separationChecking`, except `grit.tui`, its examples and
`dev/outline`. The terminal's FFM layer hands byte arrays to Java, which separation checking
treats as `Mutable` against pure Java signatures; under ADR 0002 an edge never holds a
`Durable`, and the outline tool is no part of grit's runtime.

Consequences:
- A nested step, a step inside `transact`, or `recv` inside a step is a compile error where
  workflows are written, not a runtime surprise.
- Other capabilities may be captured freely by step bodies; only `Durable` is exclusive.
- It stacks a second experimental feature on capture checking. Code that aliases mutable
  state needs `Stateful`/`Mutable` or `@untrackedCaptures`, and an array handed to Java
  needs `caps.unsafe.unsafeAssumePure`, only in quarantine modules (ADR 0001).
- If `grit.tui` ever needs a `Durable`, this is violated silently: the flag is off there.
- Enforced by the compiler, pinned by the probe `grit.core.durable.SeparationTests` (in
  `grit.core.probes`): the rejected forms must fail, the allowed forms must compile, and the
  nested step must compile with the flag off. The commit gate runs it when the Scala version
  or core's capture syntax changes. The suite is deleted once separation checking leaves
  experimental status, and until then re-read at every Scala upgrade.
