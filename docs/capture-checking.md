# Capture and separation checking in grit

What grit has learned by being bitten, with each item's symptom, cause and fix. Scala 3.9.0;
both features are experimental, so re-read this at every Scala upgrade (the checklist is at
the end).

## What is on, and where

`GritModule.scalacOptions` in `build.mill` sets:

- **Capture checking** (`-language:experimental.captureChecking`) in every module.
- **Separation checking** (`-language:experimental.separationChecking`) in every module
  except `grit.tui` and `grit.tui.examples`. `def separationChecking = false` turns it off
  there. The reason is in
  [ADR 0003](decisions/0003-durable-is-exclusive-under-separation-checking.md), and the
  errors that turning it on would raise are in `.local/backlog/tui-separation-checking.md`.

## The vocabulary grit uses

- **`T^` marks a tracked capability.** `Tx` is an opaque type over `java.sql.Connection`,
  and it is tracked only where it is written `Tx^`. Without the `^`, capture checking
  cannot see a `Tx` leaving its transaction. Every store method takes `(using tx: Tx^)`.
- **`->` is a pure function, `=>` an impure one.** An impure function may capture
  anything. Parameters that must not smuggle capabilities, such as `Journaled.json`'s
  codec functions, are `->`.
- **`caps.SharedCapability`** marks a capability that can be used from many places at
  once: `Inbox`, and later `Provider`. `caps.Capability` is sealed; extend one of its
  subtraits instead.
- **`caps.ExclusiveCapability`** is for a capability that nothing else may hold while it
  is in use. `Durable` is one. Under separation checking, a parameter such as
  `body: () => A` *hides* what the argument captures, and a call is rejected when those
  hidden captures overlap the exclusive capability the method is called on. That is what
  makes a step inside a step a compile error.

## Traps

**A step body may not mention the `Durable` at all.** This covers any member read, not
only a nested `step`.

- *Symptom:* `Separation failure: argument of type (…) ?->{d} String to method transact …
  hides capabilities {d}`.
- *Cause:* separation checking tracks references to `d`, not which member is used, so
  `d.anything` inside the body puts `d` in its captures.
- *Fix:* compute what the step needs before the step and capture the plain value. This is
  why a workflow body receives its `WorkflowId` as an argument rather than from `Durable`.
  Anything that keeps `d` in its capture set, such as a helper built around `d`, has the
  same problem.

**A `var` field in a plain class.**

- *Symptom:* a separation error on the field.
- *Cause:* separation checking treats it as mutable state aliased through the object.
- *Fix:* `@caps.unsafe.untrackedCaptures` when the field only ever holds immutable values
  (the fakes, `InMemoryDurable`). Otherwise make the class `caps.Stateful`, which then needs
  `update` modifiers on the methods that mutate it. A mutable *local* is fine.

**An array handed to a Java method.**

- *Symptom:* `Found: Array[String]^{any.rd}  Required: Array[String]^{}`.
- *Cause:* separation checking treats every array as `Mutable`, and Java signatures are
  read as asking for a pure one.
- *Fix:* `caps.unsafe.unsafeAssumePure(arr)`, with a comment saying the callee only reads
  it. Only in quarantine modules and tests (`Main`, `SqlInbox`, `SeparationTests`).

**A function stored in a value must match its capture set.**

- *Symptom:* `Found: Journaled[A]^{write, read}  Required: Journaled[A]`.
- *Cause:* an anonymous class that closes over impure function parameters captures them.
- *Fix:* declare the parameters pure (`A -> ujson.Value`) if they are, which codecs are.
  Otherwise the result type must carry the capture set.

**Resources with a capture set and `Using`.**

- *Cause:* Scala 3.9's capture-checked standard library types `Using.Manager.apply` at the
  resource's own capture set. An object-level `given Releasable[Terminal]` cannot accept an
  instance that carries a capability.
- *Fix:* make the given capture-polymorphic:
  `given releasable[C^]: Using.Releasable[Terminal^{C}]` (`Terminal`, `Scheduler`).
  `Using.Releasable` still will not widen to a `Background`'s capture set; see the comment
  in `Runtime.run`.

**A local binding of a tracked value needs its capture set written out.**

- *Example:* `val conn: java.sql.Connection^{tx} = Tx.connection(tx)`.
- Inference will not widen it for you.

**Iterator-producing combinators.** `args.sliding(2).collectFirst { … }` is rejected with
`Illegal capture reference`. Indexed `Vector` code is the way out.

**A thunk in a data type launders capabilities.** This is why `grit.tui`'s `Effect` is
plain data with no function cases (`grit/tui/CLAUDE.md`, rule 3).

## Testing that something does not compile

utest's `assertCompileError` (`scala.compiletime.testing`) never reports capture- or
separation-checking errors. Capture checking runs after the typer, and a probe that should
fail compiles "cleanly" there. `EntryStoreTests` records the same limit for the `Tx`
escape.

The way that works is `grit.core.SeparationTests`. It runs `dotty.tools.dotc.Driver` in the
test, against core's run classpath with core's own `scalacOptions`, which `build.mill`
passes in as `GRIT_PROBE_CLASSPATH` and `GRIT_PROBE_OPTIONS`. It asserts on
`reporter.allErrors`. Every probe suite needs:

- a positive control that must compile, which proves the classpath and prelude are sound;
- a control with the flag off, which proves the flag is what rejects.

A negative test is not adopted until it has been watched failing on a planted breach. For
`SeparationTests`, that breach was `Durable` made a `SharedCapability`.

## Tooling that trips on capture syntax

- **scalafmt** parses `^` only because of `runner.dialectOverride.allowCaptureChecking =
  true` in `.scalafmt.conf`. Without it, each such file fails with *"`identifier` expected
  but `)` found"* and is silently skipped.
- **upickle's `derives ReadWriter` crashes** with a `MatchError` on
  `caps.internal.inferred` in its macro (upickle 4.4.3). Write codecs by hand over `ujson`,
  or with `readwriter[ujson.Value].bimap`, which compiles under both checkers.
- **Metals and the IDE parser** flag `(Tx^)` as *"`identifier` expected"*. It is the
  editor's parser, not the compiler.
- **enola's Scala extractor** silently degrades on capture syntax. Import and module facts
  stay sound; symbol facts do not (`CLAUDE.md`, the architecture gate).
- **Mill prints only the first 10 errors per module.** Turning a flag on across a module
  can report "21 errors found" and show half of them.
- **Scala 3.9's `-Wunused`** reports variables read only inside utest's `assert` as unused.
  `GritTests` silences exactly that message in tests.

## On a Scala upgrade

1. `./mill __.test` from clean. `SeparationTests` failing means separation checking now
   accepts or rejects something different: read the probes before changing them.
2. Re-check each trap above against the new compiler, and delete any that no longer
   applies.
3. If separation checking is no longer experimental, delete `SeparationTests` and the
   `scala3-compiler` test dependency (ADR 0003).
4. Try `derives ReadWriter` again, and the scalafmt override.
5. Consider turning separation checking on in `grit.tui`
   (`.local/backlog/tui-separation-checking.md`).
