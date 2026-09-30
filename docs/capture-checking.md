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
  [ADR 0003](decisions/0003-durable-is-exclusive-under-separation-checking.md). Turning it
  on there raises the traps below that `grit.tui` has not been rewritten around: `var`
  fields in plain classes, arrays handed to Java, and a reach capability leaking through a
  `foreach`.

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

- *Symptom:* `Mutable variable n is defined in a class that does not extend Stateful or
  Mutable. The variable needs to be annotated with untrackedCaptures to allow this.`
- *Cause:* separation checking treats it as mutable state aliased through the object.
  Visibility does not matter: `private var` and `private[this] var` fail the same way.
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

**An overloaded Java varargs method crashes the compiler.**

- *Symptom:* `java.lang.AssertionError: assertion failed: unexpected type of
  container.withCommand: <overloaded container.withCommand>` from `Recheck.recheckApply`,
  a compiler crash rather than an error.
- *Cause:* capture checking's recheck cannot handle the overload set of a Java method
  with a varargs alternative (Testcontainers' `withCommand(String...)`).
- *Fix:* call a non-varargs overload (`withCommand(String)` in `TestPostgres`).

**A generic by-name parameter handed on to another by-name parameter crashes the compiler.**

- *Symptom:* `java.lang.AssertionError: assertion failed` from `CaptureOps.boxDeeply`
  under `cc.Setup`, naming no file.
- *Cause:* `def patient[R <: SlackApiTextResponse](body: => R) = call(body)`, where `call`
  takes `body: => R` too.
- *Fix:* take a thunk, `body: () => R`, and call `call(body())` (`SocketSlack.patient`).

**A capability cannot be a field of an object.**

- *Symptom:* `lazy value engine needs an explicit type because it captures a root
  capability`, then, once typed, `object SqlLiveTests needs to extend Capability since it
  has a field engine with any in its type`.
- *Cause:* an object holding an `Engine^` (a `SharedCapability`) would itself be a
  capability.
- *Fix:* open it where it is used and close it in a `finally`; keep plain values, such as
  a `DbConfig`, in the fields.

**Iterator-producing combinators.** `args.sliding(2).collectFirst { … }` is rejected with
`Illegal capture reference`. Indexed `Vector` code is the way out.

**A `Vector` field of an object, left to inference.**

- *Symptom:* `value Pairs needs an explicit type because it captures a root capability in
  its type Vector[(String, String)^{}]{val prefix1: Array[Object^{}]^}`.
- *Cause:* the inferred type is `Vector`'s refinement, whose internal `prefix1` is an array,
  which capture checking treats as `Mutable`, so the field would hold a root capability.
- *Fix:* write the type out: `val Pairs: Vector[(String, String)] = Vector(…)`
  (`grit.models.ToolStreamProbe`). The same holds for any `Vector` value of an object,
  such as `val Keys: Vector[String] = …`.

**A case class with a capability field, passed around as a value.**

- *Symptom:* `Found: TurnTooling^{ws}  Required: TurnTooling{val workspace:
  Workspace^'s1; val tools: Toolbox[CapSet^'s2]^'s3}^'s4`, *"capability `any` cannot flow
  into capture set {any?}"* (`TurnTooling` a case class with those fields), where a value
  built elsewhere (a `def`'s result, or a default argument, whose type drops the captures
  entirely) is passed on.
- *Cause:* a field typed `Workspace^` gets a capture set of its own in each value's
  refined type, and the declared type of the value that carries it has lost it.
- *Fix:* construct the value inline where it is passed; pass its parts, not the value. Or
  pass it as a supertype none of whose members holds a capability. Or tie the capabilities
  to a capture-set parameter the value's type names: `TurnTooling[C^]` holds a
  `Toolbox[C]`, and `Main` builds a `TurnTooling[{tuned, models, store}]` inline and hands
  it to `Turn.body[C^]`.

**An abstract capability member, implemented by a case-class field.**

- *Symptom:* `error overriding method jot in trait TurnTooling of type -> Jot^{fresh}; value
  jot of type Jot^ has incompatible type`.
- *Cause:* a `def jot: Jot^` result is a fresh root capability, which no field's `^`
  matches.
- *Fix:* leave the member off the trait and read it from the case the code has matched; or
  make the holder a case class whose field it is (`TurnTooling`'s `jot`).

**A tupled lambda over a nested `Vector`.** Under separation checking,
`turns.zipWithIndex.flatMap((turn, t) => …)` over a `Vector[Vector[A]]` is rejected:
*"capability `any` cannot flow into capture set {} of value turn"*, with the inferred
parameter type showing `Vector`'s internal `prefix1: Array[Object^…]`. Writing the parameter
types out fixes it: `(turn: Vector[A], t: Int) => …` (`grit.assembly.eval.Eval.load`).

**A thunk in a data type launders capabilities.** This is why `grit.tui`'s `Effect` is
plain data with no function cases (`grit/tui/CLAUDE.md`, rule 3).

**`this` in a trait is `^{any}`.**

- *Symptom:* passing `this` from a trait's method to something that takes a pure value
  fails with `Found: (T.this : …^{any})`.
- *Cause:* an unannotated trait's self may capture anything, and a method body's references
  fold into it.
- *Fix:* a pure self type, `trait T { self: T^{} => … }`. It also rejects a capturing
  implementation where it is defined, which is what `grit.tui.runtime.app.App` relies on.

**A global capability is checked where it is used.**

- *Symptom:* `[E223] … not included in the allowed capture set {} … declare … with a uses
  clause`, on an object or class under a pure self type.
- *Cause:* referring to a top-level capability captures it.
- *Fix:* pass it in instead. A `uses` clause goes in the template header; written in the
  body it parses as an identifier.

**An enum or case class nested in a class captures `this`.**

- *Symptom:* a lambda that names one of its cases is typed `…^{C.this}` (it was
  `Mailbox[M]^{NodeRuntime.this}` in the view-tree spike). A codec over a case class nested
  in an abstract test base is rejected where a pure function is wanted: `Found: (v:
  ujson.Value) ->{DurableContract.this} …  Required: ujson.Value -> …`.
- *Cause:* a nested enum or case class is a path-dependent type.
- *Fix:* move it, and anything built over it, to the companion object
  (`DurableContract.Picked`).

**A context function that takes a capability, stored in a field.**

- *Symptom:* `capability body* cannot flow into capture set {any}`, putting a `WorkflowId =>
  Durable^ ?=> String` into a map field.
- *Cause:* the reach capability of the inner context function stays in the type.
  `caps.unsafe.unsafeAssumePure` strips only the outer capture set, so it does not help.
- *Fix:* cast to the pure type, `body.asInstanceOf[WorkflowId -> Durable^ ?-> String]`, with
  the proof beside it that nothing it captures outlives the call that handed it in
  (`DbosRuntime.run`: called only while `run` waits, removed in a `finally`).

**A `=>` field makes its holder a capability.**

- *Symptom:* a long cascade ending in *"needs to extend Capability"* that does not point at
  the field.
- *Cause:* `A => B` is `(A -> B)^{any}`, so a case class holding one captures anything.
- *Fix:* declare handler and callback fields `->`.

**A message can carry a capability past a pure app.**

- *Symptom:* none; it compiles. A `Msg` case holding a `T^`, delivered by a host, hands
  the capability to a pure `update`.
- *Fix:* declare the message type pure (`enum Msg extends caps.Pure`), and bound it where
  it is consumed (`App[S, M <: caps.Pure]`). A capability-typed case is then an error where
  it is declared.

**`^` in a `def` result is fresh per call.** An abstract `def cap: Cap^` cannot be
implemented by a `val cap: Cap^`.

**Mutable collections are pure types.** A typed `ArrayBuffer` field passes, even under
separation checking. Nothing catches it; keep mutation in scoped locals (STYLE rule 7).

**A tuple match type inside an `inline` def.**

- *Symptom:* `a match type could not be fully reduced: trying to reduce Tuple.Fold[(("a" :
  String), …)^'s1, …] failed since selector … does not match case h *: t`, from
  `constValueTuple[N].toList` at the inline's call site.
- *Cause:* the tuple is given a capture-set variable (`^'s1`), and the match type will not
  reduce over it.
- *Fix:* skip the match type: `constValueTuple[N].productIterator.toVector.map(_.toString)`
  (`grit.core.tool.Args.of`).

**A `Vector` inside a named tuple, read in a lambda.** The nested-`Vector` trap again, from
the caller's side: `args.read(json).map(_.edits.size)`, where `edits: Vector[…]`, is rejected
(*"capability `any²` cannot flow into capture set {any}"*, or a separation failure that
*"hides non-local value x$proxy"*), and the lambda's parameter type is too long to write out.
`Field.each` reads a `List`, which has no array inside, so the lambda compiles.

**A class that holds a collection of capturing values.**

- *Symptom:* `Local reach capability Toolbox.this.tools* leaks into capture scope of class
  Toolbox. You could try to abstract the capabilities referred to by Toolbox.this.tools* in
  a capset variable.`, on every method that reads the field.
- *Cause:* `tools: Vector[Tool[?]^]` gives each element a capture set of its own, reachable
  only through the field (`tools*`), and a method may not let it out.
- *Fix:* a capture-set parameter on the class: `final class Toolbox[C^] private (tools:
  Vector[Tool.Offered^{C}])`, built by `def of[C^](tools: Tool.Offered^{C}*)`. The type is
  then written `Toolbox[{ws}]`, not `Toolbox^{ws}`.

**A wildcard over a class with a `=>` field.**

- *Symptom:* `Found: Tool[String]^{ws}  Required: Tool[?]{val run: (Tool[?]^'s1)#A^'s2 ->'s3
  Outcome^'s4}^'s5`, passing a tool where a `Tool[?]` is wanted.
- *Cause:* the wildcard is refined by the captured field, whose type mentions the unknown
  `A`, and the concrete type does not conform to that refinement.
- *Fix:* a non-generic sealed supertrait that says what the holder needs (`Tool.Offered`:
  name, schema, `bind`), and hold that instead of `Tool[?]`.

**A probe's flag-off control cannot parse capture syntax.** With capture checking off,
`Workspace^` and `Toolbox[{ws}]` do not parse, so a control for a probe written in them
compiles the same source with its capture sets erased (`ToolCaptureTests.erased`).

**An `Args.of` written inside another.** `Field.each("…", Args.of((oldText = …, newText =
…)))` inline in an outer `Args.of` fails with *"a match type could not be fully reduced:
trying to reduce `Args.Values[(Field[String]^'s3, Field[String]^'s4)^'s5]`"*: the inner tuple
gets a capture-set variable, the trap above. Bind the inner `Args` to a `val` first
(`grit.tools.Coding.replacement`).

**A helper whose result type names its own parameter's capability.** In a test,
`def bound(book: Book, …) = Toolbox.of[{book}](…).bind(…)` crashed the compiler (3.9):
*"assertion failed: orphan parameter reference: TermParamRef(book)"*, an internal error, not
a diagnostic. The inferred result type mentions `{book}`, a parameter of the method it
escapes. Build the value where the capability is a local `val` instead (`TuningTests`), or
give the helper an explicit result type that does not name the parameter.

**A generic call of `Toolbox.bind` from another file of `grit.core`.**

- *Symptom:* a clean compile of `grit.core` fails inside `Toolbox.bind` itself: *"Reference
  `C` is not included in the allowed capture set 's1 of the enclosing method bind in class
  Toolbox"*. An incremental compile of the same sources passes, so a green test run proves
  nothing; `./mill clean grit.core.compile` shows it.
- *Cause:* not known; it appeared when `grit.core.edge` called `tools.bind` from a method
  generic in `C^`, compiled in the same run as `Toolbox`.
- *Fix:* call `bind` from another module: running a request is `grit.edge.Run.request`.

**`-Wunused` on a default method.** A parameter a default `def` ignores warns where the
`_` of a lambda never did. Mark it `@unused`.

## Testing that something does not compile

utest's `assertCompileError` (`scala.compiletime.testing`) never reports capture- or
separation-checking errors. Capture checking runs after the typer, and a probe that should
fail compiles "cleanly" there.

The way that works is `grit.core.durable.SeparationTests`. It runs `dotty.tools.dotc.Driver` in the
test, against core's run classpath with core's own `scalacOptions`, which `build.mill`
passes in as `GRIT_PROBE_CLASSPATH` and `GRIT_PROBE_OPTIONS`. It asserts on
`reporter.allErrors`. Every probe suite needs:

- a positive control that must compile, which proves the classpath and prelude are sound;
- a control with the flag off, which proves the flag is what rejects.

A negative test is not adopted until it has been watched failing on a planted breach. For
`SeparationTests`, that breach was `Durable` made a `SharedCapability`.

Put one breach in each probe: a typer error in the same source stops compilation before
capture checking runs, and masks it. `grit.tui`'s `AppCaptureTests` is the second suite on
this pattern.

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
  `-Wunused` is on only in `scripts/check`'s lint tier, where `GritTests` silences exactly that
  message in tests.
- **`-Werror` turns a probe's warning into an error.** The `Driver` probes compile with
  `probeOptions`, the module's flags less `-Werror`, so a probe sees only the checkers' errors.

## On a Scala upgrade

1. `./mill __.test` from clean, then `scripts/it`. `SeparationTests` failing means separation checking now
   accepts or rejects something different: read the probes before changing them.
2. Re-check each trap above against the new compiler, and delete any that no longer
   applies.
3. If separation checking is no longer experimental, delete `SeparationTests` and the
   `scala3-compiler` test dependency (ADR 0003).
4. Try `derives ReadWriter` again, and the scalafmt override.
5. Consider turning separation checking on in `grit.tui`, and run `scripts/tui-gate`
   after, since the terminal seam is what would change.
