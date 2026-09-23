# grit — Programming Style

> This is not hygiene. It is load-bearing architecture.

grit's premise is that context can be **elided without loss** — that a groomer can drop
detail and the model still gets it right. Elision is only sound if the signature is the
whole truth about a function. The moment effects hide inside bodies, you can no longer
safely show a caller anything less than full source, and the context bloat grit exists to
eliminate comes straight back.

So the rules below are not about taste. Every one of them exists to keep this true:

> **A signature without a capability is a promise of purity.**

Capture checking (`-language:experimental.captureChecking`, on project-wide) is what
makes that a compiler-enforced promise rather than a convention.

---

## 1. Referentially transparent by default

Same inputs, same output, no observable effect. A function that cannot be takes a
capability instead — and then says so in its type.

```scala
// no
def nextEntryId(): EntryId = EntryId(UUID.randomUUID().toString)

// yes — the caller supplies the entropy, so the function is a function
def entryId(seed: UUID): EntryId = EntryId(seed.toString)
```

## 2. Effects are capabilities in the signature

`(using Tx)`, an explicit `Provider`, an explicit `Clock`. No ambient singletons, no
hidden I/O, no thread-local reads outside a quarantine module (rule 8).

```scala
// no — reaches into a global, does I/O, signature says none of it
def loadBranch(s: SessionId): Vector[Entry]

// yes
def loadBranch(s: SessionId)(using Tx): Vector[Entry]
```

If you find yourself wanting a global to avoid threading a parameter, that is the rule
working. Thread the parameter.

## 3. Expected failure lives in the return type

`Either` or a sealed ADT for anything the domain anticipates. Exceptions only for the
genuinely unrecoverable.

This has teeth beyond style here: **DBOS retries a step when it throws.** An exception
you did not design becomes retry behaviour you did not design — potentially a duplicated
model call. Domain failures must not be exceptions.

```scala
// no
def parseModel(s: String): Model = throw new IllegalArgumentException(s)

// yes
def parseModel(s: String): Either[BadModel, Model]
```

The replay half of the same story: **a step's return value is persisted and replayed
from a row**, so it has to be a value Jackson can reconstruct. `Unit` is not one. A step
that has nothing to say should say what it did — `"inserted:e1"`, an enum, a count —
rather than returning `Unit` and forcing the boundary to invent `null`. `grit.dbos`
enforces this with `StepResult` evidence; a `Unit` step body is a compile error.

## 4. Total over partial

No `.get`, no `.head`, no `Map.apply`, no inexhaustive matches. Return `Option`.

```scala
// no
val latest = entries.last

// yes
val latest: Option[Entry] = entries.lastOption
```

## 5. Make illegal states unrepresentable

Sealed ADTs over stringly-typed data. Opaque types for identifiers so they cannot be
transposed at a call site.

```scala
opaque type SessionId = String
opaque type EntryId   = String

// append(entryId, sessionId) now fails to compile instead of corrupting a row
```

Prefer no upper bound. `opaque type EntryId <: String` also stops transposition, but it
lets an id widen back into a bare `String` implicitly — which is how ids leak into string
concatenation and back out as the wrong type. Make the crossing explicit
(`EntryId.value(id)`) so it is greppable.

## 6. No `null` in Scala

`null` is confined to the quarantine modules (rule 8 — `grit.dbos` today) and converted
to `Option` the moment it crosses the boundary. DBOS's Jackson layer wants `null` for
`Unit`; that dance happens in one place and is never visible outside it.

This includes nulls returned by Java methods that look total. `Throwable.getMessage` is
the one that has already bitten here: `StoreError.DatabaseError(e.getMessage)` types as
`String` and carries `null` into a pure ADT. Wrap at the call site —
`Option(e.getMessage).getOrElse(e.toString)`. Anywhere a value crosses out of a
quarantine module, assume the Java signature is lying about nullability.

## 7. Immutable data

`case class` and `val`. Mutation only inside an explicitly scoped local, never on a field
that outlives a method.

One carve-out: a **test double standing in for a mutable resource** may hold mutable
state, because that state is the thing it exists to emulate. `FakeEntryStore` has to
behave like a table for the trait's contract to be under test at all. The carve-out is
for fakes of stateful seams — not for convenience elsewhere in tests.

## 8. Java libraries are quarantined by role

Each Java-facing library lives in the one module whose job needs it — a *quarantine
module* — and that module translates Java's conventions (`null`, thrown exceptions,
mutable handles, reflection) into grit's before anything leaves it. Quarantine modules
depend only on `grit.core` and meet only in `grit.app`, so no Java library can leak into
another mechanism through a module dependency.
[ADR 0001](docs/decisions/0001-quarantine-by-role.md).

For DBOS the module is `grit.dbos`. Everything ugly — DBOS reflection over
`Function0.apply`, `JdbcStepFactory` wiring, Jackson boxing, `Unit → null`, raw
`java.sql.Connection` — lives there.

**Nothing outside `grit.dbos` imports `dev.dbos.*` or `java.sql.*`.**
(Opaque type definitions that *alias* a quarantine type — e.g. `opaque type Tx =
java.sql.Connection` — may name it fully-qualified without an `import`; that is a type
definition, not an I/O call site.)

This is the rule most likely to be inconvenient, and the one most worth holding. It is
also where capture checking will fight hardest; a per-file escape inside a quarantine
module is acceptable, everywhere else is not.

## 9. Prefer explicit capability parameters over clever inference

Capture checking is experimental. A readable error beats a minimal annotation. Write the
capability out.

## 10. The elision test

The rule the other nine serve. For any public function, ask:

> *If the groomer dropped the body and kept only the signature and its doc, could a
> competent agent still call it correctly?*

If not, the signature is wrong — fix the signature, not the doc.

Corollary: **scaladoc says _what_, never _how_.** A doc comment that narrates the body is
duplicated context that will be wrong after the next edit. A doc comment that states the
contract survives.

```scala
// no — restates the body, tells a caller nothing new
/** Loops over entries, filters by session, sorts by seq, maps to messages. */

// yes — states the contract, lets the body be dropped
/** Every message on the session's active branch, oldest first.
  * Empty if the session has no entries. */
```

---

## Why rule 10 is a feature, not an aspiration

Once LSP/BSP integration lands, signatures can be stored as symbols independently of
their bodies, and the assembler can serve **signature-only views** of the codebase. That
retrieval strategy is only correct if the signatures are trustworthy.

In other words: grit is a context harness, so grit's own source should be the most
elidable code in the repository. We dogfood the thesis.
