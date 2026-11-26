# Between B1: a brief is raised when what it shows is raised; the declare refusal says what to do instead

Between-milestones checkpoint (Nick, 2026-11-18: "lets fix those parked issues"). Planned at 1.5 h. Its items are [the brief that is never raised](artifact:grit/511) and the second bullet of [D2's parked list](artifact:grit/500). The JSON object `Field`, D2's first bullet, stays parked.

## Goal

1. **A brief is raised once what it shows is raised.** Today `ProjectCommands.briefed` keeps the current brief whenever its text and data are unchanged and `believed.dominates(d.integrity)`. So a brief lowered once stays lowered until its text changes. The fix writes a new version whenever this keeper's write would believe the brief strictly higher than the current one. It still writes none when a lower caller's keep of an unrelated artifact would only lower it: the current brief is still justified by what it shows.
   - Sketch, to be confirmed in the plan: let `target` be `believed` met with what this keeper caps a write at. Keep the current brief when `believed.dominates(d.integrity)` and `target` does not strictly dominate `d.integrity`; otherwise write.
   - The design question for the plan is how `briefed` learns the cap. `KeepingCommand.run`'s `most`, `CommandBy`, or a member on `DocumentKeeper` are the options. A turn's `CommandBy.Turn` carries no integrity.
   - Prefer what changes no core signature. If core must change, the plan says why and waits for wisdom.
2. **The declare refusal names a next step.** `MethodCommands.direct`'s refusal gains a line after its sentence: "Ask a person to declare it from the method's file." It's plain words, because model-facing text is user-facing. Its tests are `DeclareTests` and `ProjectsLiveTests`, both of which pin the text.

## Signatures

- `ProjectCommands.briefed`: private. Its doc is changed to say when a brief is raised.
- No public signature changes, unless the plan shows one is needed (see Goal 1).

## Reads

Before the first edit:

- scout show `ProjectCommands.briefed` (body), `ProjectCommands.asCaller`, `DocumentKeeper.believedAtMost`, `KeepingCommand.run` (body).
- scout uses `ProjectCommands.briefed`.
- scout tests `KeptBriefTests`.

At the point of use: `CommandFixtures.askThrough` and `CommandFixtures.askBy`. Test 2 also needs `DeclareTests` and `ProjectsLiveTests`, by the prefix of the declare refusal.

## Commits

1. A test in `KeptBriefTests`, seen red on main: a brief lowered by a lower caller's keep of an artifact it shows is raised again once a caller believed at the meet's identity keeps that artifact unchanged in text. C1 wrote this gap test red and then reverted it; restore it.
2. The fix in `briefed`. Add a test that a lower caller's keep of an artifact the brief does not show writes no brief version, and pin it with `KeptBriefTests`' existing "writes no version" test.
3. The declare refusal's next step, with `DeclareTests` and `ProjectsLiveTests` changed to match.

The plan comes first, with Elision, Capture and separation, Sharing and Retires sections, and is reviewed by wisdom before the first edit. Gate: `scripts/check` bare at the end, then `scripts/check grit.app.main.ProjectsLiveTests`.

## Cited: grit/511 (A brief is not raised when what it shows is raised)

# A brief is not raised when what it shows is raised

Found by [C1](artifact:grit/510) (2026-11-18), whose gap test for it was red on main and reverted.

`ProjectCommands.briefed` keeps a current brief whenever its text is unchanged and the brief is believed no higher than what it shows (`believed.dominates(d.integrity)`). So a brief lowered once stays lowered after what it shows is raised again, until its text changes. This fails safe: it under-believes a brief, never over-believes it.

Comparing for equality instead would write a new brief version on every keep by a lower caller, because the keeper lowers the brief to that caller each time. A fix needs what the capped keeper would write, or the keeper's `Written.Unchanged` when only integrity differs. Design first; weigh whether an under-believed brief ever costs anything (a turn refusing to act on it).

## Cited: grit/500 (Carried from M23: a JSON-object Field; the declare refusal says what to do instead)

# Carried from M23: a JSON-object Field; the declare refusal says what to do instead

From [D2](artifact:grit/497) and J1's parked lists, triaged at M23's close (2026-11-18).

- **A JSON object field.** Core's `Field` has no JSON object field, so `project_declare` takes its method as a JSON string.
- **The declaration's refusal** (`MethodCommands.direct`) says who may declare but not what to do instead, so an agent refused relays a dead end. Proposed: add "Ask a person holding an Authority credential to declare it from the method's file." Wording a person may see echoed: Nick's call; low stakes.

Dropped at triage: offering `project_declare` and `project_methods` to turns. A turn is never believed at `Authority`, so its declare is always refused, and no caller asked for the read.

## Cited: grit/510 (M23 C1: a whole keep keeps a steward's compartments; the parked integrity and fixture gaps; keep answers its version)

# M23 C1: a whole keep keeps a steward's compartments; the parked integrity and fixture gaps; keep answers its version

Checkpoint C1 of [M23's plan](artifact:grit/489), taken at its close (Nick, 2026-11-18). Planned at 1.5 h. Its items are [J2's](artifact:grit/508), [J1's](artifact:grit/505) and [X's](artifact:grit/503) parked lists, as triaged; [D4](artifact:grit/506) built the integrity it fixes.

## Goal

- **A whole keep keeps a steward's compartments.** `ProjectCommands.kept` writes a `Whose.Caller` body at `Integrity.Authority`, which has no compartment, and the keeper meets it with its caller's integrity, so a caller endorsed in compartments is recorded without them. A body that is wholly its caller's words is believed at exactly what the caller is believed at, compartments included.
- **The fixture keeps what a real run keeps.** `CommandFixtures.askThrough` rolls back a refused command (a savepoint) and applies the room's cap as `KeepingCommand.run` does, so a unit test sees what a real run commits.
- **The gaps, each a test seen red first:**
  - a whole keep by a steward caller is believed in its compartments (red on main);
  - a migration's rewrite of a renamed artifact is believed as `Whose.Prior`;
  - a brief written again when the integrity of what it shows is raised (only a lowered one is covered);
  - `project_declare` writing at the room-capped integrity;
  - `project_edit` through a turn believed below `Authority`.
- **`project_keep`'s answer gives the version it kept at**, as `project_edit`'s does.

## Signatures

Plan first: the fix for `kept` is a design choice, and the plan says which with an Elision section, before any edit. The candidates:

1. `KeepingCommand.run` already knows the caller's believed integrity met with the room's cap. Hand it to the command, for example on `CommandBy` or as the keeper's own `most`, and `kept` writes `Whose.Caller` at it.
2. A meet identity the plugin can name without knowing the deployment's compartments (`Integrity.top` takes `Compartments`): a core value or method that leaves the keeper's `most` alone.

A change to a core trait or type (`KeepingCommand`, `DocumentKeeper`, `CommandBy`, `Integrity`) waits for the manager's review of the plan, and Nick's go when the manager says so. `project_keep`'s answer, e.g. `Kept grit/x (note, open). Now at version N.`, is a like-for-like addition to `project_edit`'s wording.

## Reads

On `main` (498b469e), compiled.

- **Before the first edit:** `show ProjectCommands.kept, ProjectCommands.Whose, DocumentKeeper.AtMost` (body `write`), `Integrity.top, Integrity.meet`; `show KeepingCommand.run` (both), `CommandBy`; `uses ProjectCommands.kept`.
- **At the point of use:** `show CommandFixtures.askThrough` (body); `tests ProjectCommandsTests` and `MigrateTests` by the prefixes for integrity; the D4 tests (`tests` on the suite D4 added for "believed").

## Commits

On a new branch `m23-c1-close-fixes` off `main`.

1. The plan, as the coder's report to the manager; no commit until the manager answers.
2. `askThrough` with its savepoint and the room's cap, and the tests that change because of it.
3. The steward's whole keep: its test, red on main, then the fix.
4. The four other gaps' tests.
5. `project_keep`'s answer gives its version.

`scripts/check grit.projects` per commit; bare `scripts/check` before handing back. Record actual hours here when done.

**Actual:** about 0.4 h by the branch's clock (planned 1.5 h): four commits, 2dc6f0ef..d8cab8ff. The brief-raised gap is not built: its premise does not hold on `main` (a brief held lower than what it shows is left as it is, by `briefed`'s design), and it waits for a decision.

## Elision

The choice is candidate 2 with no core change: the meet's identity is `Tx.integrityOf(Principal.Grit)` (`Integrity.top` under the deployment's compartments). Every keeper a command is handed is capped at its caller (the trait member `KeepingCommand.run`'s contract on `own`), so writing the identity keeps exactly the caller's integrity, compartments included.

New, in `object ProjectCommands`:

```scala
/** What a write of words wholly its caller's hands the keeper it goes through: the meet's
  * identity, grit's own integrity. Safe because the keeper a command is handed believes each
  * document at no more than its asker's words (the trait member [[KeepingCommand.run]]'s `own`),
  * so the document is believed at exactly that, compartments included; through a keeper capped
  * at nothing it is grit's own.
  */
private[plugin] def asCaller(using Tx^): Integrity
```

`kept`'s doc, signature unchanged: "Keeps `artifact` through `own` at the label of `before`, its document, or at public when new: the version it is now at. Believed at what `own` believes its caller at, met with what `before` was believed at when `whose` is [[Whose.Prior]] and there is one."

A caller uses `asCaller` from its signature and doc alone; its one failure (an uncapped keeper) is stated. The check turned down a core sentinel needing a "never written" invariant.

## Capture and separation

`asCaller` is a pure read of the `Tx`'s visibility, returning a pure `Integrity`; no capability held or captured, no type parameter, no `caps.unsafe`. No new claim for the compiler, so no probe. No workflow step's name, order or encoding changes (replay unaffected).

## Sharing

- `asCaller` is the one home of "the caller's whole words", over `Tx.integrityOf(Principal.Grit)`: used by `kept` (Caller, and Prior with no `before`), `New.run`'s index, `Migrate.run`'s index rewrite, `MethodCommands.kept`, and `briefed`'s two fallbacks (an empty meet is the identity).
- `project_keep`'s answer and `project_edit`'s share one `AnswerText` function for "Now at version N."
- `askThrough` calls core's `KeepingCommand.run` for the room's cap rather than copying it, and rolls back by `InMemoryDocuments.savepoints`, as `ScriptedKeeping` does.

## Retires

- the six `Integrity.Authority` stand-ins for "leave the keeper's cap alone" in the projects plugin;
- `CommandFixtures.askBelieved`, a near-copy once `askThrough` takes room and believed (a call of it, or deleted);
- `project_edit`'s inline "Now at version" text.
