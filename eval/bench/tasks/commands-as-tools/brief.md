# C3. Commands as turn tools

The projects plugin's commands are offered to a turn as tools, and a command run by a turn is the same command a person runs through an edge, not a copy: same spec, same `run`, the same believed rule. The design below is settled; build it, red first, and commit as `## Commits` says.

## Goal

- **Reads:** a turn reads through `Hosted.reading`/`Bound.Reading`, with `ReadingCommand.run` inside `Reads.read`. A reading command requires nothing.
- **Keeps:** through a new `Bound.Keeping`, a sibling of `Bound.Reading`, whose `Tx^` stays inside the call's step, with a probe that rejects letting it escape. A keeping command requires `Integrity.Outside`, and a turn's keep lands without asking: keeps and moves are reversible through history.
- **Integrity:** what a turn keeps is believed at no more than its call's stamp met with the room's declared cap (ADR 0036). A scripted-model test pins a keep's believed level.
- **Which commands:** the projects plugin's seven reads, `project_new`, `project_keep` and `project_move`; not withdraw, migrate, claim, release or claims (`project_claims` is a keeping command that only reads claims, and is left out with claim and release).
- **Who wrote it:** a turn's keep is written by the person the turn answers, with the turn named: `"<accounts>, through grit (turn <seq> of conversation <id>)"`. A claim still names the person alone.
- **A new ADR, 0053, replaces 0048**, whose title ("with no model and no turn") becomes false.
- Tool descriptions are model-facing and may be repeated to a person: plain words.

## Design

1. **core `store`: `Keeps`, the write sibling of `Reads`.** `Jot.as(subject): Keeps^`, as `Db.as`, and `Jot.within(jot, bound): Jot^`, as `Db.within`: a write runs its body only when what its transaction knows is classified at most `bound`. `Keeps.write` takes a body answering a `Keeps.Verdict` (`Commit(kept)` or `Discard(undone)`): committed on `Commit`, rolled back on `Discard` with its value returned. No `Jot` implementation changes.
2. **core `tool`: `Hosted.keeping` and `Bound.Keeping`.** `Hosted.keeping(spec, shown, requires, output)(run: (A, CallSlot, Integrity) => (Tx^) ?=> Either[StoreError, Outcome])`: told the call and the integrity it was stamped at, run in the transaction its turn opens for the call. It takes no gate: it never asks first. `Toolbox.bind` hands the stamp it checked (`Trusted.check`'s integrity) to the tool's bind, so a turn cannot pass the wrong one; an edge's bind (`Toolbox.requested`) binds at `Untrusted`. An edge answers a keeping call that it is not run there (`Run.request`, as for `Bound.Reading`).
3. **turn: a keeping call settles in one transaction with its result.** `Rounding.Binding` makes the round's keeps as it makes its reads: `routed.keeps(tooling.jot).as(Subject.Turn(turn))`, with `Turn.Bounded.keeps(jot)` beside `Bounded.reads(db)`. `Settling.keepFrom`: a result already kept is returned and nothing runs; else, in one `Keeps.write`, the run and, on `Done`, its result entry, committed together. Any other outcome is rolled back and kept as the result alone, by the existing settle path; a store failure there is answered `Failed`, saying nothing was kept. A keeping call's kind is `Free`: no new step, name or encoding, so replay is unaffected (ADR 0004; `TurnReplayTests`).
4. **core `plugin`: one command, two askers.** `CommandBy` (`Direct(person)` through an edge, `Turn(person, turn)` through a turn answering them) replaces `by: Principal.Person` in both `run`s. `KeepingCommand.run` in the object is the believed rule written once: the keeper believes each document at no more than the integrity given, met with the room's cap; `SqlPluginCommands` and the turn's tool both call it. `Commanded` (`Reading(plugin, command)`, `Keeping(plugin, terms, command)`) and `Commanded.of(plugins)` replace `SqlPluginCommands`' private `Runnable`, so the edge and the kit enumerate commands one way. `PluginCommands.offered` stays, as an edge's MCP tool list calls it. `PluginCommand.offeredToTurns` (false unless a command says) and `PluginCommand.shown(args)` (its transcript line, `""` unless it says) are new.
5. **dbos:** `Stores.claims: PluginName -> Claims` (`SqlClaims`), so the kit reaches a plugin's claims as it reaches `reads` and `keeper`.
6. **core `plugin`: `CommandTools.of(commanded, reads, keeper, claims, askers, clock): Tool.Offered^{clock}`**, a command as a turn's tool. It runs for the person the turn answers (`Askers`, `CommandBy.Turn`), dated by `clock`, and answers `Failed`, running nothing, when the turn answers no person (`CommandTools.NoPerson`). A reading command runs inside `reads.read`; its `Done` is believed at the least integrity of the documents it read (untrusted for any read of its plugin's cache), as grit's own words when it read none. That least is tracked through a decorated shelf and cache writing a local `var` of the one call, through a function made pure by `caps.unsafe.unsafeAssumePure`, its proof beside it (rule 11): the local is held only by the pure command while it runs and read once after. A keeping command is `Hosted.keeping`, requiring `Outside`, and runs `KeepingCommand.run` with its call's stamp and the room the turn's transaction is opened in; a turn in no room keeps nothing (`CommandTools.NoRoom`), and a refusal is `Failed` with its reason, rolled back.
7. **kit:** `PluginBinding.commands(plugins, reads, keeper, claims, askers, clock)` gives every command a turn is offered as a tool, through `Commanded.of` and `CommandTools.of`; `Launch.asking` offers them after the plugins' tools, in the plugins' order, its toolbox's capture set gaining `clock`.
8. **projects:** the ten commands above set `offeredToTurns` and `shown`. A turn's writer as Goal says. The head-showing helper is renamed `headed`, as `shown` is now a command's. Every description names only what a turn is offered (`project_stale` no longer points at `project_withdraw`; its list is candidates to end), and no description ends with the line telling a project artifact from a claude.ai Artifact: unwrap `said` at its call sites and delete it.
9. **ADR 0053** carries over 0048's standing decisions (a person's command reads under `Subject.Person` and cannot choose whom it reads for; a reading command's transaction is read-only; names unique across commands and tools; no audit row) and adds the turn's: run for the person the turn answers, reads bounded by its route and believed at the least read, a keep requiring `Outside`, never asking, in one transaction with its result, believed at no more than its call's stamp met with the room's cap. 0048 is deleted in the same commit and every reference retargeted (ADRs 0027, 0049, 0051; the projects README; the Scaladoc that cites it).

## Reads

Before the first edit (compile first):

- `show {"symbols":["PluginCommand","PluginCommands","Hosted","Bound","ReadingCommand","KeepingCommand"],"docs":true}`
- `uses {"symbols":["PluginCommands.offered","Hosted.reading","Bound.Reading","SqlPluginCommands"]}`
- `area {"names":["projects"]}`, then `show` the commands this touches
- `docs/decisions/0048-*.md`, `0036-*.md`, `docs/decisions/README.md`

At the point of use: the existing tests of `Bound.Reading` and of the projects commands, by name prefix; `docs/capture-checking.md` for the probe.

## Commits

Each behaviour red first, the red quoted in its commit message (a plant's diff with it).

1. **turn: a keeping tool runs in the one transaction that keeps its result** (design 1–3). Reds: `TurnToolsTests` (a crash before a keeping call's result is kept keeps nothing, so its rerun keeps afresh; a call not answering done keeps nothing it wrote; its kind is `Free`), `ToolboxTests` (a keeping call is told the integrity it was stamped at), `JotKeepsTests`, `TurnRouteTests` (a keep is bounded by the route), `RunTests` (an edge does not run it), and the probe `KeepingToolCaptureTests` in `grit.core.probes` (a run keeping its `Tx` in a `var` is rejected; plant `(Tx)` for `(Tx^)`).
2. **plugin: a command runs for whoever asked, through an edge or a turn, believed by one rule** (design 4–5). Red: `ProjectCommandsTests` "a keep through a turn names the person and the turn as its writer".
3. **plugin: a plugin's commands that say so are offered to a turn as tools** (design 6–8 but the descriptions). Reds: `TurnGateTurnTests` "a turn fallen to outside keeps what it calls believed at outside, not at its trigger's level" (scripted model); a new `CommandToolsTests`; `PluginToolsTests` (a turn is offered each plugin's commands that say so); `ProjectCommandsTests` (a turn is offered the reads, new, keep and move).
4. **docs: ADR 0053** (design 9).
5. **projects: a command's description names only what a turn is offered** (design 8's descriptions).

Every commit compiles with no warnings. End with a short report: the commit hashes and each red.
