# 0053. A plugin's command is one command whether a person gives it through an edge or a turn calls it, run for the person asking, believed at what the edge or turn gives met with the room's cap, and leaves no audit row

Status: accepted (2026-10-30)

Context: some plugins keep records a person changes directly: a project's artifacts, a
ledger's entries. A person changes one through an edge with no model and no turn; a person
talking to grit wants the turn answering them to read and change the same records. Turned
down:

- A turn running a command through the edge's path (`PluginCommands.run`): it opens its own
  transaction for a person from an account, where a turn reads as its route bounds it and
  writes from inside its step.
- A copy of a command's logic as a tool: two writers kept alike by hand.
- A command reading at grit's clearance or the room's alone: it would show a person what the
  room may receive but they may not read.
- An audit row per command, as an administration command keeps (ADR 0033): a command's writes
  are the plugin's own documents, whose versions already record what was kept and when, and a
  plugin's data names who asked when its records need it.

Decision:

- **A plugin may contribute commands** (`Plugin.commands`), each a `PluginCommand` that names
  itself and reads its arguments by a `ToolSpec`. A command runs for whoever asked
  (`CommandBy`): a person through an edge (`Direct`), or a person through the turn answering
  them (`Turn`, the turn named beside them). It cannot choose whom it reads for: it is pure,
  handed the open transaction, and holds no `Db`, `Reads` or `Jot`.
- **Through an edge** (`PluginCommands`, in `EdgeStores`, in the edge's process over the store,
  ADR 0002) it runs by name in one transaction for the person behind an account, in their
  room (`Subject.Person`); an account no person is linked to runs nothing. Its answer
  (`CommandOutcome`) is that person's alone.
- **Through a turn** a command its plugin offers turns (`offeredToTurns`) is a tool under the
  same spec (`CommandTools`), run for the person the turn answers; a turn answering no person
  (a job's run, grit's own) runs nothing. A reading command reads in the turn's reads, bounded
  as the turn was routed, and its answer is believed at the least integrity of what it read.
  A keeping command requires `Outside`, never asks first, and runs in the one transaction that
  also keeps the call's result, committed only on `Done`: a crash before the commit keeps
  nothing, so a rerun runs it afresh rather than answering a refusal for what happened.
- **A reading command's transaction is read-only. A keeping command is handed its plugin's
  keeper and claims**, and what it keeps is believed at no more than what the edge gives the
  account's words there, or the call's stamp, met with the room's declared cap
  (`KeepingCommand.run`, ADR 0036). It is kept at the label the command gives joined with its
  transaction's floor: a turn's keep is kept where a person's would be, and refused once that
  floor rises past the turn's route, as the turn's reply and its pieces are (ADR 0030).
- **Names are unique across a deployment's commands and tools**, and a keeping command's plugin
  keeps documents: `Deployment.of` refuses either breach (`CommandRepeated`,
  `CommandUnshelved`).
- **A command leaves no audit row.**

Consequences:

- A person changes a plugin's records in one transaction at no model cost, or asks a turn to;
  either way through checks the plugin runs itself.
- An edge offers commands as tools are offered (`PluginCommands.offered`), so one schema reader
  serves both.
- Who asked is recorded only where a plugin's data says so; a turn's write names the turn too.
- Enforced by the compiler (`Subject.Person` takes a person, never grit; `Hosted.keeping`
  takes no gate), probes in `PluginCaptureTests` and `KeepingToolCaptureTests`, `Deployment.of`,
  and `PluginLiveTests`.
