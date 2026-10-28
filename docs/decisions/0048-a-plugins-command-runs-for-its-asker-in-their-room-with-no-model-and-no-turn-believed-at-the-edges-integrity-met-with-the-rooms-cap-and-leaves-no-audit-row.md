# 0048. A plugin's command runs for its asker in their room, with no model and no turn, believed at the edge's integrity met with the room's cap, and leaves no audit row

Status: accepted (2026-10-27)

Context: some plugins keep records a person changes directly: a project's artifacts, a
ledger's entries. Until now a plugin changed its documents only by posting, its jobs and its
tools, each run by grit for a closed period, a schedule or a model's call. A person who wants
one record changed would have to ask a model to call a tool, paying for a turn and trusting the
model to pass what they typed. Turned down:

- A tool run by an edge without a turn: a tool is offered to a model, and its result is
  model-facing text; a command's answer is the person's.
- A command reading at grit's clearance or the room's alone: it would show a person what
  the room may receive but they may not read.
- An audit row per command, as an administration command keeps (ADR 0033): a command's
  writes are the plugin's own documents, whose versions already record what was kept and when,
  and a plugin's data names who asked when its records need it. Agents acting as principals
  revisit this.

Decision:

- **A plugin may contribute commands** (`Plugin.commands`), each a `PluginCommand` that names
  itself and reads its arguments by a `ToolSpec`, as a tool does. An edge runs one by name for
  the person behind an account, through `PluginCommands` (in `EdgeStores`), with no model and
  no turn, and answers at once (`CommandOutcome.Done` or `CommandOutcome.Refused`), to that person alone.
  It runs in the edge's process over the store (ADR 0002).
- **It reads for its asker in their room** (`Subject.Person`): everything up to the label the
  room is written at outside grit met with the asker's clearance. The asker is a person, never
  grit, and is resolved from the account as the transaction opens; an account no person is
  linked to runs nothing.
- **It cannot choose whom it reads for.** A command is pure and is handed the open transaction:
  it holds no `Db`, `Reads` or `Jot` through which to open one under a subject of its own, and
  cannot keep the one it is handed.
- **A reading command is handed its plugin's reads in a read-only transaction**; a write
  through it is refused by the database. **A keeping command is handed its plugin's keeper**,
  and what it keeps is believed at no more than the integrity the edge gives what the account
  says there, met with the room's declared cap, as an inbound message is (ADR 0036). It is
  committed only when it answers `Done`; a refusal or a failed store call keeps nothing.
- **Names are unique across a deployment's commands and tools**, and a keeping command's plugin
  keeps documents: `Deployment.of` refuses either breach (`CommandRepeated`,
  `CommandUnshelved`).
- **A command leaves no audit row.**

Consequences:

- A person changes a plugin's records in one transaction at no model cost, and a plugin's
  records can be kept only through checks it runs itself.
- An edge offers commands as tools are offered (`PluginCommands.offered`), so one schema
  reader serves both.
- Who asked is recorded only where a plugin's data says so.
- Enforced by the compiler (`Subject.Person` takes a person, never grit), probes in `PluginCaptureTests`
  (a command holding a `Db`, a command keeping its `Tx`), `Deployment.of`, and
  `PluginLiveTests`.
