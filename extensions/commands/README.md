# grit.commands

Implements `ServedEdge`: the edge a deployment mounts to serve an outside coding harness over
HTTP ([ADR 0062](../../docs/decisions/0062-an-outside-harness-reaches-a-deployment-over-http-through-an-edge-that-writes-no-files-each-request-from-a-caller-and-each-worker-named-by-its-credential.md)).
A shipped edge extension (ADR 0021): it names core, `grit.edge` and `grit.mcp`'s wire alone
(`commands-names-core-edge-and-mcp-wire`), and no harness, no plugin and no MCP client. What a
deployment's own commands and waits need of a plugin is handed to it as values.

In dependency order, each package importing only those before it:

- **`worker`**: a worker's name, and where its work is placed: a directory on a worker is the
  place `worker:<name>/<dir>`; and the claim holder a session there, or an agent it starts,
  works as (`Holders`), readable in a claims listing and the same whoever computes it.
- **`caller`**: who a request is from (`Caller`: an account, the integrity its words are
  believed at, its worker), and the deployment's rule for it (`Callers`): today a request with no
  credential is the owner's, and one with a credential is refused. Routes ask `Callers` alone, so
  credentials per worker change this package and no route.
- **`declare`**: what a deployment hands the edge as values, pure: its own commands
  (`OwnCommand`), each guarding a plugin's command or added beside them, and lent the request's
  way to run a plugin's command (`Ran`) for that request alone; and the wait on a plugin's
  document `/wait` reads (`HoldingWatch`), so the edge names no plugin; and each harness's
  adapter (`Adapter`), whose join script `/join/<harness>` serves, so the edge names no harness.
- **`events`**: a harness's event lines, private to the module: one JSON object a line in
  grit's words (ADR 0050), read into one batch of one session's events, each line refused named
  by its number and why. A harness's adapter maps its hooks onto them; nothing here names one.
- **`routes`**: the routes, private to the module, and the one table that turns what a command
  came to into a status (`RouteReply`), so the routes cannot disagree. On every route a query
  parameter it does not read is 400, named (`/command`, `/mcp`, `/wait` and `/join` read none), and
  so is a key of `/wait`'s body; a command's arguments are its plugin's to refuse, and an event
  line's unread key refuses that line alone. A request `Hosts` does not
  admit, or `Callers` finds no caller for, is refused before any route; no GET runs a command.
- **`edge`**: `CommandsEdge`, the `ServedEdge` a deployment mounts: `of` refuses what cannot be
  served safely (two own commands of one name, a caller outside its realm, no credential beyond
  loopback), and `open` refuses own commands that do not match the plugins' offered ones and an
  offered command `/mcp` cannot list, vouches each caller's account, and listens.

No source file sits at the module's root. The test tree mirrors it.

## What the code cannot say

- **Serve writes no files.** Everything a harness is told it is told in an answer.
- **A worker is named by configuration, never by a host.** No hostname is read or stored.
