# grit.commands

Implements `ServedEdge`: the edge a deployment mounts to serve an outside coding harness over
HTTP ([ADR 0062](../../docs/decisions/0062-an-outside-harness-reaches-a-deployment-over-http-through-an-edge-that-writes-no-files-each-request-from-a-caller-and-each-worker-named-by-its-credential.md)).
A shipped edge extension (ADR 0021): it names core, `grit.edge` and `grit.mcp`'s wire alone
(`commands-names-core-edge-and-mcp-wire`), and no harness, no plugin and no MCP client. What a
deployment's own commands and waits need of a plugin is handed to it as values.

In dependency order, each package importing only those before it:

- **`worker`**: a worker's name, and where its work is placed: a directory on a worker is the
  place `worker:<name>/<dir>`.
- **`caller`**: who a request is from (`Caller`: an account, the integrity its words are
  believed at, its worker), and the deployment's rule for it (`Callers`): today a request with no
  credential is the owner's, and one with a credential is refused. Routes ask `Callers` alone, so
  credentials per worker change this package and no route.
- **`declare`**: what a deployment hands the edge as values, pure: its own commands
  (`OwnCommand`), each guarding a plugin's command or added beside them, and lent the request's
  way to run a plugin's command (`Ran`) for that request alone.
- **`routes`**: the routes, private to the module, and the one table that turns what a command
  came to into a status (`Answered`), so the routes cannot disagree. A request `Hosts` does not
  admit is refused before any route; no GET runs a command.

No source file sits at the module's root. The test tree mirrors it.

## What the code cannot say

- **Serve writes no files.** Everything a harness is told it is told in an answer.
- **A worker is named by configuration, never by a host.** No hostname is read or stored.
