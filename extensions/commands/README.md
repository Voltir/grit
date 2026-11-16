# grit.commands

Implements `ServedEdge`: the edge a deployment mounts to serve an outside coding harness over
HTTP ([ADR 0062](../../docs/decisions/0062-an-outside-harness-reaches-a-deployment-over-http-through-an-edge-that-writes-no-files-each-request-from-a-caller-and-each-machine-named-by-its-credential.md)).
A shipped edge extension (ADR 0021): it names core, `grit.edge` and `grit.mcp`'s wire alone
(`commands-names-core-edge-and-mcp-wire`), and no harness, no plugin and no MCP client. What a
deployment's own commands and waits need of a plugin is handed to it as values.

In dependency order, each package importing only those before it:

- **`machine`**: a machine's name, and where its work is placed: a directory on a machine is the
  place `machine:<name>/<dir>`; and the claim holder a session there, or an agent it starts,
  works as (`ClaimHolders`), readable in a claims listing and the same whoever computes it.
- **`caller`**: who a request is from (`Caller`: an account, the integrity its words are
  believed at, and the machine it is on), and the deployment's rule for it (`Callers`): every
  request the owner's while none bears a credential (`Callers.owner`), or each the caller of the
  credential it bears (`Callers.of`, [below](#credentials)). Routes ask `Callers` alone.
- **`declare`**: what a deployment hands the edge as values, pure: its own commands
  (`OwnCommand`), each guarding a plugin's command or added beside them, and lent the request's
  way to run a plugin's command (`Ran`) for that request alone; and the wait on a plugin's
  document `/wait` reads (`HoldingWatch`), so the edge names no plugin. Each harness's adapter is
  core's `Adapter`, whose join script `/join/<harness>` serves, so the edge names no harness and an
  adapter's module names no `grit.commands`.
- **`events`**: a harness's event lines, private to the module: one JSON object a line in
  grit's words (ADR 0050), read into one batch of one session's events, each line refused named
  by its number and why. A harness's adapter maps its hooks onto them; nothing here names one.
- **`routes`**: the routes, private to the module, and the one table that turns what a command
  came to into a status (`RouteReply`), so the routes cannot disagree. On every route a query
  parameter it does not read is 400, named (`/command`, `/mcp`, `/wait` and `/join` read none), and
  so is a key of `/wait`'s body; a command's arguments are its plugin's to refuse, and an event
  line's unread key refuses that line alone. A request `Hosts` does not
  admit (403), or `Callers` finds no caller for (401), is refused before any route, and no route
  sees its `Authorization` header; no GET runs a command. The listener's own answers, 413, 400
  and 503, come before the caller is read.
- **`edge`**: `CommandsEdge`, the `ServedEdge` a deployment mounts: `of` refuses what cannot be
  served safely (two own commands of one name, a caller outside its realm, an address beyond
  loopback, and without credentials a name beyond loopback's), and `open` refuses own commands
  that do not match the plugins' offered ones and an offered command `/mcp` cannot list, vouches
  each caller's account, and listens.

No source file sits at the module's root. The test tree mirrors it.

## Credentials

A deployment that serves more than its owner gives each person's machine a credential, and
configures the edge with `Callers.of`, one `Callers.Token(digest, caller)` for each: the
credential's SHA-256 digest, never its text, and the caller a request bearing it is from. A
digest is no secret, so it may sit in the deployment's code or its configuration; the text is
the person's alone. Whoever runs the edge makes a credential and its digest so, the text never on
a command line:

```
umask 077; head -c 32 /dev/urandom | base64 | tr -d '=\n' | tr '+/' '-_' > ~/.config/grit/credential
tr -d '\n' < ~/.config/grit/credential | sha256sum | cut -d' ' -f1
```

and hands the file to the person whose machine it is, who joins with it
([the Claude Code adapter](../claudecode/README.md#the-join)). One account may be given two
credentials: one for the person's own command line, believed in full, and one for the sessions
they start, believed lower, which then write as that person at that integrity; or one on each
of two machines, whose directories are then two places.

## What the code cannot say

- **Serve writes no files.** Everything a harness is told it is told in an answer.
- **A machine is named by configuration, never by a host.** No hostname is read or stored.
