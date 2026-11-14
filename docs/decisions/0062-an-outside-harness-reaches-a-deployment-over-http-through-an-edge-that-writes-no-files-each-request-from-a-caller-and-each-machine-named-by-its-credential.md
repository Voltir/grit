# 0062. An outside harness reaches a deployment over HTTP through an edge that writes no files, each request from a caller and each machine named by its credential

Status: accepted (2026-11-13); revised (2026-11-14)

Context: a coding harness beside grit (one that edits code on a machine or in a sandbox) runs a
deployment's plugin commands, calls them as MCP tools, and waits on the store for what another
session does (ADR 0051). One deployment served this with routes of its own, a single owner,
brief files written to disk for the harness to read, and slots named after the machine. A
second machine, or a sandbox, breaks each of these: files are on one disk, a host's name is
whatever the machine says, and a route that assumes the owner cannot later take another
caller's credential. Turned down:

- **An edge per harness.** Each would repeat the routes, the checks and the status table; a
  harness's own words belong in its adapter.
- **Files a harness reads.** They live on one machine; an answer reaches any.
- **A machine named by its host.** A host's name is not a credential, and two sandboxes may share
  one. No `Slot` type: a machine is named by configuration.
- **Each route checking who is asking.** A second rule for callers would touch every route.
- **A credential's text in the edge's configuration** (a `Secret` read from its environment):
  the serving process's environment and the deployment's `.env` are readable by any session the
  same user starts, so the owner's credential could not be kept from them. A digest is no secret.
- **One account per machine.** A person on two machines is one person, and a person's own command
  line and the sessions they start on one machine are one person at two integrities.

Decision:

- **`grit.commands` is an edge extension** a deployment mounts as a `ServedEdge`. It names core,
  `grit.edge` and `grit.mcp`'s wire, never a harness, a plugin or `grit.turn`. A wait on a
  plugin's document, and a deployment's own commands, are values the deployment passes in.
- **Every route takes a caller** (`Caller`), and routes ask `Callers` alone who a request is
  from. A deployment chooses one of two rules. `Callers.owner`: a request with no credential is
  the deployment's owner, and the edge then admits only loopback's names (`Hosts.loopback`), so
  **any process on that machine is the owner**. `Callers.of`: every request bears a credential
  as `Authorization: Bearer`, and the edge's configuration holds each credential's SHA-256
  digest, never its text, so neither the configuration nor the serving process's environment
  holds a secret; the text is in the person's file and on the wire alone. A request bearing none
  is 401 `WWW-Authenticate: Bearer`, one bearing a text no digest is that of 401
  `error="invalid_token"` (RFC 6750, and MCP's authorization), a known caller below a command's
  integrity 403. Either way the edge listens on loopback alone until it is served over TLS,
  since a credential crosses the network in clear.
- **A credential names two facts, apart**: who is asking, an account and the integrity its words
  are believed at, and which machine it is on, the name that places its directories and from
  which a claim holder is built (ADR 0049). One account may hold two credentials, at two
  integrities (a person's own command line believed in full, the sessions they start believed
  lower: an agent working for its person writes as that person, and the integrity on what it
  keeps tells their words from its) or on two machines; one machine may carry two accounts.
  Only what cannot be told apart is refused: two credentials of one digest.
- **A machine is named by its credential's configuration**, never by a host. A directory on a
  machine is the place `machine:<name>/<dir>` (ADR 0013), so one path on two machines is two
  places, and a credential for the machine an owner's sessions run on reuses its name.
- **Only whoever began a session records on it.** A batch of events on a session another person
  began, or one kept where the caller does not read, is refused alike (403 at `/events`), so a
  second caller cannot move another's last event or replace their summary.
- **Every request is admitted by `Hosts` before any route**: its `Host` must name one of the
  listener's names, a present `Origin` must be `http://` that `Host`, and a present
  `Sec-Fetch-Site` must be `same-origin` or `none`. This keeps a page another site serves out,
  through DNS rebinding or a cross-site request. No GET writes, since a page may send one with
  neither header.
- **A deployment's own commands** are of two kinds: one guarding a plugin's command of the same
  name (offered at `/mcp` with the plugin's schema, refusing a caller below its integrity, and
  otherwise running what it wraps), and one added beside them, at `/command` alone. The edge
  refuses to open when a guard names no offered command or an added one collides.
- **A harness's words live in its adapter**, one module a harness (`grit.claudecode` for Claude
  Code), naming core alone. An adapter is pure data a deployment hands the edge: core's
  `Adapter`, the harness's name and its join script. The edge serves the script at
  `GET /join/<harness>` as text and runs nothing of it; a person runs it in a checkout, where it
  installs the harness's hooks, which translate the harness's events into the edge's routes
  (`/brief`, `/holder`, `/events`), and registers the edge's `/mcp`. The deployment's address
  reaches the script as its argument, never as a path into another repository.
- **Serve writes no files.** A join writes the harness's files in the person's checkout, never
  the edge.

Consequences:

- A harness on any machine reaches a deployment with plain HTTP; what is said to it is in its
  answers.
- Under `Callers.owner`, anyone who can run a process on the owner's machine acts as the owner.
  Under `Callers.of`, whoever can read a credential's file acts as its caller: a person's own
  credential is kept where the sessions they start cannot read it, or those sessions are them.
- What the labels say of a caller: its clearance is its account's person's own, the same for
  every credential of that account, until an agent is bounded below its person; what a command
  keeps is believed at no more than the caller's integrity met with the room's cap (ADR 0053),
  but a plugin's commands are not gated by integrity, only a deployment's own guards are; a brief
  is fenced as a brief but does not show the integrity of what it carries, so text an agent kept
  reads in another session's brief like any other; and an outside harness's model, upstream of
  everything it is told, is cleared by nothing.
- A guard is an interim home for an integrity check its plugin should make itself.
- A join script is piped into a shell, so it is written to change nothing when cut short and to
  refuse a plain `http://` address beyond loopback. With a credential it checks the file is the
  person's alone and the edge takes it, keeps it off every command line, and sends it only to
  the edge it was joined to.
- Enforced by the law (`commands-names-core-edge-and-mcp-wire`, `claudecode-names-core-alone`),
  `CommandsEdge.of`'s refusals, `HostsTests`, `CallersTests` and the sessions contract.
