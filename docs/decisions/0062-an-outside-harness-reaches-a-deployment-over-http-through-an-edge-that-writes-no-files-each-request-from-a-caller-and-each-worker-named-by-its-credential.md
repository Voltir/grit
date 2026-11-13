# 0062. An outside harness reaches a deployment over HTTP through an edge that writes no files, each request from a caller and each worker named by its credential

Status: accepted (2026-11-13)

Context: a coding harness beside grit (one that edits code on a machine or in a sandbox) runs a
deployment's plugin commands, calls them as MCP tools, and waits on the store for what another
worker does (ADR 0051). One deployment served this with routes of its own, a single owner,
brief files written to disk for the harness to read, and slots named after the machine. A
second machine, or a sandbox, breaks each of these: files are on one disk, a host's name is
whatever the machine says, and a route that assumes the owner cannot later take a worker's
credential. Turned down:

- **An edge per harness.** Each would repeat the routes, the checks and the status table; a
  harness's own words belong in its adapter.
- **Files a harness reads.** They live on one machine; an answer reaches any.
- **A worker named by its host.** A host's name is not a credential, and two sandboxes may share
  one. No `Slot` type: a worker is named by configuration.
- **Each route checking who is asking.** A credential added later would touch every route.

Decision:

- **`grit.commands` is an edge extension** a deployment mounts as a `ServedEdge`. It names core,
  `grit.edge` and `grit.mcp`'s wire, never a harness, a plugin or `grit.turn`. A wait on a
  plugin's document, and a deployment's own commands, are values the deployment passes in.
- **Every route takes a caller** (`Caller`): an account, the integrity its words are believed
  at, and its worker's name. Routes ask `Callers` alone who a request is from. Today there is
  one rule: a request with no credential is the deployment's owner, and the edge must then
  listen on loopback and admit only loopback's names (`Hosts.loopback`), so **any process on
  that machine is the owner**. Credentials per worker are added to `Callers` later without
  touching a route.
- **A worker is named by its credential** (by the deployment's configuration until credentials
  are), never by a host. The owner's worker name is configuration, and a credential for that
  machine later must reuse it. A directory on a worker is the place `worker:<name>/<dir>`
  (ADR 0013), so one path on two workers is two places.
- **Every request is admitted by `Hosts` before any route**: its `Host` must name one of the
  listener's names, a present `Origin` must be `http://` that `Host`, and a present
  `Sec-Fetch-Site` must be `same-origin` or `none`. This keeps a page another site serves out,
  through DNS rebinding or a cross-site request. No GET writes, since a page may send one with
  neither header.
- **A deployment's own commands** are of two kinds: one guarding a plugin's command of the same
  name (offered at `/mcp` with the plugin's schema, refusing a caller below its integrity, and
  otherwise running what it wraps), and one added beside them, at `/command` alone. The edge
  refuses to open when a guard names no offered command or an added one collides.
- **Serve writes no files.**

Consequences:

- A harness on any machine reaches a deployment with plain HTTP; what is said to it is in its
  answers.
- Anyone who can run a process on the owner's machine acts as the owner until credentials are
  added; the edge refuses to listen beyond loopback while it takes uncredentialed requests.
- A guard is an interim home for an integrity check its plugin should make itself.
- Enforced by the law (`commands-names-core-edge-and-mcp-wire`), `CommandsEdge.of`'s refusals,
  and `HostsTests`.
