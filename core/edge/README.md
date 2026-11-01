# grit.edge

An edge's side of the engine (ADR 0017), over core's traits alone: how an edge serves the tool
requests routed to the places it registers, and the instructions it reads at a place. It is
core in the sense of [`docs/extending.md`](../../docs/extending.md): it names no protocol, so the
Slack and MCP edges, the chat and the kit share it. It depends on core alone (its Mill `moduleDeps`).

One idea, so one package, `grit.edge`:

- **`Server`**: serves a `Desk`'s requests on a thread of its own, each run by the edge's
  `Tools` (what its tools are where a request was routed) on a virtual thread of its own,
  with orphans settled on every pass. What it tells an operator names a request by its slot,
  never by its arguments or its answer.
- **`Run`**: one request run by a `Toolbox`: its arguments read under their repairs, then run
  free or approved as its permit says.
- **`PlaceFragments`**: a place's own instructions as the Place layer of a system prompt, its
  instruction files or the words a deployment declared for it, bounded so a deep tree cannot
  crowd out the rest.

What the code cannot say: an edge holds no transaction and reaches the store only through the
`Desk` and the other traits it is handed (ADR 0002), so anything served here works the same
for an edge in another process. `Server` is lent its `said` per run rather than sharing it with
`Tools`, which separation checking requires; `grit.edge.probes` pins it.
