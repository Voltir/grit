# grit.edge

An edge's side of the engine (ADR 0017), over core's traits alone: how an edge serves the tool
requests routed to the places it registers, the instructions it reads at a place, and the one
HTTP listener every edge serving HTTP shares. It is core in the sense of
[`docs/extending.md`](../../docs/extending.md#where-code-goes): what every edge shares and no one
protocol owns, so the Slack and MCP edges, the chat and the kit share it. It depends on core
alone (its Mill `moduleDeps`).

Two packages, `desk` and `http`, which name each other in neither direction; each names core
alone, and `http` the JDK's HTTP server besides, which nothing else in grit's main sources
imports (STYLE rule 8).

## `grit.edge.desk`

What works a `Desk` (ADR 0017): the server that claims and runs the tool requests routed to an
edge's places, one request's run, and what a desk's edge advertises with them: `PlaceFragments`
belongs here as the instructions an edge serving a place advertises with its tools.

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

## `grit.edge.http`

An address answered over HTTP: the transport, and nothing carried over it (no MCP, no route, no
credential). An edge hands `Listener.open` a function from a `Request` to a `Response` and gets a
`Listener` it closes.

- **`Listen`**: a host and port read from text, an IPv6 literal in brackets, and whether it is
  loopback, read from the text alone, so `java.net.InetAddress` stays in `grit.host`.
- **`Request`**, **`Verb`**: a request read whole, its target decoded by `Request.of`, which a
  caller's tests use to build requests as the listener reads them.
- **`Response`**, **`Status`**, **`ContentType`**: what a request is answered with; a status that
  needs a header carries what the header says (`Status.NotAllowed` its verbs).
- **`Listener`**: the JDK's `HttpServer` behind the bounds its constants name: each request on a
  virtual thread, a body capped and read within a deadline, and a `close` that returns only once
  no request can still call `answer`.

What the code cannot say: the JDK's server takes the handler that holds `answer` and `log`
untracked, since a Java method's parameter is not capture checked. The proof that this cannot be
observed outside the `Listener`'s capture set sits beside `createContext`; it rests on `close`,
so a listener never closed keeps answering. `answer` is lent `log` per request rather than
holding it, which separation checking requires; `grit.edge.probes` pins it.
