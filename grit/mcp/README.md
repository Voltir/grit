# grit.mcp

grit's client of Model Context Protocol servers, at revision 2026-07-28 over Streamable
HTTP: no handshake and no session, each request a POST of its own carrying its protocol
version. A server's tools are offered to a turn by an edge hosting them at a service place
(ADR 0017), and only those the server marks read-only. A shipped extension (ADR 0021): it
names a generic protocol and no organisation, depends on `grit.core` and `grit.edge`, and is
named only by a deployment (`mcp-names-core-alone`). The JDK HTTP client's second egress
beside `grit.models` (STYLE rule 8); neither names the other.

In dependency order:

- **`wire`** — the protocol over ujson, pure: `McpError`, every expected failure of one
  exchange; `McpTool`, a listed tool grit may offer, read from a `tools/list` page with those
  `Skipped` and why; `Rpc`, the requests grit sends and the results and errors read back;
  `Headers`, what a request carries besides its body; `Sse`, the response in an event
  stream; `Answer`, a call's result as the text the model reads. Imports nothing in mcp.
- **`client`** — a server reached over HTTP: `McpServer`, a server a deployment declares;
  `Bearer`, its token read from the environment and never shown; `McpClient`, its tools
  (`Listed`) and calls, each request a POST of its own. The JDK HTTP client lives here alone.
  Imports `wire`.
- **`edge`** — the servers' tools served at a service place: `McpEdge`, the `ServedEdge` a
  deployment declares, which registers the place, advertises there and serves it; and
  `McpTools`, the `grit.edge.Tools` it serves with, which runs a request by calling its server
  and tells the desk what to advertise when the lists change. Imports `wire` and `client`.

The test tree mirrors it. `client`'s and `edge`'s tests run against `FakeMcpServer`, an MCP server in the
test JVM (`com.sun.net.httpserver`) that checks every request as the spec's server rules say.
`McpServerContract` is the exchanges a server must answer as the client reads them; the fake
keeps it in the unit tier (`McpServerFakeTests`), and `LiveProbe`, a main in test sources that
needs a token and so is no tier, will run it against GitHub's at the live run (its doc says
how); until then the fake is held to the spec and go-sdk's source, not to a real server.

`test/resources/github/tools-list.json` is the `tools/list` result GitHub's hosted read-only
server (`https://api.githubcopilot.com/mcp/readonly`) answered on 2026-09-30, its icons
removed and nothing else changed, so the reader and the header mirroring are tested on the
entries a real server sends, `x-mcp-header` annotations included. `test/resources/toolsnaps/`
holds GitHub's write tool `issue_write`, which that server does not list, as its source lists
it: https://github.com/github/github-mcp-server at `5a1a3866d4a681d771162d35680d6c7219eac3b0`
(MIT). The source's snapshots lack the hosted server's annotations, so they are not kept for
its read tools.
