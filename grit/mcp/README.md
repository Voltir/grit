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

The test tree mirrors it.
