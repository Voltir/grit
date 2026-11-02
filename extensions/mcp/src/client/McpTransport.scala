package grit.mcp.client

import java.net.URI

import grit.core.host.Argv

/** How grit reaches an MCP server. */
enum McpTransport extends caps.Pure {

  /** Streamable HTTP: each request a POST of its own to `endpoint`, carrying `Authorization:
    * Bearer` with the token `credential` gives.
    */
  case Http(endpoint: URI, credential: Credential)

  /** stdio: `argv` started at the root of the `Processes` that start it, one JSON-RPC message a
    * line each way; what it writes to its errors is read only for the operator's log.
    */
  case Stdio(argv: Argv)
}
