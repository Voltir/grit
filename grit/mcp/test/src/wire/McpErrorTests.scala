package grit.mcp.wire

import utest.*

/** [[McpError]]: each failure as the line a person or the model reads. */
object McpErrorTests extends TestSuite {

  val tests = Tests {
    test("each error's line says what the server did, with what it said") {
      Vector(
        McpError.Unreachable("connection refused"),
        McpError.Unauthorized(Some("Bearer realm=\"mcp\"")),
        McpError.Unauthorized(None),
        McpError.Forbidden(Some("Bearer error=\"insufficient_scope\", scope=\"repo\"")),
        McpError.Unsupported(Vector("2025-11-25", "2025-06-18")),
        McpError.Unsupported(Vector.empty),
        McpError.Rejected(405, "Method Not Allowed"),
        McpError.Rejected(404, ""),
        McpError.Rpc(-32602, "Unknown tool: invalid_tool_name"),
        McpError.InputRequired,
        McpError.Unreadable("no result"),
        McpError.Status(502),
        McpError.OutOfScope("its scope does not offer search_code")
      ).map(_.message) ==> Vector(
        "could not be reached: connection refused",
        "refused grit's token (HTTP 401; it asks: Bearer realm=\"mcp\")",
        "refused grit's token (HTTP 401)",
        "grit's token lacks a permission (HTTP 403; it asks: Bearer error=\"insufficient_scope\", scope=\"repo\")",
        "does not speak MCP 2026-07-28; it speaks 2025-11-25, 2025-06-18",
        "does not speak MCP 2026-07-28, and names no version it does",
        "answered HTTP 405 with no MCP error: Method Not Allowed",
        "answered HTTP 404 with no MCP error",
        "answered error -32602: Unknown tool: invalid_tool_name",
        "wanted input that grit does not give",
        "answered something grit cannot read: no result",
        "answered HTTP 502",
        "was not asked: its scope does not offer search_code"
      )
    }
  }
}
