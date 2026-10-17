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
        McpError.OutOfScope("its scope does not offer search_code"),
        McpError.Unattributed("its answer's text is not a JSON object"),
        McpError.Unplaced("the file's folder could not be looked up"),
        McpError.NoToken("https://oauth2.example.com/token answered HTTP 400: invalid_grant"),
        McpError.NotLooked("the call names no fileId, which grit looks up with meta first")
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
        // The model reads this as the call's result: it says nothing was read, and to
        // tell the person rather than answer around it.
        "was not asked, so nothing of it was read: its scope does not offer search_code. " +
          "Tell the person this is out of reach; do not answer with something else in its place.",
        "answered, but grit cannot hold the answer to its scope, so none of it is shown: " +
          "its answer's text is not a JSON object",
        "answered, but grit could not tell who may see the answer, so none of it is shown: " +
          "the file's folder could not be looked up",
        "was not asked: grit could not get a token for it: " +
          "https://oauth2.example.com/token answered HTTP 400: invalid_grant",
        "was not asked, since grit could not first look up what it would read: " +
          "the call names no fileId, which grit looks up with meta first"
      )
    }

    test("the operator's log is told an error's line, but nothing of a call out of scope") {
      (
        McpError.OutOfScope("its calls are limited to repo r, and this one has repo secret").logged,
        McpError.Status(502).logged
      ) ==> (None, Some("answered HTTP 502"))
    }

    test(
      "a server's own words reach the operator's log as one line of at most LoggedMost characters, cut at a word and marked"
    ) {
      // 300 characters over three lines; the first 40 words are the most that fit in 200.
      val said = Vector.fill(3)(Vector.fill(20)("word").mkString(" ")).mkString("\n")
      val kept = Vector.fill(40)("word").mkString(" ") + " [cut]"
      (
        McpError.Rejected(400, said).logged,
        McpError.Rpc(-32602, said).logged,
        McpError.Rpc(-32602, "Unknown tool").logged
      ) ==> (
        Some(s"answered HTTP 400 with no MCP error: $kept"),
        Some(s"answered error -32602: $kept"),
        Some("answered error -32602: Unknown tool")
      )
    }
  }
}
