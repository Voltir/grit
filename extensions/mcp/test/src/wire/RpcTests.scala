package grit.mcp.wire

import utest.*

/** [[Rpc]]: the requests grit sends, and the results and errors it reads back. */
object RpcTests extends TestSuite {

  /** The spec's get_weather (server/tools.mdx, Listing Tools), marked read-only. */
  private val weather: McpTool =
    McpTool
      .page(
        ujson.Obj(
          "tools" -> ujson.Arr(
            ujson.Obj(
              "name" -> "get_weather",
              "description" -> "Get current weather information for a location",
              "inputSchema" -> ujson.Obj("type" -> "object"),
              "annotations" -> ujson.Obj("readOnlyHint" -> true)
            )
          )
        ),
        "weather"
      )
      .toOption
      .flatMap(_.tools.headOption)
      .getOrElse(throw new java.lang.AssertionError("get_weather not offered"))

  // The per-request fields every request carries (basic/index.mdx, _meta, Per-request
  // protocol fields): the version and capabilities are required, clientInfo SHOULD be sent.
  private val meta = ujson.Obj(
    "io.modelcontextprotocol/protocolVersion" -> "2026-07-28",
    "io.modelcontextprotocol/clientCapabilities" -> ujson.Obj(),
    "io.modelcontextprotocol/clientInfo" -> ujson.Obj("name" -> "grit", "version" -> "0.1.0")
  )

  val tests = Tests {
    test("a request is JSON-RPC 2.0 with its params' _meta, as the spec's examples are") {
      // streamable-http.mdx, Standard Request Headers: the tools/call body.
      Rpc.request(1, Rpc.Call.CallTool(weather, ujson.Obj("location" -> "Seattle, WA"))) ==>
        ujson.Obj(
          "jsonrpc" -> "2.0",
          "id" -> 1,
          "method" -> "tools/call",
          "params" -> ujson.Obj(
            "name" -> "get_weather",
            "arguments" -> ujson.Obj("location" -> "Seattle, WA"),
            "_meta" -> meta
          )
        )
      Rpc.request(2, Rpc.Call.ListTools(None)) ==>
        ujson.Obj(
          "jsonrpc" -> "2.0",
          "id" -> 2,
          "method" -> "tools/list",
          "params" -> ujson.Obj("_meta" -> meta)
        )
      Rpc.request(3, Rpc.Call.ListTools(Some(""))) ==>
        ujson.Obj(
          "jsonrpc" -> "2.0",
          "id" -> 3,
          "method" -> "tools/list",
          "params" -> ujson.Obj("cursor" -> "", "_meta" -> meta)
        )
    }

    test("a complete result is the result, and an absent resultType is complete") {
      // server/tools.mdx, Error Handling: an isError result is still a result.
      val isError = ujson.Obj(
        "resultType" -> "complete",
        "content" -> ujson.Arr(
          ujson.Obj(
            "type" -> "text",
            "text" -> "Invalid departure date: must be in the future. Current date is 08/08/2025."
          )
        ),
        "isError" -> true
      )
      Rpc.result(4, ujson.Obj("jsonrpc" -> "2.0", "id" -> 4, "result" -> isError)) ==> Right(
        isError
      )
      val older = ujson.Obj("tools" -> ujson.Arr())
      Rpc.result(5, ujson.Obj("jsonrpc" -> "2.0", "id" -> 5, "result" -> older)) ==> Right(older)
    }

    test("input_required is refused, and a resultType grit does not know is unreadable") {
      // server/tools.mdx, Input Required Tool Results.
      val wanted = ujson.Obj(
        "resultType" -> "input_required",
        "inputRequests" -> ujson.Obj(
          "github_login" -> ujson.Obj("method" -> "elicitation/create", "params" -> ujson.Obj())
        ),
        "requestState" -> "eyJsb2NhdGlvbiI6Ik5ldyBZb3JrIn0..."
      )
      Rpc.result(2, ujson.Obj("jsonrpc" -> "2.0", "id" -> 2, "result" -> wanted)) ==>
        Left(McpError.InputRequired)
      Rpc.result(
        2,
        ujson.Obj("jsonrpc" -> "2.0", "id" -> 2, "result" -> ujson.Obj("resultType" -> "partial"))
      ) ==>
        Left(McpError.Unreadable("an unknown resultType: partial"))
    }

    test("an answer to another request, or not a response at all, is unreadable") {
      Vector(
        ujson.Obj("jsonrpc" -> "2.0", "id" -> 9, "result" -> ujson.Obj()),
        ujson.Obj("jsonrpc" -> "2.0", "id" -> "1", "result" -> ujson.Obj()),
        ujson.Obj(
          "jsonrpc" -> "2.0",
          "id" -> 9,
          "error" -> ujson.Obj("code" -> -32603, "message" -> "x")
        ),
        ujson.Obj("jsonrpc" -> "2.0", "id" -> 1, "result" -> ujson.Arr()),
        ujson.Obj("jsonrpc" -> "2.0", "method" -> "notifications/progress"),
        ujson.Arr()
      ).map(Rpc.result(1, _)) ==> Vector(
        Left(McpError.Unreadable("a response to request 9, not 1")),
        Left(McpError.Unreadable("a response to request \"1\", not 1")),
        Left(McpError.Unreadable("a response to request 9, not 1")),
        Left(McpError.Unreadable("a result that is not an object")),
        Left(McpError.Unreadable("neither a result nor an error")),
        Left(McpError.Unreadable("not a JSON object"))
      )
    }

    test("an error is the JSON-RPC error it carries; -32022 names the versions the server takes") {
      // server/tools.mdx, Error Handling; versioning.mdx, Protocol Version Negotiation.
      Rpc.result(
        3,
        ujson.Obj(
          "jsonrpc" -> "2.0",
          "id" -> 3,
          "error" -> ujson.Obj("code" -> -32602, "message" -> "Unknown tool: invalid_tool_name")
        )
      ) ==> Left(McpError.Rpc(-32602, "Unknown tool: invalid_tool_name"))
      Rpc.result(1, ujson.read(unsupported)) ==>
        Left(McpError.Unsupported(Vector("2026-07-28", "2025-11-25")))
      // An error whose id could not be read (basic/index.mdx, Error Responses) is still its error.
      Rpc.result(
        1,
        ujson.Obj(
          "jsonrpc" -> "2.0",
          "error" -> ujson.Obj("code" -> -32700, "message" -> "Parse error")
        )
      ) ==>
        Left(McpError.Rpc(-32700, "Parse error"))
      Rpc.result(
        1,
        ujson.Obj("jsonrpc" -> "2.0", "id" -> 1, "error" -> ujson.Obj("code" -> "bad"))
      ) ==>
        Left(McpError.Unreadable("an error with no code and message"))
    }

    test(
      "an HTTP failure is the token refused, its permission lacking, the error in its body, or its status"
    ) {
      val challenge = Some("Bearer error=\"insufficient_scope\", scope=\"repo\"")
      Vector(
        Rpc.failure(401, "", Some("Bearer realm=\"mcp\"")),
        Rpc.failure(403, ujson.write(rpcError(-32603, "no")), challenge),
        Rpc.failure(400, unsupported, None),
        Rpc.failure(404, ujson.write(rpcError(-32601, "Method not found")), None),
        Rpc.failure(
          400,
          ujson.write(
            ujson.Obj(
              "jsonrpc" -> "2.0",
              "error" -> ujson.Obj("code" -> -32020, "message" -> "Header mismatch")
            )
          ),
          None
        ),
        Rpc.failure(405, "<html>Method Not Allowed</html>", None),
        Rpc.failure(404, "", None),
        Rpc.failure(502, "Bad gateway", None),
        Rpc.failure(500, ujson.write(rpcError(-32603, "Internal error")), None)
      ) ==> Vector(
        McpError.Unauthorized(Some("Bearer realm=\"mcp\"")),
        McpError.Forbidden(challenge),
        McpError.Unsupported(Vector("2026-07-28", "2025-11-25")),
        McpError.Rpc(-32601, "Method not found"),
        McpError.Rpc(-32020, "Header mismatch"),
        McpError.Rejected(405, "<html>Method Not Allowed</html>"),
        McpError.Rejected(404, ""),
        McpError.Status(502),
        McpError.Rpc(-32603, "Internal error")
      )
    }

    test("a 4xx body with no MCP error is kept as one line of text, at most Excerpt characters") {
      // What a person reads of it: no line break or control character, nothing unbounded.
      Vector(
        Rpc.failure(400, "unauthorized:\r\n  bad\ttoken\u0007format\n", None),
        Rpc.failure(400, "x" * (Rpc.Excerpt + 1), None),
        Rpc.failure(400, "x" * Rpc.Excerpt, None)
      ) ==> Vector(
        McpError.Rejected(400, "unauthorized: bad token format"),
        McpError.Rejected(400, "x" * Rpc.Excerpt + "…"),
        McpError.Rejected(400, "x" * Rpc.Excerpt)
      )
    }
  }

  // versioning.mdx, Protocol Version Negotiation: the spec's UnsupportedProtocolVersionError.
  private val unsupported =
    """{"jsonrpc":"2.0","id":1,"error":{"code":-32022,"message":"Unsupported protocol version",""" +
      """"data":{"supported":["2026-07-28","2025-11-25"],"requested":"1900-01-01"}}}"""

  private def rpcError(code: Int, message: String): ujson.Obj =
    ujson.Obj(
      "jsonrpc" -> "2.0",
      "id" -> 1,
      "error" -> ujson.Obj("code" -> code, "message" -> message)
    )
}
