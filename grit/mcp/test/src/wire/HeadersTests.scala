package grit.mcp.wire

import utest.*

/** [[Headers]]: what a request carries besides its body and its token. */
object HeadersTests extends TestSuite {

  private val issueRead: McpTool =
    McpTool
      .page(
        ujson.Obj(
          "tools" -> ujson.Arr(
            ujson.Obj(
              "name" -> "issue_read",
              "inputSchema" -> ujson.Obj("type" -> "object"),
              "annotations" -> ujson.Obj("readOnlyHint" -> true)
            )
          )
        ),
        "github"
      )
      .toOption
      .flatMap(_.tools.headOption)
      .getOrElse(throw new java.lang.AssertionError("issue_read not offered"))

  val tests = Tests {
    test("a value is sent as it is, or Base64 in the sentinel when the spec's table says so") {
      // streamable-http.mdx, Value Encoding: the table's five rows.
      Vector("us-west1", "Hello, 世界", " padded ", "line1\nline2", "=?base64?literal?=")
        .map(Headers.encoded) ==> Vector(
        "us-west1",
        "=?base64?SGVsbG8sIOS4lueVjA==?=",
        "=?base64?IHBhZGRlZCA=?=",
        "=?base64?bGluZTEKbGluZTI=?=",
        "=?base64?PT9iYXNlNjQ/bGl0ZXJhbD89?="
      )
    }

    test("an inner space or tab is plain; one at either end, or a control character, is not") {
      Vector("a b\tc", "\tlead", "trail\t", "bell\u0007", "del\u007f", "=?base64?", "?=")
        .map(Headers.encoded) ==> Vector(
        "a b\tc",
        "=?base64?CWxlYWQ=?=",
        "=?base64?dHJhaWwJ?=",
        "=?base64?YmVsbAc=?=",
        "=?base64?ZGVsfw==?=",
        "=?base64?",
        "?="
      )
    }

    test("every request names its method and protocol version, and a call its tool") {
      // streamable-http.mdx: Sending Messages 2, Protocol Version Header, Standard Request Headers.
      Headers.of(Rpc.Call.ListTools(Some("c"))) ==> Vector(
        "Accept" -> "application/json, text/event-stream",
        "Content-Type" -> "application/json",
        "MCP-Protocol-Version" -> "2026-07-28",
        "Mcp-Method" -> "tools/list"
      )
      Headers.of(Rpc.Call.CallTool(issueRead, ujson.Obj("owner" -> "x"))) ==> Vector(
        "Accept" -> "application/json, text/event-stream",
        "Content-Type" -> "application/json",
        "MCP-Protocol-Version" -> "2026-07-28",
        "Mcp-Method" -> "tools/call",
        "Mcp-Name" -> "issue_read"
      )
    }
  }
}
