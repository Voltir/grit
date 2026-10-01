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

  /** The tool `entry` lists as, under the prefix `github`, when grit may offer it. */
  private def listed(entry: ujson.Obj): Either[McpError, Option[McpTool]] =
    McpTool.page(ujson.Obj("tools" -> ujson.Arr(entry)), "github").map(_.tools.headOption)

  /** The `Mcp-Param-*` headers of a call of `entry` with `arguments`. */
  private def mirrored(
      entry: ujson.Obj,
      arguments: ujson.Obj
  ): Either[McpError, Option[Vector[(String, String)]]] =
    listed(entry).map(
      _.map(t => Headers.of(Rpc.Call.CallTool(t, arguments)).filter(_._1.startsWith("Mcp-Param-")))
    )

  /** A read-only tool whose schema has `properties`. */
  private def tool(name: String, properties: ujson.Obj): ujson.Obj = ujson.Obj(
    "name" -> name,
    "inputSchema" -> ujson.Obj("type" -> "object", "properties" -> properties),
    "annotations" -> ujson.Obj("readOnlyHint" -> true)
  )

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

    test("a call mirrors its annotated argument: the spec's execute_sql example") {
      // streamable-http.mdx, Schema Extension: its example tool and its resulting request.
      val executeSql = tool(
        "execute_sql",
        ujson.Obj(
          "region" -> ujson.Obj(
            "type" -> "string",
            "description" -> "The region to execute the query in",
            "x-mcp-header" -> "Region"
          ),
          "query" -> ujson.Obj("type" -> "string", "description" -> "The SQL query to execute")
        )
      )
      listed(executeSql).map(
        _.map(t =>
          Headers.of(
            Rpc.Call.CallTool(
              t,
              ujson.Obj("region" -> "us-west1", "query" -> "SELECT * FROM users")
            )
          )
        )
      ) ==> Right(
        Some(
          Vector(
            "Accept" -> "application/json, text/event-stream",
            "Content-Type" -> "application/json",
            "MCP-Protocol-Version" -> "2026-07-28",
            "Mcp-Method" -> "tools/call",
            "Mcp-Name" -> "execute_sql",
            "Mcp-Param-Region" -> "us-west1"
          )
        )
      )
    }

    test("a call of GitHub's get_file_contents mirrors owner and repo, as GitHub requires") {
      // The live list's entry (extensions/mcp/README.md); without these GitHub answers -32020.
      val entry = ujson.Obj.from(
        ujson
          .read(
            scala.io.Source
              .fromInputStream(getClass.getResourceAsStream("/github/tools-list.json"))
              .mkString
          )("tools")
          .arr
          .find(_.obj.get("name").contains(ujson.Str("get_file_contents")))
          .fold(Iterable.empty[(String, ujson.Value)])(_.obj)
      )
      mirrored(
        entry,
        ujson.Obj("owner" -> "the-actual-best", "repo" -> "actualbest", "path" -> "README.md")
      ) ==> Right(
        Some(Vector("Mcp-Param-owner" -> "the-actual-best", "Mcp-Param-repo" -> "actualbest"))
      )
    }

    test(
      "a value is mirrored as text: a string as it is, a boolean in lowercase, an integer in decimal"
    ) {
      // streamable-http.mdx, Value Encoding: type conversion, then the encoding table.
      val kinds = tool(
        "kinds",
        ujson.Obj(
          "s" -> ujson.Obj("type" -> "string", "x-mcp-header" -> "S"),
          "u" -> ujson.Obj("type" -> "string", "x-mcp-header" -> "U"),
          "b" -> ujson.Obj("type" -> "boolean", "x-mcp-header" -> "B"),
          "i" -> ujson.Obj("type" -> "integer", "x-mcp-header" -> "I"),
          "n" -> ujson.Obj("type" -> "integer", "x-mcp-header" -> "N"),
          "o" -> ujson.Obj(
            "type" -> "object",
            "properties" -> ujson.Obj(
              "deep" -> ujson.Obj("type" -> "string", "x-mcp-header" -> "Deep")
            )
          )
        )
      )
      mirrored(
        kinds,
        ujson.Obj(
          "s" -> "us-west1",
          "u" -> "Hello, 世界",
          "b" -> false,
          "i" -> -7,
          "n" -> 9007199254740991L.toDouble,
          "o" -> ujson.Obj("deep" -> " padded ")
        )
      ) ==> Right(
        Some(
          Vector(
            "Mcp-Param-S" -> "us-west1",
            "Mcp-Param-U" -> "=?base64?SGVsbG8sIOS4lueVjA==?=",
            "Mcp-Param-B" -> "false",
            "Mcp-Param-I" -> "-7",
            "Mcp-Param-N" -> "9007199254740991",
            "Mcp-Param-Deep" -> "=?base64?IHBhZGRlZCA=?="
          )
        )
      )
    }

    test("an argument absent, null, or not a string, boolean or safe integer is not mirrored") {
      // streamable-http.mdx, Server Behavior for Custom Headers: absent and null are omitted;
      // any other value breaks the schema, which the server refuses.
      val kinds = tool(
        "kinds",
        ujson.Obj(
          "absent" -> ujson.Obj("type" -> "string", "x-mcp-header" -> "Absent"),
          "nul" -> ujson.Obj("type" -> "string", "x-mcp-header" -> "Nul"),
          "frac" -> ujson.Obj("type" -> "integer", "x-mcp-header" -> "Frac"),
          "huge" -> ujson.Obj("type" -> "integer", "x-mcp-header" -> "Huge"),
          "list" -> ujson.Obj("type" -> "string", "x-mcp-header" -> "List"),
          "o" -> ujson.Obj(
            "type" -> "object",
            "properties" -> ujson.Obj(
              "deep" -> ujson.Obj("type" -> "string", "x-mcp-header" -> "Deep")
            )
          )
        )
      )
      mirrored(
        kinds,
        ujson.Obj(
          "nul" -> ujson.Null,
          "frac" -> 1.5,
          "huge" -> 9007199254740992L.toDouble,
          "list" -> ujson.Arr("a"),
          "o" -> "not an object"
        )
      ) ==> Right(Some(Vector.empty))
    }
  }
}
