package grit.mcp.wire

import grit.core.tool.ToolName

import utest.*

/** [[McpTool.page]]: a `tools/list` page read, and only the tools grit may offer kept. */
object McpToolTests extends TestSuite {

  /** GitHub's own `tools/list` entry for `name` (see grit/mcp/README.md). */
  private def snap(name: String): ujson.Value =
    ujson.read(
      scala.io.Source
        .fromInputStream(getClass.getResourceAsStream(s"/toolsnaps/$name.snap"))
        .mkString
    )

  private def page(tools: ujson.Value*): ujson.Obj =
    ujson.Obj("resultType" -> "complete", "tools" -> ujson.Arr.from(tools))

  private def readOnly(name: String, extra: (String, ujson.Value)*): ujson.Obj = {
    val tool = ujson.Obj(
      "name" -> name,
      "description" -> "D.",
      "inputSchema" -> ujson.Obj("type" -> "object"),
      "annotations" -> ujson.Obj("readOnlyHint" -> true)
    )
    extra.foreach((k, v) => tool(k) = v)
    tool
  }

  private def read(result: ujson.Obj): McpTool.Page =
    McpTool.page(result, "github").getOrElse(throw new java.lang.AssertionError(s"unread: $result"))

  private def offered(p: McpTool.Page): Vector[(String, String, String)] =
    p.tools.map(t => (t.name, ToolName.value(t.offered), t.does))

  val tests = Tests {
    test("GitHub's read-only tools are offered under the prefix as listed; its write tool is not") {
      val p = read(page(snap("get_file_contents"), snap("issue_write"), snap("get_me")))
      offered(p) ==> Vector(
        (
          "get_file_contents",
          "github_get_file_contents",
          "Get file or directory contents: Get the contents of a file or directory from a GitHub repository"
        ),
        (
          "get_me",
          "github_get_me",
          "Get my user profile: Get details of the authenticated GitHub user. Use this when a request " +
            "is about the user's own profile for GitHub. Or when information is missing to build " +
            "other tool calls."
        )
      )
      p.tools.headOption.map(_.inputSchema) ==> snap("get_file_contents").obj.get("inputSchema")
      p.skipped ==> Vector(Skipped.NotReadOnly("issue_write"))
    }

    test(
      "a tool is read-only only when readOnlyHint is true: absent, false or not a boolean is not"
    ) {
      // The spec's own listing example, which has no annotations (server/tools.mdx, Listing Tools).
      val weather = ujson.Obj(
        "name" -> "get_weather",
        "title" -> "Weather Information Provider",
        "description" -> "Get current weather information for a location",
        "inputSchema" -> ujson.Obj("type" -> "object")
      )
      val p = read(
        page(
          weather,
          readOnly("hinted_false", "annotations" -> ujson.Obj("readOnlyHint" -> false)),
          readOnly("hinted_text", "annotations" -> ujson.Obj("readOnlyHint" -> "true")),
          readOnly("hinted")
        )
      )
      offered(p).map(_._1) ==> Vector("hinted")
      p.skipped ==> Vector(
        Skipped.NotReadOnly("get_weather"),
        Skipped.NotReadOnly("hinted_false"),
        Skipped.NotReadOnly("hinted_text")
      )
    }

    test("a name that is not a grit tool name under the prefix is skipped, never mangled") {
      // Names the spec allows (server/tools.mdx, Tool Names), and one too long once prefixed.
      val long = "a" * 58
      val p = read(
        page(readOnly("getUser"), readOnly("admin.tools.list"), readOnly(long), readOnly("ok_1"))
      )
      offered(p).map(_._2) ==> Vector("github_ok_1")
      p.skipped ==> Vector(
        Skipped.BadName(
          "getUser",
          "a tool name is a lowercase letter, then lowercase letters, digits or _: github_getUser"
        ),
        Skipped.BadName(
          "admin.tools.list",
          "a tool name is a lowercase letter, then lowercase letters, digits or _: github_admin.tools.list"
        ),
        Skipped.BadName(long, s"a tool name is at most 64 characters: github_$long")
      )
    }

    test("a tool mirroring any argument into a header is skipped, however deep the annotation") {
      // The spec's x-mcp-header example (streamable-http.mdx, Schema Extension), marked read-only.
      val executeSql = readOnly(
        "execute_sql",
        "inputSchema" -> ujson.Obj(
          "type" -> "object",
          "properties" -> ujson.Obj(
            "region" -> ujson.Obj(
              "type" -> "string",
              "description" -> "The region to execute the query in",
              "x-mcp-header" -> "Region"
            ),
            "query" -> ujson.Obj("type" -> "string", "description" -> "The SQL query to execute")
          ),
          "required" -> ujson.Arr("region", "query")
        )
      )
      val nested = readOnly(
        "nested",
        "inputSchema" -> ujson.Obj(
          "type" -> "object",
          "properties" -> ujson.Obj(
            "where" -> ujson.Obj(
              "type" -> "array",
              "items" -> ujson.Obj("type" -> "string", "x-mcp-header" -> "Where")
            )
          )
        )
      )
      val p = read(page(executeSql, nested, readOnly("plain")))
      offered(p).map(_._1) ==> Vector("plain")
      p.skipped ==> Vector(
        Skipped.HasHeaderParams("execute_sql"),
        Skipped.HasHeaderParams("nested")
      )
    }

    test(
      "the tool's own title comes before its annotations'; either alone, or the description alone"
    ) {
      val p = read(
        page(
          readOnly(
            "both",
            "title" -> "Own",
            "annotations" -> ujson.Obj("readOnlyHint" -> true, "title" -> "Hint")
          ),
          readOnly("untitled"),
          ujson.Obj(
            "name" -> "undescribed",
            "title" -> "Only a title",
            "inputSchema" -> ujson.Obj("type" -> "object"),
            "annotations" -> ujson.Obj("readOnlyHint" -> true)
          )
        )
      )
      offered(p).map(_._3) ==> Vector("Own: D.", "D.", "Only a title")
    }

    test("an entry that is not a tool is skipped, saying why, and the rest offered") {
      val p = read(
        page(
          ujson.Str("get_me"),
          ujson.Obj("description" -> "D.", "inputSchema" -> ujson.Obj()),
          readOnly("no_schema", "inputSchema" -> ujson.Null),
          readOnly("odd_description", "description" -> 3),
          readOnly("fine")
        )
      )
      offered(p).map(_._1) ==> Vector("fine")
      p.skipped ==> Vector(
        Skipped.Malformed("a listed tool is not an object"),
        Skipped.Malformed("a listed tool has no name"),
        Skipped.Malformed("no_schema's inputSchema is not an object"),
        Skipped.Malformed("odd_description's description is not a string")
      )
    }

    test("the next cursor: absent ends the list, and an empty string is a cursor") {
      // server/utilities/pagination.mdx: a missing nextCursor ends the list.
      read(page()).next ==> None
      read(ujson.Obj("tools" -> ujson.Arr(), "nextCursor" -> "")).next ==> Some("")
      read(ujson.Obj("tools" -> ujson.Arr(), "nextCursor" -> "abc")).next ==> Some("abc")
    }

    test("ttlMs is kept as listed, and absent or negative reads as 0") {
      // server/utilities/caching.mdx, Time-to-Live.
      read(ujson.Obj("tools" -> ujson.Arr(), "ttlMs" -> 300000)).ttlMs ==> 300000L
      read(ujson.Obj("tools" -> ujson.Arr(), "ttlMs" -> -5)).ttlMs ==> 0L
      read(page()).ttlMs ==> 0L
    }

    test("a page with no tools array, or a cursor or ttlMs of the wrong type, is unreadable") {
      Vector(
        ujson.Obj("resultType" -> "complete"),
        ujson.Obj("tools" -> ujson.Arr(), "nextCursor" -> 3),
        ujson.Obj("tools" -> ujson.Arr(), "ttlMs" -> "soon"),
        ujson.Obj("tools" -> ujson.Arr(), "ttlMs" -> 1.5)
      ).map(McpTool.page(_, "github")) ==> Vector(
        Left(McpError.Unreadable("a tools/list result has no tools array")),
        Left(McpError.Unreadable("a tools/list result's nextCursor is not a string")),
        Left(McpError.Unreadable("a tools/list result's ttlMs is not a whole number")),
        Left(McpError.Unreadable("a tools/list result's ttlMs is not a whole number"))
      )
    }

    test("each skip's log line names the tool and why it is not offered") {
      Vector(
        Skipped.NotReadOnly("issue_write"),
        Skipped.BadName(
          "getUser",
          "a tool name is a lowercase letter, then lowercase letters, digits or _: github_getUser"
        ),
        Skipped.HasHeaderParams("execute_sql"),
        Skipped.Malformed("a listed tool has no name")
      ).map(_.message) ==> Vector(
        "issue_write is not offered: it is not marked read-only",
        "getUser is not offered: a tool name is a lowercase letter, then lowercase letters, digits or _: github_getUser",
        "execute_sql is not offered: it mirrors an argument into a header (x-mcp-header)",
        "a listed tool is not offered: a listed tool has no name"
      )
    }
  }
}
