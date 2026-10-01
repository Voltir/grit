package grit.mcp.wire

import grit.core.tool.ToolName

import utest.*

/** [[McpTool.page]]: a `tools/list` page read, and only the tools grit may offer kept. */
object McpToolTests extends TestSuite {

  /** GitHub's own `tools/list` entry for `name`, from its source (see grit/mcp/README.md). */
  private def snap(name: String): ujson.Value =
    ujson.read(
      scala.io.Source
        .fromInputStream(getClass.getResourceAsStream(s"/toolsnaps/$name.snap"))
        .mkString
    )

  /** GitHub's hosted read-only server's `tools/list` result, as it answered (see
    * grit/mcp/README.md).
    */
  private val live: ujson.Obj =
    ujson.Obj.from(
      ujson
        .read(
          scala.io.Source
            .fromInputStream(getClass.getResourceAsStream("/github/tools-list.json"))
            .mkString
        )
        .obj
    )

  /** The live list's entry for `name`. */
  private def github(name: String): ujson.Value =
    live("tools").arr
      .find(_.obj.get("name").contains(ujson.Str(name)))
      .getOrElse(throw new java.lang.AssertionError(s"$name is not in the live list"))

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

  /** The spec's x-mcp-header example (streamable-http.mdx, Schema Extension), marked read-only. */
  private val executeSql: ujson.Obj = readOnly(
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

  private def read(result: ujson.Obj): McpTool.Page =
    McpTool.page(result, "github").getOrElse(throw new java.lang.AssertionError(s"unread: $result"))

  private def offered(p: McpTool.Page): Vector[(String, String, String)] =
    p.tools.map(t => (t.name, ToolName.value(t.offered), t.does))

  val tests = Tests {
    test("GitHub's read-only tools are offered under the prefix as listed; its write tool is not") {
      val p = read(page(github("get_file_contents"), snap("issue_write"), github("get_me")))
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
      p.tools.headOption.map(_.inputSchema) ==> github("get_file_contents").obj.get("inputSchema")
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

    test("GitHub's live list is offered whole, each tool mirroring the arguments it annotates") {
      // Every tool of the hosted read-only server, 20 of them with x-mcp-header annotations.
      val p = read(live)
      p.skipped ==> Vector.empty
      p.tools.map(_.name) ==> live("tools").arr.toVector.flatMap(_.obj.get("name")).map(_.str)
      p.tools.find(_.name == "get_file_contents").map(_.params) ==> Some(
        Vector(McpTool.Param(Vector("owner"), "owner"), McpTool.Param(Vector("repo"), "repo"))
      )
      p.tools.find(_.name == "get_me").map(_.params) ==> Some(Vector.empty)
    }

    test("an x-mcp-header annotation is read at its chain of properties keys, however deep") {
      // The spec's example (streamable-http.mdx, Schema Extension), marked read-only, and one
      // nested in an object property, which the spec permits.
      val nested = readOnly(
        "nested",
        "inputSchema" -> ujson.Obj(
          "type" -> "object",
          "properties" -> ujson.Obj(
            "where" -> ujson.Obj(
              "type" -> "object",
              "properties" -> ujson.Obj(
                "zone" -> ujson.Obj("type" -> "string", "x-mcp-header" -> "Zone"),
                "count" -> ujson.Obj("type" -> "integer", "x-mcp-header" -> "Count")
              )
            ),
            "dry" -> ujson.Obj("type" -> "boolean", "x-mcp-header" -> "Dry-Run")
          )
        )
      )
      val p = read(page(executeSql, nested))
      p.tools.map(t => (t.name, t.params)) ==> Vector(
        ("execute_sql", Vector(McpTool.Param(Vector("region"), "Region"))),
        (
          "nested",
          Vector(
            McpTool.Param(Vector("where", "zone"), "Zone"),
            McpTool.Param(Vector("where", "count"), "Count"),
            McpTool.Param(Vector("dry"), "Dry-Run")
          )
        )
      )
      p.skipped ==> Vector.empty
    }

    test("a tool whose x-mcp-header annotations break the spec is skipped, saying why") {
      // streamable-http.mdx, Schema Extension: each constraint broken once.
      def annotated(name: String, schema: ujson.Obj): ujson.Obj =
        readOnly(name, "inputSchema" -> ujson.Obj("type" -> "object", "properties" -> schema))
      def string(header: ujson.Value): ujson.Obj =
        ujson.Obj("type" -> "string", "x-mcp-header" -> header)
      val p = read(
        page(
          annotated(
            "in_items",
            ujson.Obj("where" -> ujson.Obj("type" -> "array", "items" -> string("Where")))
          ),
          annotated(
            "in_any_of",
            ujson.Obj("where" -> ujson.Obj("anyOf" -> ujson.Arr(string("Where"))))
          ),
          readOnly(
            "on_root",
            "inputSchema" -> ujson.Obj("type" -> "string", "x-mcp-header" -> "Root")
          ),
          annotated("empty", ujson.Obj("a" -> string(""))),
          annotated("not_a_token", ujson.Obj("a" -> string("Re gion"))),
          annotated("crlf", ujson.Obj("a" -> string("A\r\nX-Evil: 1"))),
          annotated("not_text", ujson.Obj("a" -> string(3))),
          annotated(
            "a_number",
            ujson.Obj("a" -> ujson.Obj("type" -> "number", "x-mcp-header" -> "A"))
          ),
          annotated("untyped", ujson.Obj("a" -> ujson.Obj("x-mcp-header" -> "A"))),
          annotated("twice", ujson.Obj("a" -> string("Region"), "b" -> string("region"))),
          executeSql
        )
      )
      p.tools.map(_.name) ==> Vector("execute_sql")
      p.skipped ==> Vector(
        Skipped.BadHeader(
          "in_items",
          "the x-mcp-header at /properties/where/items is not on a property reached through properties alone"
        ),
        Skipped.BadHeader(
          "in_any_of",
          "the x-mcp-header at /properties/where/anyOf/0 is not on a property reached through properties alone"
        ),
        Skipped.BadHeader(
          "on_root",
          "the x-mcp-header at / is not on a property reached through properties alone"
        ),
        Skipped.BadHeader("empty", "the x-mcp-header at /properties/a is not a header name: \"\""),
        Skipped.BadHeader(
          "not_a_token",
          "the x-mcp-header at /properties/a is not a header name: \"Re gion\""
        ),
        Skipped.BadHeader(
          "crlf",
          "the x-mcp-header at /properties/a is not a header name: \"A\\r\\nX-Evil: 1\""
        ),
        Skipped.BadHeader("not_text", "the x-mcp-header at /properties/a is not a header name: 3"),
        Skipped.BadHeader(
          "a_number",
          "the x-mcp-header at /properties/a is on a property of type \"number\", not string, integer or boolean"
        ),
        Skipped.BadHeader(
          "untyped",
          "the x-mcp-header at /properties/a is on a property of no type, not string, integer or boolean"
        ),
        Skipped.BadHeader(
          "twice",
          "the x-mcp-header at /properties/b, region, repeats Region, case aside"
        )
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
        Skipped.BadHeader("in_items", "the x-mcp-header at /properties/where/items is misplaced"),
        Skipped.Malformed("a listed tool has no name"),
        Skipped.OutOfScope("search_code")
      ).map(_.message) ==> Vector(
        "issue_write is not offered: it is not marked read-only",
        "getUser is not offered: a tool name is a lowercase letter, then lowercase letters, digits or _: github_getUser",
        "in_items is not offered: its x-mcp-header annotations break the spec: the x-mcp-header at /properties/where/items is misplaced",
        "a listed tool is not offered: a listed tool has no name",
        "search_code is not offered: its server's scope cannot hold it to its bounds"
      )
    }
  }
}
