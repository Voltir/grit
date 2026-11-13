package grit.mcp.wire

import grit.edge.http.{Request, Response, Status, Verb}

import utest.*

/** [[Rpc.received]]: a POST to an MCP endpoint read as a server reads it, in either era. The
  * requests are wire text as written, not built by grit's own client.
  */
object ServedTests extends TestSuite {

  private val info = ServerInfo("example", "1", Some("Example instructions."))

  private def received(verb: Verb, headers: Vector[(String, String)], body: String): Asked =
    Request.of(verb, "/mcp", headers, body) match {
      case Right(r) => Rpc.received(r, info)
      case Left(why) => throw new java.lang.AssertionError(why)
    }

  private def post(headers: (String, String)*)(body: String): Asked =
    received(Verb.Post, headers.toVector, body)

  /** An answer's status and its body as JSON, `ujson.Null` when it has none; or what was asked. */
  private def answered(asked: Asked): Either[Asked, (Int, ujson.Value)] = asked match {
    case Asked.Answered(r) =>
      Right((r.status.code, if (r.body.isEmpty) ujson.Null else ujson.read(r.body)))
    case other => Left(other)
  }

  /** An answer's status, error code and id; or what was asked. */
  private def refused(asked: Asked): Either[Asked, (Int, Option[Int], ujson.Value)] =
    answered(asked).map { (status, body) =>
      val o = body.objOpt
      (
        status,
        o.flatMap(_.get("error")).flatMap(_.objOpt).flatMap(_.get("code")).map(_.num.toInt),
        o.flatMap(_.get("id")).getOrElse(ujson.Null)
      )
    }

  /** The headers of a stateless request with `method`. */
  private def stateless(method: String, more: (String, String)*): Vector[(String, String)] =
    Vector("MCP-Protocol-Version" -> "2026-07-28", "Mcp-Method" -> method) ++ more

  private val meta =
    """"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28",""" +
      """"io.modelcontextprotocol/clientCapabilities":{}}"""

  // Claude Code 2.1.303's own server/discover, as it sent it.
  private val discover =
    """{"jsonrpc":"2.0","id":"server-discover-probe-1","method":"server/discover","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientInfo":{"name":"claude-code","title":"Claude Code","version":"2.1.303","description":"Anthropic's agentic coding tool","websiteUrl":"https://claude.com/claude-code"},"io.modelcontextprotocol/clientCapabilities":{"roots":{"listChanged":true},"elicitation":{"form":{},"url":{}}}}}}"""

  val tests = Tests {
    test("a body that is not one JSON-RPC 2.0 request is 400, -32700 or -32600, its id when read") {
      Vector(
        "{",
        """[{"jsonrpc":"2.0","id":1,"method":"ping"}]""",
        "5",
        """{"id":3,"method":"ping"}""",
        """{"jsonrpc":"1.0","id":"a","method":"ping"}""",
        """{"jsonrpc":"2.0","id":null,"method":"ping"}""",
        """{"jsonrpc":"2.0","id":1.5,"method":"ping"}""",
        """{"jsonrpc":"2.0","id":9007199254740992,"method":"ping"}""",
        """{"jsonrpc":"2.0","id":true,"method":"ping"}""",
        """{"jsonrpc":"2.0","id":4,"method":7}""",
        """{"jsonrpc":"2.0","id":5,"method":"tools/list","params":[]}""",
        """{"jsonrpc":"2.0","id":6}""",
        """{"jsonrpc":"2.0","method":"tools/list"}"""
      ).map(b => refused(post()(b))) ==> Vector(
        Right((400, Some(-32700), ujson.Null)),
        Right((400, Some(-32600), ujson.Null)),
        Right((400, Some(-32600), ujson.Null)),
        Right((400, Some(-32600), ujson.Num(3))),
        Right((400, Some(-32600), ujson.Str("a"))),
        Right((400, Some(-32600), ujson.Null)),
        Right((400, Some(-32600), ujson.Null)),
        Right((400, Some(-32600), ujson.Null)),
        Right((400, Some(-32600), ujson.Null)),
        Right((400, Some(-32600), ujson.Num(4))),
        Right((400, Some(-32600), ujson.Num(5))),
        Right((400, Some(-32600), ujson.Num(6))),
        Right((400, Some(-32600), ujson.Null))
      )
    }

    test("a notification or a client's response is 202 with no body, in either era") {
      Vector(
        post()("""{"jsonrpc":"2.0","method":"notifications/initialized"}"""),
        post("mcp-protocol-version" -> "2025-11-25")(
          """{"jsonrpc":"2.0","method":"notifications/initialized"}"""
        ),
        post(stateless("notifications/cancelled")*)(
          s"""{"jsonrpc":"2.0","method":"notifications/cancelled","params":{$meta}}"""
        ),
        post()("""{"jsonrpc":"2.0","id":1,"result":{}}"""),
        post()("""{"jsonrpc":"2.0","id":1,"error":{"code":-1,"message":"no"}}""")
      ).map(answered) ==> Vector.fill(5)(Right((202, ujson.Null)))
    }

    test("a verb other than POST is 405, allowing POST alone") {
      Vector(Verb.Get, Verb.Delete).map(v =>
        received(v, Vector("Accept" -> "text/event-stream"), "") match {
          case Asked.Answered(r) => Some(r.status)
          case _ => None
        }
      ) ==> Vector.fill(2)(Some(Status.NotAllowed(Vector(Verb.Post))))
    }

    test("a stateless request is held to its headers and _meta, the first broken check answered") {
      // streamable-http.mdx, Server Validation; basic/index.mdx, _meta. Each row breaks the
      // check it names and every later one, so the order is pinned too.
      val list = s"""{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{$meta}}"""
      val versionOnly = """"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28"}"""
      def call(name: String, meta: String = versionOnly) =
        s"""{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"$name",$meta}}"""
      val noCapabilities =
        """{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{"_meta":""" +
          """{"io.modelcontextprotocol/protocolVersion":"2026-07-28"}}}"""
      val later =
        """{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{"_meta":""" +
          """{"io.modelcontextprotocol/protocolVersion":"2099-01-01",""" +
          """"io.modelcontextprotocol/clientCapabilities":{}}}}"""
      Vector(
        // the version header missing, with nothing else declared but _meta's
        post("Mcp-Method" -> "tools/list")(noCapabilities),
        // _meta without its version, the header declaring 2026-07-28
        post(stateless("tools/list")*)(
          """{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{"_meta":{}}}"""
        ),
        // the header and _meta disagreeing
        post(stateless("tools/list").updated(0, "MCP-Protocol-Version" -> "2025-11-25")*)(list),
        // Mcp-Method missing, then naming another method
        post("MCP-Protocol-Version" -> "2026-07-28")(noCapabilities),
        post(stateless("tools/call")*)(noCapabilities),
        // Mcp-Name naming another tool, and missing
        post(stateless("tools/call", "Mcp-Name" -> "other")*)(call("read")),
        post(stateless("tools/call")*)(call("read")),
        // clientCapabilities missing
        post(stateless("tools/list")*)(noCapabilities),
        // a revision not listed, header and _meta agreeing
        post(stateless("tools/list").updated(0, "MCP-Protocol-Version" -> "2099-01-01")*)(later),
        // methods 2026-07-28 does not have here
        post(stateless("initialize")*)(
          s"""{"jsonrpc":"2.0","id":1,"method":"initialize","params":{$meta}}"""
        ),
        post(stateless("ping")*)(s"""{"jsonrpc":"2.0","id":1,"method":"ping","params":{$meta}}"""),
        post(stateless("subscriptions/listen")*)(
          s"""{"jsonrpc":"2.0","id":1,"method":"subscriptions/listen","params":{$meta}}"""
        )
      ).map(refused) ==> Vector(
        Right((400, Some(-32020), ujson.Num(1))),
        Right((400, Some(-32602), ujson.Num(1))),
        Right((400, Some(-32020), ujson.Num(1))),
        Right((400, Some(-32020), ujson.Num(1))),
        Right((400, Some(-32020), ujson.Num(1))),
        Right((400, Some(-32020), ujson.Num(1))),
        Right((400, Some(-32020), ujson.Num(1))),
        Right((400, Some(-32602), ujson.Num(1))),
        Right((400, Some(-32022), ujson.Num(1))),
        Right((404, Some(-32601), ujson.Num(1))),
        Right((404, Some(-32601), ujson.Num(1))),
        Right((404, Some(-32601), ujson.Num(1)))
      )
    }

    test("an unlisted revision's error names every listed one, newest first, and the one asked") {
      // basic/versioning.mdx: -32022's data statelessly; the older era's 400 carries the same.
      // Whether the older revisions belong in the list is open; this pins that they are in it.
      val later =
        """{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{"_meta":""" +
          """{"io.modelcontextprotocol/protocolVersion":"2099-01-01",""" +
          """"io.modelcontextprotocol/clientCapabilities":{}}}}"""
      Vector(
        post("MCP-Protocol-Version" -> "2099-01-01", "Mcp-Method" -> "tools/list")(later),
        post("MCP-Protocol-Version" -> "2025-03-26")(
          """{"jsonrpc":"2.0","id":1,"method":"tools/list"}"""
        )
      ).map(answered(_).map((s, b) => (s, b("error")("code"), b("error")("data")))) ==> Vector(
        Right(
          (
            400,
            ujson.Num(-32022),
            ujson.Obj(
              "supported" -> ujson.Arr("2026-07-28", "2025-11-25", "2025-06-18"),
              "requested" -> "2099-01-01"
            )
          )
        ),
        Right(
          (
            400,
            ujson.Num(-32602),
            ujson.Obj(
              "supported" -> ujson.Arr("2026-07-28", "2025-11-25", "2025-06-18"),
              "requested" -> "2025-03-26"
            )
          )
        )
      )
    }

    test("server/discover is answered from the server's info, each field as the schema names it") {
      // schema 2026-07-28, DiscoverResult; the server's name and version in _meta.
      answered(
        post("mcp-protocol-version" -> "2026-07-28", "mcp-method" -> "server/discover")(
          discover
        )
      ) ==> Right(
        (
          200,
          ujson.read(
            """{"jsonrpc":"2.0","id":"server-discover-probe-1","result":{"resultType":"complete",""" +
              """"supportedVersions":["2026-07-28","2025-11-25","2025-06-18"],""" +
              """"capabilities":{"tools":{"listChanged":false}},""" +
              """"instructions":"Example instructions.","ttlMs":0,"cacheScope":"private",""" +
              """"_meta":{"io.modelcontextprotocol/serverInfo":{"name":"example","version":"1"}}}}"""
          )
        )
      )
    }

    test("initialize answers the revision asked when it is listed and has one, else 2025-11-25") {
      def initialize(asked: Option[String]) = {
        val version = asked.fold("")(v => s""""protocolVersion":"$v",""")
        post()(
          s"""{"jsonrpc":"2.0","id":0,"method":"initialize","params":{$version"capabilities":{},""" +
            """"clientInfo":{"name":"c","version":"1"}}}"""
        )
      }
      val versions =
        Vector(Some("2025-06-18"), Some("2025-11-25"), Some("2025-03-26"), Some("2026-07-28"), None)
          .map(v => answered(initialize(v)).map(_._2("result")("protocolVersion")))
      (
        versions,
        answered(initialize(Some("2025-11-25")))
      ) ==> (
        Vector("2025-06-18", "2025-11-25", "2025-11-25", "2025-11-25", "2025-11-25")
          .map(v => Right(ujson.Str(v))),
        Right(
          (
            200,
            ujson.read(
              """{"jsonrpc":"2.0","id":0,"result":{"protocolVersion":"2025-11-25",""" +
                """"capabilities":{"tools":{"listChanged":false}},""" +
                """"serverInfo":{"name":"example","version":"1"},""" +
                """"instructions":"Example instructions."}}"""
            )
          )
        )
      )
    }

    test("before 2026-07-28, ping is an empty result and an unknown method -32601 at 200") {
      Vector(
        post()("""{"jsonrpc":"2.0","id":"p","method":"ping"}"""),
        post("mcp-protocol-version" -> "2025-11-25")(
          """{"jsonrpc":"2.0","id":2,"method":"server/discover"}"""
        )
      ).map(answered) ==> Vector(
        Right((200, ujson.read("""{"jsonrpc":"2.0","id":"p","result":{}}"""))),
        Right(
          (
            200,
            ujson.read(
              """{"jsonrpc":"2.0","id":2,"error":{"code":-32601,""" +
                """"message":"no method is named server/discover"}}"""
            )
          )
        )
      )
    }

    test("tools/list and tools/call reach the server with the id as sent, in the revision read") {
      def list(id: String, params: String = meta) =
        s"""{"jsonrpc":"2.0","id":$id,"method":"tools/list","params":{$params}}"""
      Vector(
        post(stateless("tools/list")*)(list("\"é-1\"")),
        post(stateless("tools/list")*)(list("\"\"", s""""cursor":"",$meta""")),
        post(stateless("tools/list")*)(list("0", s""""cursor":"page 2",$meta""")),
        post(stateless("tools/list")*)(list("-5")),
        post(stateless("tools/list")*)(list("9007199254740991")),
        // header names in lower case, and an Mcp-Param-* no tool declared, not read
        post(
          "mcp-protocol-version" -> "2026-07-28",
          "mcp-method" -> "tools/call",
          "mcp-name" -> "=?base64?cmVhZA==?=",
          "Mcp-Param-Region" -> "anything"
        )(
          s"""{"jsonrpc":"2.0","id":7,"method":"tools/call","params":{"name":"read",""" +
            s""""arguments":{"q":1},"_meta":{"progressToken":3,"other/key":true,""" +
            """"io.modelcontextprotocol/protocolVersion":"2026-07-28",""" +
            """"io.modelcontextprotocol/clientCapabilities":{}}}}"""
        ),
        // Claude Code 2.1.303's own tools/list and tools/call after initialize
        post("mcp-protocol-version" -> "2025-11-25")(
          """{"method":"tools/list","jsonrpc":"2.0","id":1}"""
        ),
        post("mcp-protocol-version" -> "2025-11-25")(
          """{"method":"tools/call","params":{"name":"example_read","arguments":""" +
            """{"topic":"example"},"_meta":{"claudecode/toolUseId":"toolu_example",""" +
            """"progressToken":2}},"jsonrpc":"2.0","id":2}"""
        ),
        // no header and no _meta: 2025-06-18; arguments absent
        post()("""{"jsonrpc":"2.0","id":8,"method":"tools/call","params":{"name":"read"}}""")
      ) ==> Vector(
        Asked.List(None, ujson.Str("é-1"), Revision.Jul2026),
        Asked.List(Some(""), ujson.Str(""), Revision.Jul2026),
        Asked.List(Some("page 2"), ujson.Num(0), Revision.Jul2026),
        Asked.List(None, ujson.Num(-5), Revision.Jul2026),
        Asked.List(None, ujson.Num(9007199254740991d), Revision.Jul2026),
        Asked.Call("read", ujson.Obj("q" -> 1), ujson.Num(7), Revision.Jul2026),
        Asked.List(None, ujson.Num(1), Revision.Nov2025),
        Asked.Call(
          "example_read",
          ujson.Obj("topic" -> "example"),
          ujson.Num(2),
          Revision.Nov2025
        ),
        Asked.Call("read", ujson.Obj(), ujson.Num(8), Revision.Jun2025)
      )
    }

    test("params tools/list or tools/call cannot read are -32602: 400 statelessly, 200 before") {
      def stateless(method: String, params: String, name: Option[String] = None) =
        post(this.stateless(method, name.map("Mcp-Name" -> _).toSeq*)*)(
          s"""{"jsonrpc":"2.0","id":1,"method":"$method","params":{$params,$meta}}"""
        )
      def older(method: String, params: String, version: Option[String]) =
        post(version.map("MCP-Protocol-Version" -> _).toSeq*)(
          s"""{"jsonrpc":"2.0","id":1,"method":"$method","params":{$params}}"""
        )
      Vector(
        stateless("tools/list", """"cursor":5"""),
        stateless("tools/call", """"arguments":{}"""),
        stateless("tools/call", """"name":"read","arguments":[]""", Some("read")),
        older("tools/list", """"cursor":5""", Some("2025-06-18")),
        older("tools/call", """"arguments":{}""", Some("2025-11-25")),
        older("tools/call", """"name":"read","arguments":"x"""", None)
      ).map(refused) ==> Vector(
        Right((400, Some(-32602), ujson.Num(1))),
        Right((400, Some(-32602), ujson.Num(1))),
        Right((400, Some(-32602), ujson.Num(1))),
        Right((200, Some(-32602), ujson.Num(1))),
        Right((200, Some(-32602), ujson.Num(1))),
        Right((200, Some(-32602), ujson.Num(1)))
      )
    }
  }
}
