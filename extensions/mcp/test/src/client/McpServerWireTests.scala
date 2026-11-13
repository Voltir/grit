package grit.mcp.client

import grit.core.edge.Variable
import grit.core.provider.ToolSchema
import grit.edge.http.{Listen, Listener, Request, Response}
import grit.mcp.scope.McpScope
import grit.mcp.wire.{CallResult, Listing, McpTool, Posted, Rpc, ServerInfo}

/** [[McpServerContract]], kept by grit's own server's wire over a [[Listener]] on a loopback
  * port: each request read by [[Rpc.received]], one read-only tool listed by
  * [[McpTool.described]] on one page, and a call of it answered with a text naming it and its
  * arguments. grit's client is what asks it.
  */
object McpServerWireTests extends McpServerContract {

  private val tool = ToolSchema(
    "read",
    "Reads a file.",
    ujson.Obj(
      "type" -> "object",
      "properties" -> ujson.Obj("path" -> ujson.Obj("type" -> "string")),
      "required" -> ujson.Arr("path")
    )
  )

  private val listed: Vector[ujson.Obj] =
    McpTool.described(tool, reads = true).toOption.toVector

  private def answer(request: Request): Response =
    Rpc.received(request, ServerInfo("wire", "1", None)) match {
      case Posted.Answered(response) => response
      case list: Posted.List =>
        list.answered(
          if (list.cursor.isEmpty) Listing.Tools(listed, None) else Listing.NoSuchCursor
        )
      case call: Posted.Call =>
        call.answered(
          if (call.tool == tool.name)
            CallResult.Done(s"called ${call.tool} with ${ujson.write(call.arguments)}")
          else CallResult.NoSuchTool
        )
    }

  // Test scaffolding: the listener answers with `answer`, a function of this object's pure
  // values, and logs nowhere; the one effect reached through it is `close`, in utestAfterAll,
  // once every test has run.
  @caps.unsafe.untrackedCaptures
  private val listener: Listener^ =
    Listen
      .parse("127.0.0.1:0")
      .flatMap(at =>
        Listener.open(at, (request, _) => answer(request), _ => ()).left.map(_.message)
      ) match {
      case Right(l) => l
      case Left(why) => throw new java.lang.AssertionError(why)
    }

  protected val server: McpServer[McpTransport.Http] =
    McpServer.of(
      "wire",
      s"http://${listener.at.shown}/mcp",
      Credential.static(Variable("WIRE_MCP_TOKEN")),
      McpScope.Open
    ) match {
      case Right(s) => s
      case Left(why) => throw new java.lang.AssertionError(why)
    }

  protected val authorizer: Authorizer =
    Credential.open(
      server.transport.credential,
      Map("WIRE_MCP_TOKEN" -> "unread"),
      KeyFiles.Local
    ) match {
      case Right(a) => a
      case Left(why) => throw new java.lang.AssertionError(why.message)
    }

  protected val read: (String, ujson.Obj) = ("read", ujson.Obj("path" -> "README.md"))

  override def utestAfterAll(): Unit = listener.close()
}
