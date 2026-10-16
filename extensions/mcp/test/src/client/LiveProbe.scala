package grit.mcp.client

import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Path}

import grit.core.edge.Variable
import grit.mcp.scope.McpScope
import grit.mcp.wire.{Headers, McpError, McpTool, Rpc, Sse}

/** [[McpServerContract]] and [[grit.mcp.github.GitHubItemsContract]] against GitHub's MCP
  * server, read-only (no model call, no spend):
  *
  * {{{
  * GITHUB_MCP_TOKEN=… ./mill grit.mcp.test.runMain grit.mcp.client.LiveProbe \
  *   <owner> <repo> <issue> <pull> [dir]
  * }}}
  *
  * The token comes from `GITHUB_MCP_TOKEN` alone and is never printed. `<owner>/<repo>` is the
  * repository whose `README.md` the read call fetches, `<issue>` an issue of it with a comment
  * and `<pull>` a pull request with a review. With `dir`, the answers to the first
  * `tools/list` page and to that call are written there as they came, each body under a name
  * saying its type, to be kept as fixtures; neither holds the token. Exits 1 when a contract
  * test fails or the token is not set.
  */
object LiveProbe {

  /** GitHub's hosted endpoint, which by its docs lists only its read-only tools. */
  val Endpoint = "https://api.githubcopilot.com/mcp/readonly"

  val Token: Variable = Variable("GITHUB_MCP_TOKEN")

  def main(args: Array[String]): Unit = {
    val ((owner, repo, issue, pull), dir) = args.toVector match {
      case Vector(o, r, IsNumber(i), IsNumber(p)) => ((o, r, i, p), None)
      case Vector(o, r, IsNumber(i), IsNumber(p), d) => ((o, r, i, p), Some(Path.of(d)))
      case _ =>
        System.err.println("usage: LiveProbe <owner> <repo> <issue> <pull> [dir]")
        sys.exit(2)
    }
    val probe = for {
      server <- McpServer.of("github", Endpoint, Credential.static(Token), McpScope.Open)
      bearer <- Bearer.of(sys.env, Token).left.map(_.message)
      authorizer <- Credential.open(server.credential, sys.env).left.map(_.message)
    } yield (server, bearer, authorizer)
    probe match {
      case Left(why) =>
        System.err.println(why)
        sys.exit(1)
      case Right((server, bearer, authorizer)) =>
        val arguments = ujson.Obj("owner" -> owner, "repo" -> repo, "path" -> "README.md")
        dir.foreach(capture(server, bearer, arguments, _))
        val items = new GitHubItems(server, bearer, s"$owner/$repo", issue, pull)
        val leaves = Vector(
          utest.TestRunner
            .runAndPrint(new GitHubContract(server, authorizer, arguments).tests, "LiveProbe"),
          utest.TestRunner.runAndPrint(items.tests, "LiveProbe items")
        ).flatMap(_.leaves.toVector)
        val failed = leaves.count(_.value.isFailure)
        println(s"LiveProbe: ${leaves.size - failed} passed, $failed failed")
        if (failed > 0) sys.exit(1)
    }
  }

  /** The contract against GitHub's server, its read call `get_file_contents` with `arguments`. */
  private final class GitHubContract(s: McpServer, a: Authorizer, arguments: ujson.Obj)
      extends McpServerContract {
    protected def server: McpServer = s
    protected def authorizer: Authorizer = a
    protected def read: (String, ujson.Obj) = ("get_file_contents", arguments)
    // A fine-grained PAT's shape, so GitHub refuses it as unknown (401), not as malformed (400).
    protected def unknownToken: String = "github_pat_" + "0" * 22 + "_" + "0" * 59
  }

  /** A whole number, as an argument. */
  private object IsNumber {
    def unapply(arg: String): Option[Int] = arg.toIntOption
  }

  /** The items contract against GitHub's server, each call made as its first `tools/list` page
    * lists the tool.
    */
  private final class GitHubItems(s: McpServer, b: Bearer, r: String, i: Int, p: Int)
      extends grit.mcp.github.GitHubItemsContract {
    protected def repository: String = r
    protected def issue: Int = i
    protected def pull: Int = p
    private val http = HttpClient.newHttpClient()
    private lazy val listed: Vector[McpTool] =
      read(http, s, b, 1, Rpc.Call.ListTools(None))
        .flatMap(McpTool.page(_, s.name))
        .fold(e => throw new java.lang.AssertionError(e.message), _.tools)
    protected def answer(tool: String, arguments: ujson.Obj): ujson.Obj =
      listed.find(_.name == tool) match {
        case None => throw new java.lang.AssertionError(s"$tool is not on the first page")
        case Some(t) =>
          read(http, s, b, 2, Rpc.Call.CallTool(t, arguments))
            .fold(e => throw new java.lang.AssertionError(e.message), identity)
      }
  }

  /** The result `server` answers request `id` making `call`. */
  private def read(
      http: HttpClient,
      server: McpServer,
      bearer: Bearer,
      id: Long,
      call: Rpc.Call
  ): Either[McpError, ujson.Obj] = {
    val response = sent(http, server, bearer, id, call)
    val message =
      if (response.headers.firstValue("Content-Type").orElse("").startsWith("text/event-stream"))
        Sse.response(response.body.linesIterator, id)
      else
        scala.util.Try(ujson.read(response.body)).toOption.toRight(McpError.Unreadable("not JSON"))
    message.flatMap(Rpc.result(id, _))
  }

  /** Request `id` making `call`, sent to `server` as its headers say, with `bearer`. */
  private[mcp] def sent(
      http: HttpClient,
      server: McpServer,
      bearer: Bearer,
      id: Long,
      call: Rpc.Call
  ): HttpResponse[String] =
    http.send(
      Headers
        .of(call)
        .foldLeft(HttpRequest.newBuilder(server.endpoint)) { case (r, (k, v)) => r.header(k, v) }
        .header("Authorization", s"Bearer ${bearer.value}")
        .POST(HttpRequest.BodyPublishers.ofString(ujson.write(Rpc.request(id, call))))
        .build(),
      HttpResponse.BodyHandlers.ofString()
    )

  /** Writes the answers to the first `tools/list` page and to `get_file_contents` with
    * `arguments` into `dir`, as they came; the call is made as that page lists the tool, so
    * with its `Mcp-Param-*` headers, and is not made when the page does not offer it.
    */
  private def capture(server: McpServer, bearer: Bearer, arguments: ujson.Obj, dir: Path): Unit = {
    Files.createDirectories(dir)
    val http = HttpClient.newHttpClient()
    def post(id: Long, call: Rpc.Call, name: String): (Boolean, String) = {
      val response = sent(http, server, bearer, id, call)
      val kind = response.headers.firstValue("Content-Type").orElse("none")
      val streamed = kind.startsWith("text/event-stream")
      val file = dir.resolve(s"$name.${response.statusCode}.${if (streamed) "sse" else "json"}")
      Files.writeString(file, response.body)
      println(s"wrote $file ($kind)")
      (streamed, response.body)
    }
    val (streamed, listed) = post(1, Rpc.Call.ListTools(None), "tools-list")
    val message =
      if (streamed) Sse.response(listed.linesIterator, 1)
      else scala.util.Try(ujson.read(listed)).toOption.toRight(McpError.Unreadable("not JSON"))
    message.flatMap(Rpc.result(1, _)).flatMap(McpTool.page(_, server.name)) match {
      case Right(page) =>
        page.tools.find(_.name == "get_file_contents") match {
          case Some(tool) =>
            val _ = post(2, Rpc.Call.CallTool(tool, arguments), "get-file-contents")
          case None => println("get_file_contents is not offered on the first page; not called")
        }
      case Left(e) => println(s"the first page is not read: ${e.message}")
    }
  }
}
