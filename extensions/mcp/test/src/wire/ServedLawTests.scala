package grit.mcp.wire

import grit.core.message.Cut
import grit.core.provider.ToolSchema
import grit.edge.http.{Request, Response, Verb}

import utest.*

/** The law tying the wire's two halves: what grit's client sends, [[Rpc.received]] reads as
  * sent, and what [[Posted.List.answered]] and [[Posted.Call.answered]] write, the client reads
  * as the server meant it. [[ServedTests]] holds the server to literal wire text; this holds the
  * halves to each other.
  */
object ServedLawTests extends TestSuite {

  private val seed = 20261113L
  private val info = ServerInfo("example", "1", None)

  /** What grit's client sends as request `id` making `call`, its body changed by `edit` and its
    * header declaring `version`, read by the server.
    */
  private def sent(
      id: Long,
      call: Rpc.Call,
      edit: ujson.Obj => Unit = _ => (),
      version: String = Rpc.Version
  ): Posted = {
    val body = Rpc.request(id, call)
    edit(body)
    val headers =
      Headers.of(call).map((k, v) => if (k == "MCP-Protocol-Version") (k, version) else (k, v))
    Request.of(Verb.Post, "/mcp", headers, ujson.write(body)) match {
      case Right(r) => Rpc.received(r, info)
      case Left(why) => throw new java.lang.AssertionError(why)
    }
  }

  /** `response` as grit's client reads the answer to request `id`. */
  private def read(id: Long, response: Response): Either[McpError, ujson.Obj] =
    if (response.status.code == 200) Rpc.result(id, ujson.read(response.body))
    else Left(Rpc.failure(response.status.code, response.body, None))

  /** Characters a name, a description or a text may hold: ASCII, spaces, a quote, a newline,
    * and non-ASCII within and beyond the BMP.
    */
  private val alphabet: Vector[String] =
    Vector("a", "Z", "0", " ", "\"", "\n", "é", "世", "😀", "=", "?")

  private def text(random: scala.util.Random^, most: Int): String =
    Vector.fill(random.nextInt(most + 1))(alphabet(random.nextInt(alphabet.size))).mkString

  /** A tool grit serves: its schema, and whether it only reads. Every 10th description is past
    * [[McpTool.StoredChars]], which the client cuts.
    */
  private def tool(random: scala.util.Random^, i: Int): (ToolSchema, Boolean) = {
    val description =
      if (random.nextInt(10) == 0) "word " * (McpTool.StoredChars / 4) else text(random, 30)
    val parameters = ujson.Obj(
      "type" -> "object",
      "properties" -> ujson.Obj.from(
        (0 until random.nextInt(3)).map(p =>
          s"arg$p" -> ujson.Obj("type" -> "string", "description" -> text(random, 10))
        )
      )
    )
    (
      ToolSchema(s"tool_${i}_${random.nextInt(1000)}", description, parameters),
      random.nextBoolean()
    )
  }

  private def cursor(random: scala.util.Random^): Option[String] = random.nextInt(3) match {
    case 0 => None
    case 1 => Some("")
    case _ => Some(text(random, 12))
  }

  private def result(random: scala.util.Random^): CallResult = random.nextInt(4) match {
    case 0 => CallResult.Done(s"t:${text(random, 20)}")
    case 1 => CallResult.Failed(s"t:${text(random, 20)}")
    case 2 => CallResult.BadArguments(s"t:${text(random, 20)}")
    case _ => CallResult.NoSuchTool
  }

  /** Why one round of a list and a call broke the law, if it did. */
  private def round(random: scala.util.Random^, r: Int): Vector[String] = {
    val tools = Vector.tabulate(random.nextInt(5))(i => tool(random, r * 10 + i))
    val described = tools.flatMap((schema, reads) => McpTool.described(schema, reads).toOption)
    val reading = tools.collect { case (schema, true) => schema }
    val next = cursor(random)
    val asked = cursor(random)
    val listId = random.nextLong(9007199254740991L)
    val listing = sent(listId, Rpc.Call.ListTools(asked)) match {
      case l @ Posted.List(c, id, Revision.Jul2026)
          if c == asked && id == ujson.Num(listId.toDouble) =>
        Right(l)
      case other => Left(s"list $listId from $asked read as $other")
    }
    val page = listing.flatMap(l =>
      read(listId, l.answered(Listing.Tools(described, next)))
        .flatMap(McpTool.page(_, "p"))
        .left
        .map(e => s"list read back as $e")
    )
    val listed = page.flatMap { p =>
      val want = (
        reading.map(s => (s.name, Cut.head(s.description, McpTool.StoredChars), s.parameters)),
        tools.collect { case (schema, false) => Skipped.NotReadOnly(schema.name) },
        next,
        0L
      )
      val got = (p.tools.map(t => (t.name, t.does, t.inputSchema)), p.skipped, p.next, p.ttlMs)
      Either.cond(got == want, p, s"listed $got, not $want")
    }
    val called = listed.flatMap(_.tools.headOption.toRight("")).flatMap { t =>
      val arguments = ujson.Obj.from(
        (0 until random.nextInt(3)).map(a =>
          s"arg$a" -> (if (random.nextBoolean()) ujson.Str(text(random, 8))
                       else ujson.Num(random.nextInt(100).toDouble))
        )
      )
      val callId = random.nextLong(9007199254740991L)
      val answer = result(random)
      sent(callId, Rpc.Call.CallTool(t, arguments)) match {
        case c @ Posted.Call(name, args, id, Revision.Jul2026)
            if name == t.name && args == arguments && id == ujson.Num(callId.toDouble) =>
          val got =
            read(callId, c.answered(answer)).flatMap(Answer.of(_)).map(a => (a.text, a.isError))
          val want = answer match {
            case CallResult.Done(said) => Right((said, false))
            case CallResult.Failed(said) => Right((said, true))
            case CallResult.BadArguments(why) => Right((why, true))
            case CallResult.NoSuchTool => Left(McpError.Rpc(-32602, s"no tool is named ${t.name}"))
          }
          Either.cond(got == want, (), s"$answer read back as $got")
        case other => Left(s"call $callId of ${t.name} read as $other")
      }
    }
    Vector(listing, page, listed, called).collect { case Left(why) if why.nonEmpty => why }.take(1)
  }

  val tests = Tests {
    test("what the client sends is read as sent, and what the server answers read as meant") {
      val random = new scala.util.Random(seed)
      (seed, (0 until 200).flatMap(round(random, _)).take(3).toVector) ==> (seed, Vector())
    }

    test("the server's refusals are read by the client as the errors they are") {
      val list = Rpc.Call.ListTools(None)
      val version = "io.modelcontextprotocol/protocolVersion"
      def refusal(asked: Posted) = asked match {
        case Posted.Answered(r) => read(1, r)
        case other => Right(ujson.Obj("asked" -> other.toString))
      }
      Vector(
        refusal(sent(1, list, _("params")("_meta")(version) = "2099-01-01", "2099-01-01")),
        refusal(sent(1, list, _("method") = "resources/list")),
        refusal(sent(1, list, _("params")("_meta").obj.remove(version))),
        read(1, Posted.List(None, ujson.Num(1), Revision.Jul2026).answered(Listing.NoSuchCursor))
      ) ==> Vector(
        Left(McpError.Unsupported(Vector("2026-07-28", "2025-11-25", "2025-06-18"))),
        Left(McpError.Rpc(-32020, "Mcp-Method does not match the method")),
        Left(McpError.Rpc(-32602, s"_meta lacks $version")),
        Left(McpError.Rpc(-32602, "no page of tools has that cursor"))
      )
    }
  }
}
