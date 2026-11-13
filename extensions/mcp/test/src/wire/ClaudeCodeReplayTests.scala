package grit.mcp.wire

import grit.core.provider.ToolSchema
import grit.edge.http.{Request, Response, Verb}

import utest.*

/** Claude Code 2.1.303's requests to an HTTP MCP server, replayed through [[Rpc.received]]: the
  * six it sent another server, recorded, and the two stateless ones it is expected to send once
  * `server/discover` is answered, synthesised from that recording (the module's README says
  * how each was cut).
  */
object ClaudeCodeReplayTests extends TestSuite {

  private val info = ServerInfo("example", "1", Some("Example instructions."))

  private def lines(name: String): Vector[ujson.Value] =
    scala.io.Source
      .fromInputStream(getClass.getResourceAsStream(s"/claude-code/$name"), "UTF-8")
      .getLines()
      .filter(_.nonEmpty)
      .map(ujson.read(_))
      .toVector

  /** A recorded or synthesised request, read by the server. */
  private def received(line: ujson.Value): Posted = {
    val verb = line("verb").str match {
      case "POST" => Verb.Post
      case "GET" => Verb.Get
      case other => Verb.Other(other)
    }
    val headers = line("headers").obj.toVector.map((k, v) => (k, v.str))
    Request.of(verb, line("path").str, headers, line("body").str) match {
      case Right(r) => Rpc.received(r, info)
      case Left(why) => throw new java.lang.AssertionError(why)
    }
  }

  /** `v` reduced to its shape as the fixture keeps an answer: every string empty, every number
    * 0, every list its first item, and the keys of a schema's `properties` numbered.
    */
  private def shape(v: ujson.Value, key: String = ""): ujson.Value = v match {
    case o: ujson.Obj if key == "properties" =>
      ujson.Obj.from(o.value.toVector.zipWithIndex.map { case ((_, p), i) =>
        s"property-${i + 1}" -> shape(p)
      })
    case o: ujson.Obj => ujson.Obj.from(o.value.toVector.map((k, x) => k -> shape(x, k)))
    case a: ujson.Arr => ujson.Arr.from(a.value.headOption.map(shape(_)).toVector)
    case _: ujson.Str => ujson.Str("")
    case _: ujson.Num => ujson.Num(0)
    case other => other
  }

  /** `r`'s status and its body's shape, `""` for a body that is not JSON. */
  private def shaped(r: Response): (Int, ujson.Value) =
    (r.status.code, scala.util.Try(ujson.read(r.body)).toOption.fold(ujson.Str(""))(shape(_)))

  /** The tool the recorded server listed, as grit describes one of its shape. */
  private val tool = ToolSchema(
    "example_read",
    "Reads an example.",
    ujson.Obj(
      "type" -> "object",
      "properties" -> ujson.Obj(
        "topic" -> ujson.Obj("type" -> "string", "description" -> "The topic."),
        "detail" -> ujson.Obj("type" -> "string", "description" -> "How much.")
      ),
      "required" -> ujson.Arr("topic"),
      "additionalProperties" -> false
    )
  )

  /** What the server sends for `asked`: a list answered with [[tool]], a call with a text. */
  private def answered(asked: Posted): Response = asked match {
    case Posted.Answered(r) => r
    case l: Posted.List =>
      l.answered(Listing.Tools(McpTool.described(tool, reads = true).toOption.toVector, None))
    case c: Posted.Call => c.answered(CallResult.Done("an example"))
  }

  val tests = Tests {
    test("the recorded handshake is answered with its statuses and shapes; discover now answered") {
      val recorded = lines("handshake-2.1.303.jsonl")
      val want = recorded.map(l => (l("status").num.toInt, l("answer"))).zipWithIndex.map {
        // server/discover: the recorded server had none (-32601 at 200); grit answers it.
        case (_, 0) =>
          (
            200,
            ujson.read(
              """{"jsonrpc":"","id":"","result":{"resultType":"","supportedVersions":[""],""" +
                """"capabilities":{"tools":{"listChanged":false}},"instructions":"","ttlMs":0,""" +
                """"cacheScope":"","_meta":{"io.modelcontextprotocol/serverInfo":""" +
                """{"name":"","version":""}}}}"""
            )
          )
        // tools/list: the recorded server's tools carried no annotations; grit's say readOnlyHint.
        case ((status, answer), 4) =>
          val listed = ujson.copy(answer)
          listed("result")("tools")(0)("annotations") = ujson.Obj("readOnlyHint" -> true)
          (status, listed)
        case (row, _) => row
      }
      recorded.map(l => shaped(answered(received(l)))) ==> want
    }

    test("the stateless list and call, synthesised from the recorded discover, reach the server") {
      lines("stateless-synthesised-2.1.303.jsonl").map(received) ==> Vector(
        Posted.List(None, ujson.Num(1), Revision.Jul2026),
        Posted.Call("example_read", ujson.Obj("topic" -> "example"), ujson.Num(2), Revision.Jul2026)
      )
    }
  }
}
