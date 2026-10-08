package grit.slack.client

import java.net.{InetSocketAddress, URLDecoder}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

/** Slack's Web API for tests, answering each request with what Slack answered the same request
  * when `scripts/slack-capture` recorded it (`slack-api/NNN-<method>.json`). A request is the
  * same when its method and [[SlackStub.Keyed]] params are. The nth such request gets the nth
  * recording, the last repeating. One no recording answers is refused with an error naming it,
  * `unrecorded: <method> <params>`, which [[SocketSlack]] reads as `Refused`. The first
  * `limited` requests are answered with the hand-written rate limit (`rate-limited.json`), and
  * each request whose key (its method, then its keyed params as `name=value`, space-separated)
  * is in `failing` with Slack's `fatal_error`. Listens on a free port of 127.0.0.1 until closed.
  * Four recordings were written by hand from Slack's API docs, not captured: `036` and `037`
  * (`users.info`) and `047` and `048` (`users.list`), checked against a live workspace's
  * answers by shape only.
  */
final class SlackStub(limited: Int, failing: Set[String] = Set.empty) extends AutoCloseable {
  import SlackStub.*

  // Touched only by the server's one dispatcher thread, while the test waits on its call.
  @caps.unsafe.untrackedCaptures
  private var left = limited
  @caps.unsafe.untrackedCaptures
  private var served = Map.empty[String, Int]

  private val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
  server.createContext("/api/", Handler)
  server.start()

  /** The Web API URL [[SocketSlack]] is pointed at. */
  val api: String = s"http://127.0.0.1:${server.getAddress.getPort}/api/"

  def close(): Unit = server.stop(0)

  private object Handler extends HttpHandler {
    def handle(exchange: HttpExchange): Unit = {
      val method = exchange.getRequestURI.getPath.stripPrefix("/api/")
      val body = new String(exchange.getRequestBody.readAllBytes(), UTF_8)
      val params = form(body) ++ Option(exchange.getRequestURI.getRawQuery).fold(Map.empty)(form)
      val answer =
        if (left > 0) {
          left -= 1
          RateLimited
        } else if (failing.contains(keyOf(method, params))) Fatal
        else {
          val key = keyOf(method, params)
          val all = recordings.getOrElse(key, Vector.empty)
          val n = served.getOrElse(key, 0)
          served = served.updated(key, n + 1)
          all.lift(n).orElse(all.lastOption).getOrElse(unrecorded(key))
        }
      val bytes = answer.body.getBytes(UTF_8)
      answer.headers.foreach((k, v) => exchange.getResponseHeaders.add(k, v))
      exchange.sendResponseHeaders(answer.status, bytes.length.toLong)
      val out = exchange.getResponseBody
      try out.write(bytes)
      finally out.close()
    }
  }
}

object SlackStub {

  /** The params that change Slack's answer; `limit`, the token, a post's text, blocks and
    * metadata, and the SDK's other defaults do not pick a recording.
    */
  val Keyed: Vector[String] = Vector(
    "channel",
    "ts",
    "thread_ts",
    "oldest",
    "inclusive",
    "cursor",
    "user",
    "timestamp",
    "message_ts",
    "name",
    "include_all_metadata"
  )

  /** An HTTP answer as recorded. */
  final case class Answer(status: Int, headers: Map[String, String], body: String)

  /** A boolean param as Slack reads it: the SDK sends `1`/`0`, the capture `true`/`false`. */
  private def flag(v: String): String = v match {
    case "true" => "1"
    case "false" => "0"
    case other => other
  }

  private def keyOf(method: String, params: Map[String, String]): String =
    (method +: Keyed.flatMap(k => params.get(k).map(v => s"$k=${flag(v)}"))).mkString(" ")

  private def form(encoded: String): Map[String, String] =
    encoded
      .split('&')
      .toVector
      .filter(_.nonEmpty)
      .map { pair =>
        val (k, v) = pair.span(_ != '=')
        (URLDecoder.decode(k, UTF_8), URLDecoder.decode(v.drop(1), UTF_8))
      }
      .toMap

  private def answer(json: ujson.Value): Answer =
    Answer(
      json("status").num.toInt,
      json("headers").obj.map((k, v) => (k, v.str)).toMap,
      ujson.write(json("body"))
    )

  /** Slack's answer to a request it failed on its side. */
  private val Fatal: Answer =
    Answer(
      200,
      Map("content-type" -> "application/json; charset=utf-8"),
      ujson.write(ujson.Obj("ok" -> false, "error" -> "fatal_error"))
    )

  private def unrecorded(key: String): Answer =
    Answer(
      200,
      Map("content-type" -> "application/json; charset=utf-8"),
      ujson.write(ujson.Obj("ok" -> false, "error" -> s"unrecorded: $key"))
    )

  private val directory: Path = Option(getClass.getResource("/slack-api"))
    .map(u => Paths.get(u.toURI))
    .getOrElse(throw new java.lang.AssertionError("the slack-api recordings are missing"))

  private def read(file: Path): ujson.Value = ujson.read(Files.readString(file))

  private val RateLimited: Answer = answer(read(directory.resolve("rate-limited.json")))

  /** Every recorded answer by its request's key, in the order recorded. */
  private val recordings: Map[String, Vector[Answer]] = {
    val stream = Files.list(directory)
    val files =
      try stream.iterator.asScala.toVector
      finally stream.close()
    files
      .filter(f => f.getFileName.toString.matches("""\d{3}-.*\.json"""))
      .sortBy(_.getFileName.toString)
      .map(read)
      .map { r =>
        val params = r("params").obj.map((k, v) => (k, v.str)).toMap
        (keyOf(r("method").str, params), answer(r))
      }
      .groupMap(_._1)(_._2)
  }
}
