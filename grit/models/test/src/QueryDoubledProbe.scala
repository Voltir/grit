package grit.models

import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Path}
import java.time.Instant

import scala.util.control.NonFatal

import grit.assembly.retrieval.QueryWriter
import grit.core.id.{ConversationId, EntryId, TurnSeq}
import grit.core.message.{Message, Usage}
import grit.core.provider.ProviderError
import grit.core.store.{Entry, Payload}

/** Live probe: is the query role's text doubled (one text twice over), and where? The query
  * role's request as retrieval builds it ([[QueryWriter.request]]) for one message, sent
  * `Calls` times plain and `Calls` times streamed to `Model` on `Pin` (the query role's
  * catalog pin, set here so that no override in the environment moves it), with each
  * response's `provider` and stop. A reply is read twice: its raw text (the plain body's
  * `content`, the stream's `delta.content` joined) and grit's ([[OpenRouterJson.response]],
  * [[OpenRouterStream.fold]], then [[QueryWriter.text]]). Doubled raw text is the upstream's;
  * grit's doubled over a single raw text is grit's. Each raw body is saved to a fresh
  * temporary directory, which the run prints.
  *
  * `MaxTokens` caps each call; the run stops once its spend passes `Budget`.
  *
  * {{{set -a; . ./.env; set +a; ./mill grit.models.test.runMain grit.models.QueryDoubledProbe [--heard] <message>}}}
  *
  * `--heard` sends the message as overheard (an unprompted turn's root) rather than said to
  * grit. Not a test: `./mill __.test` makes no model calls.
  */
object QueryDoubledProbe {

  private val Calls = 10
  private val MaxTokens = 400
  private val Budget = BigDecimal("0.009")
  private val Model = "openai/gpt-oss-120b"
  private val Pin = "cerebras/fp16"

  /** One reply: the raw text, grit's query, the upstream that served it, its stop and cost. */
  private final case class Read(
      raw: String,
      query: Either[ProviderError, String],
      served: String,
      stop: String,
      cost: Option[BigDecimal]
  )

  def main(args: Array[String]): Unit = {
    val heard = args.headOption.contains("--heard")
    val message = args.dropWhile(_ == "--heard").mkString(" ").trim
    if (message.isEmpty) println("usage: QueryDoubledProbe [--heard] <message>")
    else {
      val env = sys.env ++ Map(
        ModelRole.Query.modelVar -> Model,
        ModelRole.Query.upstreamVar -> Pin,
        ModelRole.Query.maxTokensVar -> MaxTokens.toString
      )
      OpenRouterConfig.forRole(env, ModelRole.Query) match {
        case Left(invalid) => println(invalid)
        case Right(config) => run(config, entry(message, heard))
      }
    }
  }

  private def entry(message: String, heard: Boolean): Entry =
    Entry(
      EntryId("probe:0"),
      ConversationId("probe"),
      TurnSeq.First,
      None,
      0L,
      if (heard) Payload.Heard(message) else Payload.Message(Message.User(message)),
      Instant.EPOCH
    )

  private def run(config: OpenRouterConfig, own: Entry): Unit = {
    val dir = Files.createTempDirectory("query-doubled")
    val body = OpenRouterJson.request(
      config.model,
      config.maxTokens,
      config.upstream,
      QueryWriter.request(Vector(own)),
      config.effort,
      config.replay
    )
    val http = HttpClient.newBuilder().connectTimeout(config.timeout).build()
    println(s"[probe] $config; raw bodies in $dir")
    val calls = for {
      streamed <- Vector(false, true)
      i <- 1 to Calls
    } yield (if (streamed) "stream" else "plain", i)
    // Each call's outcome, in order, until the spend passes the budget.
    val outcomes = calls.foldLeft((BigDecimal(0), Vector.empty[(String, String)])) {
      case ((spent, done), _) if spent > Budget => (spent, done)
      case ((spent, done), (mode, i)) =>
        val file = dir.resolve(s"$mode-$i.${if (mode == "stream") "sse" else "json"}")
        call(http, config, body, mode == "stream", file) match {
          case Left(why) =>
            println(s"[probe] $mode $i: FAILED $why")
            (spent, done :+ (mode -> "failed"))
          case Right(r) =>
            val rawDoubled = doubled(r.raw)
            val gritDoubled = r.query.fold(_ => "unread", q => doubled(q))
            println(
              s"[probe] $mode $i: served by ${r.served}; stop ${r.stop}; " +
                s"raw $rawDoubled; grit $gritDoubled; cost ${r.cost.getOrElse("?")}; " +
                s"query ${r.query.fold(e => s"FAILED $e", q => ujson.write(ujson.Str(q)))}"
            )
            (
              spent + r.cost.getOrElse(BigDecimal(0)),
              done :+ (mode -> s"raw $rawDoubled, grit $gritDoubled")
            )
        }
    }
    val (spent, done) = outcomes
    Vector("plain", "stream").foreach { mode =>
      val reads = done.collect { case (m, read) if m == mode => read }
      val kinds = reads.distinct.sorted
      println(
        s"[probe] $mode: ${kinds.map(k => s"${reads.count(_ == k)}/${reads.size} $k").mkString("; ")}"
      )
    }
    println(s"[probe] spent $$$spent${if (spent > Budget) " (stopped at the budget)" else ""}")
  }

  /** `exact` when the text, trimmed, is one shorter text repeated; `spaced` when only its
    * words are (a repetition with whitespace at the join); `single` otherwise.
    */
  private def doubled(text: String): String = {
    def periodic[A](s: Seq[A]): Boolean =
      s.size >= 2 && (1 to s.size / 2).exists { p =>
        s.size % p == 0 && s.indices.forall(i => s(i) == s(i % p))
      }
    val trimmed = text.trim
    if (periodic(trimmed)) "exact"
    else if (periodic(trimmed.split("\\s+").toVector.filter(_.nonEmpty))) "spaced"
    else "single"
  }

  private def call(
      http: HttpClient,
      config: OpenRouterConfig,
      sent: ujson.Value,
      streamed: Boolean,
      save: Path
  ): Either[String, Read] = {
    val body = ujson.copy(sent)
    if (streamed) body("stream") = true
    try {
      val response = http.send(
        HttpRequest
          .newBuilder(config.endpoint)
          .timeout(config.timeout)
          .header("Authorization", s"Bearer ${config.apiKey}")
          .header("Content-Type", "application/json")
          .header("X-OpenRouter-Title", "grit")
          .POST(HttpRequest.BodyPublishers.ofString(ujson.write(body)))
          .build(),
        HttpResponse.BodyHandlers.ofString()
      )
      val _ = Files.writeString(save, response.body)
      if (response.statusCode != 200) Left(s"HTTP ${response.statusCode}")
      else if (streamed) Right(readStream(response.body))
      else Right(readPlain(response.body))
    } catch {
      case NonFatal(e) => Left(e.getClass.getSimpleName)
    }
  }

  private def readPlain(text: String): Read = {
    val root = scala.util.Try(ujson.read(text)).toOption
    val raw = root
      .flatMap(r => scala.util.Try(r("choices")(0)("message")("content").str).toOption)
      .getOrElse("")
    val reply = root.toRight(ProviderError.Refused("unreadable")).flatMap(OpenRouterJson.response)
    described(raw, reply, root.flatMap(_.objOpt).flatMap(_.get("provider")).flatMap(_.strOpt).toSet)
  }

  private def readStream(text: String): Read = {
    val chunks = text.linesIterator
      .filter(_.startsWith("data:"))
      .flatMap(l => scala.util.Try(ujson.read(l.drop(5).trim)).toOption)
      .toVector
    val raw = chunks
      .flatMap(c => scala.util.Try(c("choices")(0)("delta")("content").str).toOption)
      .mkString
    val served = chunks.flatMap(_.objOpt).flatMap(_.get("provider")).flatMap(_.strOpt).toSet
    val reply = OpenRouterStream.fold(text.linesIterator, _ => ()).flatMap(OpenRouterJson.response)
    described(raw, reply, served)
  }

  private def described(
      raw: String,
      reply: Either[ProviderError, Message.Assistant],
      served: Set[String]
  ): Read =
    Read(
      raw,
      reply.map(QueryWriter.text),
      if (served.isEmpty) "?" else served.mkString(","),
      reply.fold(_ => "?", _.stop.toString),
      reply.toOption.map(_.usage).flatMap((u: Usage) => u.costUsd)
    )
}
