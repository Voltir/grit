package grit.models

import java.net.http.{HttpClient, HttpResponse}
import java.time.Duration

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{ModelRequest, ToolSchema}

/** Live probe of the cache breakpoints [[OpenRouterJson.request]] sends for an Anthropic model,
  * which the unit tests can pin only as written: whether OpenRouter passes them on. Three
  * Claude Haiku 5.5 calls through OpenRouter share a system prompt well over the minimum
  * cacheable length and offer one tool:
  *
  *   1. the system prompt and a user message, marked on the user message;
  *   1. the same, then a reply calling the tool, its result and a user line, marked on the
  *      tool result (a `tool` message);
  *   1. the second, then a reply and another user line, marked on that line.
  *
  * The first must write to the cache; the second must read at least all the first cached; the
  * third must read at least all the second cached, which only a mark on the tool message can
  * have written. Should that last check fail while the others pass, the decision ready is to
  * mark the last user or assistant part at or before message `stable` instead of a tool
  * message.
  *
  * `max_tokens` 32; it stops before any call once its calls have cost [[Cap]], and prints each
  * call's usage and cost.
  *
  * {{{set -a; . ./.env; set +a; ./mill grit.models.test.runMain grit.models.OpenRouterCacheProbe}}}
  *
  * Not a test: `./mill __.test` makes no model calls. Without `OPENROUTER_API_KEY` it says so
  * and makes none. It exits 1 when a check fails.
  */
object OpenRouterCacheProbe {

  private val Model = "anthropic/claude-haiku-5.5"

  /** No call is made once the calls made have cost this much, in USD: 0.5¢. */
  private val Cap = BigDecimal("0.005")

  private val http = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(30)).build()

  def main(args: Array[String]): Unit = {
    val _ = args
    sys.env.get(OpenRouterConfig.KeyVar).filter(_.trim.nonEmpty) match {
      case None =>
        println(s"[probe] skipped: ${OpenRouterConfig.KeyVar} is not set, so no call was made")
      case Some(key) =>
        val failures = run(key)
        failures.foreach(f => println(s"[probe] FAILED: $f"))
        if (failures.nonEmpty) sys.exit(1)
    }
  }

  /** The probe's three calls, and the checks that failed. */
  private def run(key: String): Vector[String] = {
    // About 2,500 tokens, the same on every call: over Haiku 5.5's minimum of 512.
    val system = (1 to 100)
      .map(i =>
        s"Rule $i: a card names its source, the day it was read and who may read it; " +
          s"card $i is kept until its source changes."
      )
      .mkString("\n")
    val tools = Vector(
      ToolSchema(
        "lookup",
        "Look up a card by its number.",
        ujson.Obj(
          "type" -> "object",
          "properties" -> ujson.Obj("n" -> ujson.Obj("type" -> "integer"))
        )
      )
    )
    val asked = Message.User("Look up card 7, then reply with the word ok.")
    val calling = Message.Assistant(
      Vector(AssistantBlock.ToolCall(ToolCallId("toolu_1"), "lookup", ujson.Obj("n" -> 7))),
      StopReason.ToolUse,
      Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
      Model
    )
    val two = Vector(
      asked,
      calling,
      Message.ToolResult(ToolCallId("toolu_1"), "Card 7: kept until its source changes.", false),
      Message.User("Reply with the word ok.")
    )
    val three = two ++ Vector(said("ok"), Message.User("Now the word yes."))
    val requests = Vector(
      ModelRequest(system, Vector(asked), tools, stable = 1),
      ModelRequest(system, two, tools, stable = 3),
      ModelRequest(system, three, tools, stable = three.size)
    )
    val usages = requests.zipWithIndex.foldLeft(Vector.empty[Usage]) { case (done, (r, i)) =>
      val spent = done.flatMap(_.costUsd).sum
      if (spent >= Cap || done.size < i) done
      else send(key, r, s"call ${i + 1}").fold(done)(done :+ _)
    }
    println(s"[probe] spent ${cents(usages.flatMap(_.costUsd).sum)} in all")
    def cached(u: Usage): Long = Tokens.value(u.cacheRead) + Tokens.value(u.cacheWrite)
    usages match {
      case Vector(one, two, three) =>
        Vector(
          Option.when(Tokens.value(one.cacheWrite) + Tokens.value(one.cacheRead) == 0)(
            "call 1 neither wrote to the cache nor read from it"
          ),
          Option.when(Tokens.value(two.cacheRead) < cached(one))(
            s"call 2 read ${two.cacheRead} tokens, under the ${cached(one)} call 1 cached"
          ),
          Option.when(Tokens.value(three.cacheRead) < cached(two))(
            s"call 3 read ${three.cacheRead} tokens, under the ${cached(two)} call 2 cached: " +
              "the mark on the tool message was not passed on"
          )
        ).flatten
      case some => Vector(s"only ${some.size} of the 3 calls were answered")
    }
  }

  /** `request` sent through OpenRouter as [[OpenRouterJson.request]] writes it, its usage
    * printed under `label`; `None` when it was not answered.
    */
  private def send(key: String, request: ModelRequest, label: String): Option[Usage] = {
    val response = http.send(
      ProviderHttp.post(
        OpenRouterConfig.Endpoint,
        Duration.ofMinutes(2),
        Seq("Authorization" -> s"Bearer $key"),
        OpenRouterJson.request(Model, 32, None, request)
      ),
      HttpResponse.BodyHandlers.ofString()
    )
    if (response.statusCode != 200) {
      println(s"[probe] $label: HTTP ${response.statusCode}: ${response.body.take(300)}")
      None
    } else
      OpenRouterJson.response(ujson.read(response.body)) match {
        case Left(error) =>
          println(s"[probe] $label: $error")
          None
        case Right(reply) =>
          val u = reply.usage
          println(
            s"[probe] $label: ${reply.model}, input ${u.input} (read ${u.cacheRead}, written " +
              s"${u.cacheWrite}), output ${u.output}, ${cents(u.costUsd.getOrElse(BigDecimal(0)))}"
          )
          Some(u)
      }
  }

  private def said(text: String): Message.Assistant = Message.Assistant(
    Vector(AssistantBlock.Text(text)),
    StopReason.EndTurn,
    Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
    Model
  )

  private def cents(usd: BigDecimal): String =
    s"${(usd * 100).bigDecimal.stripTrailingZeros.toPlainString}¢"
}
