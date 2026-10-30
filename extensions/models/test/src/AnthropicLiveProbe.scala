package grit.models

import java.net.http.{HttpClient, HttpResponse}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Duration

import scala.jdk.CollectionConverters.*

import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Effort, ReasoningReplay}
import grit.core.provider.{ModelRequest, ProviderError, ToolSchema}

/** Live probe of Anthropic's Messages API, three questions the unit tests cannot answer:
  *
  *   - **Cache reads.** Two Claude Haiku 5.5 calls share a system prompt well over the minimum
  *     cacheable length; the second adds six blocks after the first's message breakpoint. Its
  *     cache reads must cover all the first call cached (the breakpoint's lookback reaching
  *     back to it), not the system prompt alone.
  *   - **`drop_block`, per model.** For Claude Haiku 5.5 and Claude Sonnet 5.5: a reply's
  *     signed thinking, asked for at effort high on a question of a few steps, sent back after
  *     the system prompt was edited, without `drop_block` and with it. A model belongs in
  *     [[AnthropicProvider.DropsMismatchedThinking]] when the call with it is answered.
  *   - **The stream reader against the real stream.** One Claude Haiku 5.5 tool call that must
  *     be reasoned out first, at effort high, streamed and plain; both saved in the directory
  *     given, and the stream folded by [[AnthropicStream.fold]] must agree with the plain
  *     response on its input tokens and stop reason, and keep every thinking block's
  *     signature.
  *
  * A check whose reply carried no thinking is reported inconclusive, never passed. Small
  * `max_tokens`, effort low but where thinking is wanted; it stops before any call once its
  * calls have cost [[Cap]], and prints each call's usage and cost.
  *
  * {{{set -a; . ./.env; set +a; ./mill grit.models.test.runMain grit.models.AnthropicLiveProbe "$PWD/extensions/models/test/resources"}}}
  *
  * Not a test: `./mill __.test` makes no model calls. Without `ANTHROPIC_API_KEY` it says so
  * and makes none. It exits 1 when a check fails.
  */
object AnthropicLiveProbe {

  private val Haiku = "claude-haiku-5-5"
  private val Sonnet = "claude-sonnet-5-5"

  /** No call is made once the calls made have cost this much, in USD: 0.9¢. */
  private val Cap = BigDecimal("0.009")

  /** The output budget of a `drop_block` check's edited calls, which need only be answered. */
  private val Edited = 64

  private val http = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(30)).build()

  // Replaced whole by the probe's own steps, run one after another on main's one thread.
  @caps.unsafe.untrackedCaptures
  private var spent = BigDecimal(0)

  // Replaced whole by the probe's own steps, run one after another on main's one thread.
  @caps.unsafe.untrackedCaptures
  private var failures = Vector.empty[String]

  def main(args: Array[String]): Unit =
    (sys.env.get(AnthropicProvider.KeyVar).filter(_.trim.nonEmpty), args.headOption) match {
      case (None, _) =>
        println(s"[probe] skipped: ${AnthropicProvider.KeyVar} is not set, so no call was made")
      case (Some(_), None) =>
        println("[probe] usage: AnthropicLiveProbe <the directory the recordings are saved in>")
        sys.exit(2)
      case (Some(key), Some(dir)) =>
        cached(key)
        dropping(key, Haiku, 1024)
        dropping(key, Sonnet, 450)
        recorded(key, Path.of(dir))
        println(s"[probe] spent ${cents(spent)} in all")
        failures.foreach(f => println(s"[probe] FAILED: $f"))
        if (failures.nonEmpty) sys.exit(1)
    }

  private def cached(key: String): Unit = {
    // About 2,500 tokens, the same on every call: over Haiku 5.5's minimum of 512.
    val system = (1 to 100)
      .map(i =>
        s"Rule $i: a card names its source, the day it was read and who may read it; " +
          s"card $i is kept until its source changes."
      )
      .mkString("\n")
    val asked = Message.User("Reply with the word ok.")
    val first = ModelRequest(system, Vector(asked), stable = 1)
    for {
      one <- answered(key, Haiku, 256, first, dropBlock = false, "cache, call 1")
      later = Vector(
        asked,
        one,
        Message.User("Now the word yes."),
        said("yes"),
        Message.User("Now the word no."),
        said("no"),
        Message.User("Now the word ok, once more.")
      )
      two <- answered(
        key,
        Haiku,
        256,
        ModelRequest(system, later, stable = later.size - 1),
        dropBlock = false,
        "cache, call 2"
      )
    } {
      val cachedByOne = Tokens.value(one.usage.cacheRead) + Tokens.value(one.usage.cacheWrite)
      val readByTwo = Tokens.value(two.usage.cacheRead)
      println(s"[probe] cache: call 1 cached $cachedByOne tokens; call 2 read $readByTwo")
      check(readByTwo > 0, "cache: the second Haiku call read nothing from the cache")
      check(
        readByTwo >= cachedByOne,
        s"cache: the second call read $readByTwo tokens, under the $cachedByOne the first " +
          "cached: its breakpoint's lookback did not reach the first call's message breakpoint"
      )
    }
  }

  /** `model`'s thinking asked for within `maxTokens`; the edited calls after it, which need only
    * be answered or refused, are given [[Edited]] tokens at effort low (effort is no edit).
    */
  private def dropping(key: String, model: String, maxTokens: Int): Unit = {
    val question = Message.User(
      "A shop sells pens at 3 for $4 and notebooks at $7 each. Ana buys 9 pens and 2 " +
        "notebooks and pays with a $50 note. How much change does she get? Work it out step " +
        "by step, then give the number alone."
    )
    for {
      reply <- answered(
        key,
        model,
        maxTokens,
        ModelRequest("You are terse.", Vector(question), stable = 1),
        dropBlock = false,
        s"drop_block, $model, its thinking",
        Effort.High
      )
    } {
      if (signed(reply).isEmpty)
        println(
          s"[probe] drop_block, $model: inconclusive, the reply carried no thinking " +
            s"(${kinds(reply)}; stop ${reply.stop})"
        )
      else if (!signed(reply).forall(identity))
        println(s"[probe] drop_block, $model: inconclusive, its thinking came unsigned")
      else {
        val edited = ModelRequest(
          "You are terse. Answer in digits.",
          Vector(question, reply, Message.User("And if she buys one pen fewer?")),
          stable = 2
        )
        val without = sent(
          key,
          model,
          Edited,
          edited,
          dropBlock = false,
          s"drop_block, $model, edited without it"
        )
        val withIt =
          sent(
            key,
            model,
            Edited,
            edited,
            dropBlock = true,
            s"drop_block, $model, edited with it"
          )
        println(s"[probe] drop_block, $model, edited without it: ${outcome(without)}")
        println(s"[probe] drop_block, $model, edited with it: ${outcome(withIt)}")
        println(
          s"[probe] drop_block, $model: " +
            (if (withIt.exists(_._1 == 200)) "accepted, so it belongs in DropsMismatchedThinking"
             else "not accepted, so it stays out of DropsMismatchedThinking")
        )
      }
    }
  }

  private def recorded(key: String, dir: Path): Unit = {
    val read = ToolSchema(
      "read_file",
      "Reads a file in the checkout.",
      ujson.Obj(
        "type" -> "object",
        "properties" -> ujson.Obj("path" -> ujson.Obj("type" -> "string")),
        "required" -> ujson.Arr("path")
      )
    )
    val request = ModelRequest(
      "You can read the checkout's files with read_file.",
      Vector(
        Message.User(
          "The file to read is named after the sum of the first six prime numbers, with .txt " +
            "after it. Work out its name step by step, then read it with the tool."
        )
      ),
      Vector(read),
      stable = 1
    )
    val body = AnthropicJson.request(
      Haiku,
      1024,
      Some(Effort.High),
      ReasoningReplay.Details,
      dropBlock = false,
      request
    )
    if (affordable("stream, streamed")) {
      val streaming = ujson.copy(body)
      streaming("stream") = true
      val response = http.send(
        ProviderHttp.post(
          AnthropicProvider.Endpoint,
          Duration.ofMinutes(2),
          AnthropicProvider.headers(key, dropBlock = false),
          streaming
        ),
        HttpResponse.BodyHandlers.ofLines()
      )
      val lines = response.body.iterator().asScala.toVector
      if (response.statusCode != 200)
        check(false, s"stream: HTTP ${response.statusCode}: ${lines.mkString("\n")}")
      else {
        save(dir.resolve("anthropic-live-haiku.sse"), lines.mkString("\n") + "\n")
        val streamed = AnthropicStream.fold(lines.iterator, _ => ()).flatMap(AnthropicJson.response)
        streamed.foreach(spend("stream, streamed", _))
        val plain =
          sent(key, Haiku, 1024, request, dropBlock = false, "stream, plain", raw = Some(body))
        plain.foreach { case (status, text) =>
          if (status == 200) save(dir.resolve("anthropic-live-haiku.json"), text + "\n")
        }
        val plainReply = plain match {
          case Some((200, text)) => AnthropicJson.response(ujson.read(text))
          case Some((status, text)) => Left(ProviderError.Refused(s"HTTP $status: $text"))
          case None => Left(ProviderError.Refused("not sent"))
        }
        (streamed, plainReply) match {
          case (Right(s), Right(p)) =>
            println(s"[probe] stream: streamed ${kinds(s)}; plain ${kinds(p)}")
            check(
              s.usage.input == p.usage.input,
              s"stream: input ${s.usage.input} streamed, ${p.usage.input} plain"
            )
            check(s.stop == p.stop, s"stream: stopped ${s.stop} streamed, ${p.stop} plain")
            if (signed(s).isEmpty)
              println("[probe] stream: inconclusive for thinking, the streamed reply had none")
            else check(signed(s).forall(identity), "stream: a thinking block lost its signature")
          case (s, p) => check(false, s"stream: streamed $s; plain $p")
        }
      }
    }
  }

  /** `request` sent on `model` ([[sent]]), its reply read, or the failure noted. */
  private def answered(
      key: String,
      model: String,
      maxTokens: Int,
      request: ModelRequest,
      dropBlock: Boolean,
      label: String,
      effort: Effort = Effort.Low
  ): Option[Message.Assistant] =
    sent(key, model, maxTokens, request, dropBlock, label, effort) match {
      case None => None
      case Some((200, text)) =>
        AnthropicJson.response(ujson.read(text)) match {
          case Right(reply) => Some(reply)
          case Left(e) =>
            check(false, s"$label: $e")
            None
        }
      case Some((status, text)) =>
        check(false, s"$label: HTTP $status: $text")
        None
    }

  /** `request` sent on `model` as [[AnthropicJson.request]] writes it (or `raw`), its status and
    * body, an answer's cost counted under `label`; `None` once the probe's spending has reached
    * [[Cap]].
    */
  private def sent(
      key: String,
      model: String,
      maxTokens: Int,
      request: ModelRequest,
      dropBlock: Boolean,
      label: String,
      effort: Effort = Effort.Low,
      raw: Option[ujson.Value] = None
  ): Option[(Int, String)] =
    Option.when(affordable(label)) {
      val body = raw.getOrElse(
        AnthropicJson.request(
          model,
          maxTokens,
          Some(effort),
          ReasoningReplay.Details,
          dropBlock,
          request
        )
      )
      val response = http.send(
        ProviderHttp.post(
          AnthropicProvider.Endpoint,
          Duration.ofMinutes(2),
          AnthropicProvider.headers(key, dropBlock),
          body
        ),
        HttpResponse.BodyHandlers.ofString()
      )
      if (response.statusCode == 200)
        AnthropicJson.response(ujson.read(response.body)).foreach(spend(label, _))
      (response.statusCode, response.body)
    }

  private def affordable(what: String): Boolean = {
    val ok = spent < Cap
    if (!ok) check(false, s"$what: not called, the probe has spent ${cents(spent)}")
    ok
  }

  private def spend(label: String, reply: Message.Assistant): Unit = {
    val cost = reply.usage.costUsd.getOrElse(BigDecimal(0))
    spent += cost
    val u = reply.usage
    println(
      s"[probe] $label: ${reply.model}, input ${u.input} (read ${u.cacheRead}, written " +
        s"${u.cacheWrite}), output ${u.output}, ${cents(cost)}; stop ${reply.stop}"
    )
  }

  private def outcome(response: Option[(Int, String)]): String = response match {
    case None => "not sent"
    case Some((200, text)) =>
      AnthropicJson.response(ujson.read(text)).fold(_.toString, r => s"answered, ${kinds(r)}")
    case Some((status, text)) => AnthropicJson.error(status, text).toString
  }

  /** For each of `m`'s thinking blocks, whether it came signed (a redacted one, with its data). */
  private def signed(m: Message.Assistant): Vector[Boolean] = m.blocks.collect {
    case AssistantBlock.Reasoning(_, replay) =>
      replay
        .flatMap(_.objOpt)
        .flatMap(r => r.get("signature").orElse(r.get("data")))
        .flatMap(_.strOpt)
        .exists(_.nonEmpty)
  }

  private def said(text: String): Message.Assistant = Message.Assistant(
    Vector(AssistantBlock.Text(text)),
    StopReason.EndTurn,
    Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
    Haiku
  )

  private def kinds(m: Message.Assistant): String = m.blocks
    .map {
      case AssistantBlock.Text(_) => "text"
      case AssistantBlock.Reasoning(_, replay) => s"thinking(kept=${replay.isDefined})"
      case AssistantBlock.ToolCall(_, name, _) => s"call:$name"
    }
    .mkString(", ")

  private def check(holds: Boolean, failure: String): Unit =
    if (!holds) failures = failures :+ failure

  private def save(path: Path, text: String): Unit = {
    val _ = Files.writeString(path, text, StandardCharsets.UTF_8)
    println(s"[probe] saved $path")
  }

  private def cents(usd: BigDecimal): String =
    s"${(usd * 100).bigDecimal.stripTrailingZeros.toPlainString}¢"
}
