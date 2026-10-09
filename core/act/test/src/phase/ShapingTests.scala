package grit.act.phase

import java.time.Instant

import grit.core.clock.SetClock
import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{ArgRepair, NameRepair, StrictSchemas}
import grit.core.provider.{ModelRequest, Provider, ProviderError, ToolUse}
import grit.core.schema.{JsonSchema, Typed}

import utest.*

/** A JSON ask's phases: its request, its reply read, and its one repair. */
object ShapingTests extends TestSuite {

  private val schemaJson: ujson.Obj = ujson.Obj(
    "type" -> "object",
    "properties" -> ujson.Obj(
      "count" -> ujson.Obj("type" -> "integer", "minimum" -> 1, "maximum" -> 5)
    ),
    "required" -> ujson.Arr("count"),
    "additionalProperties" -> false
  )

  private val schema: JsonSchema =
    JsonSchema.read(schemaJson).fold(e => throw new IllegalStateException(e.message), identity)

  private val asked: Vector[Message] = Vector(Message.User("how many?"))

  /** The reply's `count`, refused when it is 5. */
  private val count: Typed[Int] = Typed(
    schema,
    c =>
      c.json.objOpt.flatMap(_.get("count")).flatMap(_.numOpt).map(_.toInt) match {
        case Some(5) => Left("5 is too many")
        case read => read.toRight("no count")
      }
  )

  private val start = Instant.parse("2026-10-08T12:00:00Z")

  private val usage = Usage(Tokens(10), Tokens(2), Tokens.Zero, None)

  private def called(id: String, name: String, args: ujson.Value): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.ToolCall(ToolCallId(id), name, args)),
      StopReason.ToolUse,
      usage,
      "m"
    )

  private def said(text: String): Message.Assistant =
    Message.Assistant(Vector(AssistantBlock.Text(text)), StopReason.EndTurn, usage, "m")

  /** A provider that answers each call with the next of `script`, keeping each request. */
  private final class Scripted(script: Vector[Either[ProviderError, Message.Assistant]])
      extends Provider {
    @caps.unsafe.untrackedCaptures
    var requests = Vector.empty[ModelRequest]
    def complete(r: ModelRequest): Either[ProviderError, Message.Assistant] = {
      val i = requests.size
      requests = requests :+ r
      script.lift(i).getOrElse(Left(ProviderError.Refused("script ran out")))
    }
  }

  private val first: ModelRequest = Shaping.request(
    "system",
    asked,
    "reply",
    schema,
    StrictSchemas.Ignored
  )

  /** `script` asked through [[Shaping.shaped]] under `names` and quoted-number repairs. */
  private def shape(
      script: Vector[Either[ProviderError, Message.Assistant]],
      retries: Int,
      names: NameRepair = NameRepair.HarmonyCut
  ): (Shaped[Int], Vector[ModelRequest]) = {
    val provider = new Scripted(script)
    val got = Shaping.shaped(
      provider,
      first,
      "reply",
      count,
      names,
      Set(ArgRepair.QuotedNumber),
      retries,
      new SetClock(start)
    )
    (got, provider.requests)
  }

  /** What `shaped` read, as plain values: its conforming text, value and responses. */
  private def read(s: Shaped[Int]): Option[(String, Int, Vector[Message.Assistant])] = s match {
    case Shaped.Read(c, v, calls) => Some((c.text, v, calls.map(_.answer)))
    case _ => None
  }

  /** `answer`, the response to the first request. */
  private def toFirst(answer: Message.Assistant): Shaped.Call = Shaped.Call(first, answer)

  private def sent(strict: StrictSchemas): ModelRequest =
    Shaping.request("system", asked, "reply", schema, strict)

  val tests = Tests {
    test("the request requires a call of its one tool, whose parameters are the schema") {
      val got = sent(StrictSchemas.Ignored)
      (got.system, got.messages, got.use, got.tools.map(t => (t.name, t.parameters))) ==>
        ("system", asked, ToolUse.Required, Vector(("reply", schemaJson)))
    }

    test("strict is sent when the upstream holds it, under auto or only when required") {
      val strictness = StrictSchemas.values.toVector.map(s => s -> sent(s).tools.map(_.strict))
      strictness ==> Vector(
        StrictSchemas.Enforced -> Vector(true),
        StrictSchemas.WhenRequired -> Vector(true),
        StrictSchemas.Ignored -> Vector(false),
        StrictSchemas.Rejected -> Vector(false)
      )
    }

    test("a conforming reply is read in one call, its repairs made") {
      val answer = called("a", "reply", ujson.Obj("count" -> "3"))
      val (got, requests) = shape(Vector(Right(answer)), retries = 1)
      (read(got), requests) ==> (Some(("{\"count\":3}", 3, Vector(answer))), Vector(first))
    }

    test("a reply its schema refuses is answered with the mismatch's path and asked again") {
      val wrong = called("a", "reply", ujson.Obj("count" -> 9))
      val right = called("b", "reply", ujson.Obj("count" -> 2))
      val (got, requests) = shape(Vector(Right(wrong), Right(right)), retries = 1)
      val told = Message.ToolResult(
        ToolCallId("a"),
        "Your reply does not match its schema: count: expected at most 5, got 9. " +
          "Call `reply` again.",
        isError = true
      )
      (read(got), requests) ==>
        (
          Some(("{\"count\":2}", 2, Vector(wrong, right))),
          Vector(first, first.copy(messages = asked :+ wrong :+ told))
        )
    }

    test("a reply its reader refuses is answered with the refusal and asked again") {
      val wrong = called("a", "reply", ujson.Obj("count" -> 5))
      val right = called("b", "reply", ujson.Obj("count" -> 2))
      val (got, requests) = shape(Vector(Right(wrong), Right(right)), retries = 1)
      val told = Message.ToolResult(
        ToolCallId("a"),
        "Your reply could not be read: 5 is too many. Call `reply` again.",
        isError = true
      )
      (read(got), requests.map(_.messages)) ==>
        (Some(("{\"count\":2}", 2, Vector(wrong, right))), Vector(asked, asked :+ wrong :+ told))
    }

    test(
      "with one retry, two failures are unread, the last one's why, each call kept beside the request it answered"
    ) {
      val high = called("a", "reply", ujson.Obj("count" -> 9))
      val low = called("b", "reply", ujson.Obj("count" -> 0))
      val right = called("c", "reply", ujson.Obj("count" -> 2))
      val (got, requests) = shape(Vector(Right(high), Right(low), Right(right)), retries = 1)
      val tooHigh = Message.ToolResult(
        ToolCallId("a"),
        "Your reply does not match its schema: count: expected at most 5, got 9. " +
          "Call `reply` again.",
        isError = true
      )
      (got, requests.size) ==>
        (
          Shaped.Unread(
            "its arguments do not match its schema: count: expected at least 1, got 0",
            Vector(toFirst(high), Shaped.Call(first.copy(messages = asked :+ high :+ tooHigh), low))
          ),
          2
        )
    }

    test("a text reply that calls no tool is the first try's failure, told to call it") {
      val text = said("three")
      val right = called("b", "reply", ujson.Obj("count" -> 3))
      val (got, requests) = shape(Vector(Right(text), Right(right)), retries = 1)
      val told = Message.User("Your reply did not call `reply`. Call `reply` with your reply.")
      (read(got), requests.map(_.messages)) ==>
        (Some(("{\"count\":3}", 3, Vector(text, right))), Vector(asked, asked :+ text :+ told))
    }

    test("a call named with harmony's tokens is the tool's under HarmonyCut") {
      val answer = called("a", "reply<|channel|>commentary", ujson.Obj("count" -> 4))
      val (got, _) = shape(Vector(Right(answer)), retries = 0, NameRepair.HarmonyCut)
      read(got) ==> Some(("{\"count\":4}", 4, Vector(answer)))
    }

    test("a call named with harmony's tokens is no call of the tool under AsSent") {
      val answer = called("a", "reply<|channel|>commentary", ujson.Obj("count" -> 4))
      val (got, _) = shape(Vector(Right(answer)), retries = 0, NameRepair.AsSent)
      got ==> Shaped.Unread("no call of `reply`", Vector(toFirst(answer)))
    }

    test("a provider failure on the repair is failed, after the first call") {
      val wrong = called("a", "reply", ujson.Obj("count" -> 9))
      val (got, _) =
        shape(Vector(Right(wrong), Left(ProviderError.Refused("no such model"))), retries = 1)
      got ==> Shaped.Failed("no such model", Vector(toFirst(wrong)))
    }
  }
}
