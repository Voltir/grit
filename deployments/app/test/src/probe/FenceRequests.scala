package grit.app.probe

import java.time.Instant

import grit.act.phase.Shaping
import grit.core.act.Dreaming
import grit.core.act.Posed
import grit.core.context.Shown
import grit.core.document.{DocLabel, DocText}
import grit.core.id.{ConversationId, EntryId, EntrySeq, ToolCallId, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{StrictSchemas, ToolGuidance}
import grit.core.period.Balance
import grit.core.persona.Persona
import grit.core.prompt.SystemPrompt
import grit.core.provider.{ModelRequest, ToolSchema}
import grit.core.store.{Entry, Origin, Payload, Speakers}
import grit.core.visibility.Integrity
import grit.knowledge.sync.SummaryAsk
import grit.knowledge.topic.AttachAsk
import grit.lifecycle.close.{Asked, ClosingSummary}
import grit.lifecycle.transcript.PeriodTranscript
import grit.turn.{TurnLoop, TurnPrompt}

/** Each of [[FenceSamples]]' requests, as grit's builders make them. A turn's is composed here
  * as `TurnOffer` and `TurnShown` compose one (grit's base fragments, its Slack edge, its own
  * messages through `Shown.turn`, its tools listed under `ToolGuidance.SystemLines`), without
  * a window: the probe asks of the tool loop's last step alone.
  */
object FenceRequests {

  /** A sample's request, named. */
  final case class Built(name: String, request: ModelRequest)

  def all: Vector[Built] =
    FenceSamples.closes.map(c => Built(c.name, close(c))) ++
      FenceSamples.syncs.map(s => Built(s.name, json(SummaryAsk.of(s.subject, s.head)))) ++
      FenceSamples.dreams.map(d => Built(d.name, dream(d))) ++
      FenceSamples.turns.map(t => Built(t.name, turn(t)))

  private def entry(seq: Long, payload: Payload): Entry =
    Entry(
      EntryId(s"e$seq"),
      ConversationId("probe"),
      TurnSeq(0),
      None,
      EntrySeq(seq),
      payload,
      Instant.EPOCH,
      Integrity.Member
    )

  private val NoUsage = Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None)

  private def called(seq: Long, name: String, args: ujson.Obj): Entry =
    entry(
      seq,
      Payload.Exchange(
        Message.Assistant(
          Vector(AssistantBlock.ToolCall(ToolCallId(s"c$seq"), name, args)),
          StopReason.ToolUse,
          NoUsage,
          "m"
        )
      )
    )

  private def result(seq: Long, call: Long, shown: String, content: String): Entry =
    entry(seq, Payload.Result(Message.ToolResult(ToolCallId(s"c$call"), content, false), shown))

  /** The close's period: each read is a call and its result. */
  def entries(c: FenceSamples.Close): Vector[Entry] =
    c.lines.zipWithIndex.flatMap {
      case (FenceSamples.Line.Said(text), i) =>
        Vector(entry(i * 2L, Payload.Message(Message.User(text))))
      case (FenceSamples.Line.Read(shown, content), i) =>
        Vector(
          called(i * 2L, shown.takeWhile(_ != ' '), ujson.Obj()),
          result(i * 2L + 1, i * 2L, shown, content)
        )
      case (FenceSamples.Line.Replied(text), i) =>
        Vector(
          entry(
            i * 2L,
            Payload.Message(
              Message.Assistant(Vector(AssistantBlock.Text(text)), StopReason.EndTurn, NoUsage, "m")
            )
          )
        )
    }

  private def close(c: FenceSamples.Close): ModelRequest =
    ClosingSummary.request(
      PeriodTranscript.labelled(entries(c), Speakers.none, Persona.Grit),
      Balance.empty,
      Vector.empty,
      Asked.Every,
      false
    )

  private def json[T](p: Posed.Json[T]): ModelRequest =
    Shaping.request(p.system, p.messages, Posed.ReplyTool, p.reply.schema, StrictSchemas.Ignored)

  private def label(s: String): DocLabel =
    DocLabel.of(s).fold(e => throw new IllegalStateException(e), identity)
  private def text(s: String): DocText =
    DocText.of(s).fold(e => throw new IllegalStateException(e), identity)

  private def dream(d: FenceSamples.Dream): ModelRequest =
    Shaping.request(
      Dreaming.told(AttachAsk.system(d.excerpts.size, d.topics.nonEmpty)),
      Vector(
        Message.User(Dreaming.shown(d.excerpts, d.topics.map(t => (label("topic"), text(t))), 400))
      ),
      Posed.ReplyTool,
      AttachAsk.schema,
      StrictSchemas.Ignored
    )

  /** The thread the turn samples answer in. */
  val Thread: Origin = Origin.Slack("T1", "C1", "1700000000.000100")

  /** The tools a turn sample is offered: a file read, and a post anywhere. */
  val Tools: Vector[ToolSchema] = Vector(
    ToolSchema(
      "drive_read",
      "Read one file from the shared drive by its id.\nReturns its text.",
      ujson.Obj(
        "type" -> "object",
        "properties" -> ujson.Obj("id" -> ujson.Obj("type" -> "string")),
        "required" -> ujson.Arr("id"),
        "additionalProperties" -> false
      )
    ),
    ToolSchema(
      "slack_post",
      "Post a message to a Slack channel.",
      ujson.Obj(
        "type" -> "object",
        "properties" -> ujson.Obj(
          "channel" -> ujson.Obj("type" -> "string"),
          "text" -> ujson.Obj("type" -> "string")
        ),
        "required" -> ujson.Arr("channel", "text"),
        "additionalProperties" -> false
      )
    )
  )

  private def turn(t: FenceSamples.Turn): ModelRequest = {
    val asked = entry(0, Payload.Message(Message.User(t.asked)))
    val own = Vector(
      asked,
      called(1, "drive_read", ujson.Obj("id" -> "f1")),
      result(2, 1, t.shown, t.output)
    )
    val system = SystemPrompt
      .of(
        Vector(
          TurnPrompt.Base,
          TurnPrompt.Candour,
          TurnPrompt.Answering,
          TurnPrompt.Security,
          TurnPrompt.Material,
          TurnPrompt.edge(Thread)
        ) ++ TurnPrompt.destination(Thread)
      )
      .render
    TurnLoop.guided(
      ModelRequest(system, Shown.turn(own, Speakers(Map(asked.id -> "Ana"))), Tools),
      ToolGuidance.SystemLines
    )
  }
}
