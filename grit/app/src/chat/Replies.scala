package grit.app.chat

import grit.core.id.{ToolCallId, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Message}
import grit.core.store.{Entry, Payload}
import grit.dbos.engine.Engine
import grit.turn.Turn

/** Reading replies back out of the store, the way an edge does (ADR 0002). */
object Replies {

  /** The text of `turn`'s recorded reply; `None` if it has none (the turn failed). */
  def of(engine: Engine^, turn: TurnRef): Either[String, Option[String]] =
    engine.db
      .read(engine.entries.get(Turn.replyId(turn)))
      .left
      .map(e => s"unreadable: $e")
      .map(_.map(entry => text(entry).getOrElse("(not a reply)")))

  /** What `entry` said, as the transcript shows it: the user's text, or a reply's text
    * blocks. `None` for an entry the transcript does not show.
    */
  def text(entry: Entry): Option[String] = entry.payload match {
    case Payload.Message(Message.User(text)) => Some(text)
    case Payload.Message(Message.Assistant(blocks, _, _, _)) =>
      val said = blocks.collect { case AssistantBlock.Text(t) => t }.mkString
      Some(if (said.isEmpty) "(no text in the reply)" else said)
    case Payload.Message(Message.ToolResult(_, _, _)) => None
    case Payload.Summary(_) | Payload.Query(_) | Payload.Window(_, _) | Payload.Topic(_) |
        Payload.Exchange(_) | Payload.Attempt(_) | Payload.Ask(_, _) =>
      None
  }

  /** Every tool call the replies among `entries` made in their turns' loops, by turn and id. */
  def calls(entries: Vector[Entry]): Map[(TurnSeq, ToolCallId), AssistantBlock.ToolCall] =
    entries.flatMap { e =>
      e.payload match {
        case Payload.Exchange(Message.Assistant(blocks, _, _, _)) =>
          blocks.collect { case c: AssistantBlock.ToolCall => (e.turnSeq, c.id) -> c }
        case _ => Vector.empty
      }
    }.toMap

  /** What a call of a turn's tool loop came to, as one line of the transcript, when `entry`
    * is its result: the call as [[called]] shows it, found among `calls` ([[Replies.calls]]),
    * then `← n lines`, or `← ` and an error's first line; the call left out when it is not
    * among them. `None` for any other entry.
    */
  def settled(
      entry: Entry,
      calls: Map[(TurnSeq, ToolCallId), AssistantBlock.ToolCall]
  ): Option[String] = entry.payload match {
    case Payload.Exchange(Message.ToolResult(id, content, isError)) =>
      val lines = content.linesIterator.toVector
      val came =
        if (isError) s"← ${lines.headOption.getOrElse("failed")}"
        else s"← ${lines.size} ${if (lines.size == 1) "line" else "lines"}"
      Some(calls.get((entry.turnSeq, id)).fold(came)(c => s"${called(c)} $came"))
    case _ => None
  }

  /** `call` as the transcript shows it: its tool's name, then what it acts on. For `search`,
    * its pattern, quoted, and where; for any other tool, its `path`, else its `command`, else
    * its first text argument.
    */
  def called(call: AssistantBlock.ToolCall): String = {
    val args = call.arguments.objOpt.map(_.toMap).getOrElse(Map.empty[String, ujson.Value])
    def text(key: String): Option[String] = args.get(key).flatMap(_.strOpt)
    val first = call.arguments.objOpt.toVector.flatMap(_.values).collectFirst { case ujson.Str(s) =>
      s
    }
    val on = call.name match {
      case "search" =>
        text("pattern").map(p => s"\"$p\"").toVector ++ text("path").toVector
      case _ => text("path").orElse(text("command")).orElse(first).toVector
    }
    (call.name +: on).mkString(" ")
  }
}
