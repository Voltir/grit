package grit.app.chat

import grit.core.id.TurnRef
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

  /** What `entry` did in its turn's tool loop, as one line of the transcript: the calls a
    * reply made, each as its tool's name and its first text argument (`read grit/x.scala`),
    * joined by ` · `; or what a call came to: `← n lines`, or `← ` and an error's first
    * line. `None` for an entry outside the loop, and for a reply that made no call.
    */
  def exchange(entry: Entry): Option[String] = entry.payload match {
    case Payload.Exchange(Message.Assistant(blocks, _, _, _)) =>
      val calls = blocks.collect { case AssistantBlock.ToolCall(_, name, arguments) =>
        val shown = arguments.objOpt.toVector.flatMap(_.values).collectFirst { case ujson.Str(s) =>
          s
        }
        (name +: shown.toVector).mkString(" ")
      }
      Option.when(calls.nonEmpty)(calls.mkString(" · "))
    case Payload.Exchange(Message.ToolResult(_, content, isError)) =>
      val lines = content.linesIterator.toVector
      if (isError) Some(s"← ${lines.headOption.getOrElse("failed")}")
      else Some(s"← ${lines.size} ${if (lines.size == 1) "line" else "lines"}")
    case _ => None
  }
}
