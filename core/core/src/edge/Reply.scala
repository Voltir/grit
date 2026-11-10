package grit.core.edge

import grit.core.id.TurnRef
import grit.core.message.{AssistantBlock, Message}
import grit.core.store.{Entry, Payload}
import grit.core.visibility.Subject

/** A turn's reply read back out of the store, the way an edge does (ADR 0002). */
object Reply {

  /** The text of `turn`'s recorded reply; `None` if it has none (the turn failed). */
  def of(engine: Console^, turn: TurnRef): Either[String, Option[String]] =
    engine.db
      .read(Subject.Turn(turn))(engine.entries.get(turn.replyId))
      .left
      .map(e => s"unreadable: $e")
      .map(_.map(entry => text(entry).getOrElse("(not a reply)")))

  /** What `entry` said, as the transcript shows it: the user's text, or a reply's text
    * blocks. `None` for an entry the transcript does not show.
    */
  def text(entry: Entry): Option[String] = entry.payload match {
    case Payload.Message(Message.User(text)) => Some(text)
    case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
      val said = AssistantBlock.text(blocks)
      Some(if (said.isEmpty) "(no text in the reply)" else said)
    case Payload.Message(Message.ToolResult(_, _, _)) => None
    // Heard messages and posts begin and fill Slack threads, never a terminal's session.
    case Payload.Heard(_) | Payload.Posted(_) | Payload.Summary(_) | Payload.Query(_) |
        (_: Payload.Window) | (_: Payload.Prefix) | Payload.Agenda(_) | Payload.Exchange(_) |
        Payload.Result(_, _) | Payload.Declined(_, _) | Payload.Attempt(_) | Payload.Ask(_, _, _) |
        Payload.Closed(_, _, _) | Payload.Draft(_) | Payload.Continuation(_, _, _) =>
      None
  }
}
