package grit.app.chat

import grit.core.id.TurnRef
import grit.core.message.{AssistantBlock, Message}
import grit.core.period.{CloseReason, Probability}
import grit.core.store.{Entry, Payload}
import grit.dbos.engine.Link

/** Reading replies back out of the store, the way an edge does (ADR 0002). */
object Replies {

  /** The text of `turn`'s recorded reply; `None` if it has none (the turn failed). */
  def of(engine: Link^, turn: TurnRef): Either[String, Option[String]] =
    engine.db
      .read(engine.entries.get(turn.replyId))
      .left
      .map(e => s"unreadable: $e")
      .map(_.map(entry => text(entry).getOrElse("(not a reply)")))

  /** What `entry` said, as the transcript shows it: the user's text, or a reply's text
    * blocks. `None` for an entry the transcript does not show.
    */
  def text(entry: Entry): Option[String] = entry.payload match {
    case Payload.Message(Message.User(text)) => Some(text)
    case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
      val said = blocks.collect { case AssistantBlock.Text(t) => t }.mkString
      Some(if (said.isEmpty) "(no text in the reply)" else said)
    case Payload.Message(Message.ToolResult(_, _, _)) => None
    // Heard messages and posts begin and fill Slack threads, never a terminal's session.
    case Payload.Heard(_) | Payload.Posted(_) | Payload.Summary(_) | Payload.Query(_) |
        Payload.Window(_, _, _) | Payload.Topic(_) | Payload.Exchange(_) | Payload.Result(_, _) |
        Payload.Attempt(_) | Payload.Ask(_, _) | Payload.Closed(_, _, _) | Payload.Draft(_) =>
      None
  }

  /** A closing entry as the transcript's divider says it: why its period closed (`resolved
    * (0.86)`, its confidence to two places, `lapsed` or `unearned`), and its
    * [[grit.core.period.Closing.headline]]. `None` for any other entry.
    */
  def closed(entry: Entry): Option[String] = entry.payload match {
    case Payload.Closed(_, reason, closing) =>
      val why = reason match {
        case CloseReason.Resolved(confidence) =>
          val two = BigDecimal(Probability.value(confidence))
            .setScale(2, BigDecimal.RoundingMode.HALF_UP)
          s"resolved ($two)"
        case CloseReason.Lapsed => "lapsed"
        case CloseReason.Unearned => "unearned"
      }
      Some(s"$why · ${closing.headline}")
    case _ => None
  }

  /** What a call of a turn's tool loop came to, as one line of the transcript, when `entry`
    * is its result: the call as the turn kept it ([[Payload.Result]]'s `shown`), then
    * `← n lines`, or `← ` and an error's first line. `None` for any other entry.
    */
  def settled(entry: Entry): Option[String] = entry.payload match {
    case Payload.Result(Message.ToolResult(_, content, isError), call) =>
      val lines = content.linesIterator.toVector
      val came =
        if (isError) s"← ${lines.headOption.getOrElse("failed")}"
        else s"← ${lines.size} ${if (lines.size == 1) "line" else "lines"}"
      Some(s"$call $came")
    case _ => None
  }
}
