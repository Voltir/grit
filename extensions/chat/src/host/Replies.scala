package grit.chat.host

import grit.core.message.Message
import grit.core.period.{CloseReason, Probability}
import grit.core.store.{Entry, Payload, WentOn}
import grit.prose.words.Words

/** The transcript's lines for entries other than messages: a tool call's result, a closed
  * period's divider and grit's going on with a task. A message's own text is
  * [[grit.core.edge.Reply.text]].
  */
object Replies {

  /** grit's continuation note as the transcript says it: that grit went on with the task in
    * a new turn, which turn of it, and why. `None` for any other entry.
    */
  def wentOn(entry: Entry): Option[String] = entry.payload match {
    case Payload.Continuation(_, turn, WentOn.AtLimit) =>
      Some(s"── grit went on (turn $turn): the last turn reached its limit ──")
    case Payload.Continuation(_, turn, _: WentOn.CheckFailed) =>
      Some(s"── grit went on (turn $turn): the check failed ──")
    case _ => None
  }

  /** A closing entry as the transcript's divider says it: why its period closed (`resolved
    * (0.86)`, its confidence to two places, `lapsed`, `unearned`, `ran` or `rolled`), and its
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
        case CloseReason.Ran => "ran"
        case CloseReason.Rolled => "rolled"
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
        else s"← ${Words.counted(lines.size, "line")}"
      Some(s"$call $came")
    case _ => None
  }
}
