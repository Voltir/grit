package grit.app.chat

import grit.core.id.{TurnRef, TurnSeq}
import grit.core.message.Message
import grit.core.store.{Entry, Payload}
import grit.dbos.engine.TurnStatus
import grit.turn.Turn

/** What following a conversation has seen so far: the last entry shown, the step of the
  * turn in progress (`None` when none was), and the turns whose failure has been reported.
  */
final case class Follow(lastSeq: Long, step: Option[String], reported: Set[TurnSeq]) {
  def thinking: Boolean = step.nonEmpty
}

object Follow {

  val start: Follow = Follow(-1L, None, Set.empty)

  /** One look at the conversation: what the screen should be told, given every entry in
    * it now and where a turn's workflow is. New entries are shown once, in order. A turn
    * is in progress while its user message has no reply and its workflow is running, and
    * the screen is told each step it moves to ([[Turn.running]]), and each summary as it
    * is written. One
    * that finished with no reply failed, and is reported once, with its outcome.
    */
  def step(
      state: Follow,
      entries: Vector[Entry],
      status: TurnRef => TurnStatus
  ): (Follow, Vector[ChatScreen.Msg]) = {
    val fresh = entries.filter(_.seq > state.lastSeq)
    val said = fresh.flatMap(said1)
    val summaries = fresh.collect { case Entry(_, _, t, _, _, Payload.Summary(text), _) =>
      ChatScreen.Summarised(t, text)
    }
    val replied = entries.collect { case e if isReply(e) => e.turnSeq }.toSet
    val open = entries
      .filter(isUser)
      .lastOption
      .filterNot(e => replied.contains(e.turnSeq))
      .map(e => TurnRef(e.conversationId, e.turnSeq))
    val (step, failure) = open.map(t => (t, status(t))) match {
      case Some((_, TurnStatus.Running(steps))) => (Some(Turn.running(steps.map(_.name))), None)
      case Some((t, TurnStatus.Finished(outcome))) if !state.reported.contains(t.turnSeq) =>
        (None, Some(t.turnSeq -> outcome))
      case _ => (None, None)
    }
    val next = Follow(
      fresh.lastOption.fold(state.lastSeq)(_.seq),
      step,
      state.reported ++ failure.map(_._1)
    )
    val arrived =
      Option.when(said.nonEmpty || summaries.nonEmpty || step != state.step)(
        ChatScreen.Msg.Arrived(said, step, summaries)
      )
    (next, arrived.toVector ++ failure.map(f => ChatScreen.Msg.Failed(f._2)))
  }

  private def isUser(e: Entry): Boolean = e.payload match {
    case Payload.Message(Message.User(_)) => true
    case _ => false
  }

  private def isReply(e: Entry): Boolean = e.payload match {
    case Payload.Message(Message.Assistant(_, _, _, _)) => true
    case _ => false
  }

  private def said1(e: Entry): Option[ChatScreen.Said] =
    Replies.text(e).map(ChatScreen.Said(isUser(e), _, e.turnSeq))
}
