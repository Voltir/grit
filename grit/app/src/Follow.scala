package grit.app

import grit.core.{Entry, Message, Payload, TurnRef, TurnSeq}
import grit.dbos.TurnStatus

/** What following a conversation has seen so far: the last entry shown, whether a turn
  * was in progress, and the turns whose failure has been reported.
  */
final case class Follow(lastSeq: Long, thinking: Boolean, reported: Set[TurnSeq])

object Follow {

  val start: Follow = Follow(-1L, thinking = false, Set.empty)

  /** One look at the conversation: what the screen should be told, given every entry in
    * it now and where a turn's workflow is. New entries are shown once, in order. A turn
    * is in progress while its user message has no reply and its workflow is running. One
    * that finished with no reply failed, and is reported once, with its outcome.
    */
  def step(
      state: Follow,
      entries: Vector[Entry],
      status: TurnRef => TurnStatus
  ): (Follow, Vector[ChatScreen.Msg]) = {
    val fresh = entries.filter(_.seq > state.lastSeq)
    val said = fresh.flatMap(said1)
    val replied = entries.collect { case e if isReply(e) => e.turnSeq }.toSet
    val open = entries
      .filter(isUser)
      .lastOption
      .filterNot(e => replied.contains(e.turnSeq))
      .map(e => TurnRef(e.conversationId, e.turnSeq))
    val (thinking, failure) = open.map(t => (t, status(t))) match {
      case Some((_, TurnStatus.Running)) => (true, None)
      case Some((t, TurnStatus.Finished(outcome))) if !state.reported.contains(t.turnSeq) =>
        (false, Some(t.turnSeq -> outcome))
      case _ => (false, None)
    }
    val next = Follow(
      fresh.lastOption.fold(state.lastSeq)(_.seq),
      thinking,
      state.reported ++ failure.map(_._1)
    )
    val arrived =
      Option.when(said.nonEmpty || thinking != state.thinking)(
        ChatScreen.Msg.Arrived(said, thinking)
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
    Replies.text(e).map(ChatScreen.Said(isUser(e), _))
}
