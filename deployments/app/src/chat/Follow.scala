package grit.app.chat

import scala.math.Ordering.Implicits.infixOrderingOps

import grit.core.id.{EntrySeq, TurnRef, TurnSeq}
import grit.core.message.Message
import grit.core.store.{Entry, Payload}
import grit.dbos.engine.TurnStatus
import grit.turn.Turn

/** What following a conversation has seen so far: the last entry shown, the step of the
  * turn in progress (`None` when none was), the turns whose failure has been reported,
  * whether it has `looked` at all, and the call the turn in progress was `asking` about.
  */
final case class Follow(
    lastSeq: Option[EntrySeq],
    step: Option[String],
    reported: Set[TurnSeq],
    looked: Boolean = false,
    asking: Option[ChatScreen.Asked] = None
) {
  def thinking: Boolean = step.nonEmpty
}

object Follow {

  val start: Follow = Follow(None, None, Set.empty)

  /** One look at the conversation: what the screen should be told, given every entry in
    * it now and where a turn's workflow is. The first look always says what it saw, even
    * nothing, so the screen knows the conversation is read. New entries are shown once,
    * in order. A turn
    * is in progress while its user message has no reply and its workflow is running, and
    * the screen is told each step it moves to ([[Turn.running]]), and each summary as it
    * is written. One
    * that finished with no reply failed, and is reported once, with its outcome. While the
    * turn in progress has asked about a call ([[Payload.Ask]]) that has neither begun nor
    * come to a result, the screen is told it is asking, and told again when that changes.
    * A tool call shows once it has a result, on one line with it ([[Replies.settled]]), and
    * a closed period as its divider ([[Replies.closed]]).
    */
  def step(
      state: Follow,
      entries: Vector[Entry],
      status: TurnRef => TurnStatus
  ): (Follow, Vector[ChatScreen.Msg]) = {
    val fresh = entries.filter(e => state.lastSeq.forall(_ < e.seq))
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
    val asking = open.filter(_ => step.nonEmpty).flatMap(asked(entries, _))
    val next = Follow(
      fresh.lastOption.map(_.seq).orElse(state.lastSeq),
      step,
      state.reported ++ failure.map(_._1),
      looked = true,
      asking
    )
    val arrived =
      Option.when(!state.looked || said.nonEmpty || summaries.nonEmpty || step != state.step)(
        ChatScreen.Msg.Arrived(said, step, summaries)
      )
    val ask = Option.when(asking != state.asking)(ChatScreen.Msg.Asking(asking))
    (next, arrived.toVector ++ ask.toVector ++ failure.map(f => ChatScreen.Msg.Failed(f._2)))
  }

  /** The first call `turn` asked about, among `entries`, that has neither begun nor a result. */
  private def asked(entries: Vector[Entry], turn: TurnRef): Option[ChatScreen.Asked] = {
    val own = entries.filter(_.turnSeq == turn.turnSeq)
    val moved = own.flatMap {
      _.payload match {
        case Payload.Result(Message.ToolResult(call, _, _), _) => Some(call)
        case Payload.Attempt(call) => Some(call)
        case _ => None
      }
    }.toSet
    own.collectFirst {
      case Entry(_, _, _, _, _, Payload.Ask(call, shown), _) if !moved.contains(call) =>
        ChatScreen.Asked(turn.workflowId, call, shown)
    }
  }

  private def isUser(e: Entry): Boolean = e.payload match {
    case Payload.Message(Message.User(_)) => true
    case _ => false
  }

  private def isReply(e: Entry): Boolean = e.payload match {
    case Payload.Message(Message.Assistant(_, _, _, _, _)) => true
    case _ => false
  }

  private def said1(e: Entry): Option[ChatScreen.Said] = {
    val voice = if (isUser(e)) ChatScreen.Voice.User else ChatScreen.Voice.Reply
    Replies
      .text(e)
      .map(ChatScreen.Said(voice, _, e.turnSeq))
      .orElse(Replies.settled(e).map(ChatScreen.Said(ChatScreen.Voice.Tool, _, e.turnSeq)))
      .orElse(Replies.closed(e).map(ChatScreen.Said(ChatScreen.Voice.Closed, _, e.turnSeq)))
  }
}
