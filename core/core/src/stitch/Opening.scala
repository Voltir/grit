package grit.core.stitch

import grit.core.id.{StitchRef, TurnRef}
import grit.core.place.Place
import grit.core.store.{Conversation, Entry}

/** A stitchable conversation's first message, said by a person: the one message of a
  * conversation that is placed (ADR 0023), in `room`. Only [[Opening.of]] makes one.
  */
final case class Opening private (ref: StitchRef, room: Place)

object Opening {

  /** `turn`'s message as an opening, when it is the first of `entries`, all of
    * `conversation`'s kept so far, a person said it, and `conversation`'s origin is
    * stitchable; `None` otherwise.
    */
  def of(conversation: Conversation, entries: Vector[Entry], turn: TurnRef): Option[Opening] =
    entries
      .minByOption(_.seq)
      .filter(first =>
        turn.conversationId == conversation.id && first.turnSeq == turn.turnSeq &&
          first.payload.said.nonEmpty && conversation.origin.stitchable
      )
      .map(first => Opening(StitchRef(turn, first.createdAt), conversation.origin.room))
}
