package grit.tools

import scala.NamedTuple

import grit.core.store.{Askers, Db, Tx}
import grit.core.tool.{Args, Gate, Hosted, Outcome, Retry, Tool, ToolName, ToolSpec}
import grit.core.visibility.{Explanation, Label, Subject}

/** `clearance`: what the person asking is cleared for, and why. It takes no argument, so it
  * concerns only the asker. Each call reads in one transaction opened for the call's turn
  * ([[grit.core.visibility.Subject.Turn]]). In a direct message
  * ([[grit.core.place.Place.direct]]) it answers [[Explanation.text]] of
  * [[grit.core.store.Tx.explain]] of the turn's asker
  * ([[grit.core.store.Askers.of]]) and the label that transaction writes at (`Tx.floor`: the
  * thread's label met with the person's clearance now). Anywhere else it tells the model the
  * room's own label, which everyone in the room reads at and so is no secret, then
  * [[OwnClearanceNote]]: the same whoever asks, naming nothing of the asker's clearance, and
  * recording nothing; a turn with no room of its own is told [[OwnClearanceNote]] alone.
  */
object Cleared {

  /** Its tool's name, `clearance`. */
  val Name: ToolName = ToolName("clearance")

  /** The fixed line that ends the answer outside a direct message, addressed to the model that
    * reads the result, not written as a reply to the person: it says when to mention that a
    * person's own clearance is told only in a direct message.
    */
  val OwnClearanceNote: String =
    "A person's own clearance is told only in a direct message with them; say so only if they " +
      "asked about their own clearance."

  /** The answer when the store cannot be read: it names nothing of what failed. */
  val Unread: String = "I could not read your clearance just now. Ask again in a moment."

  /** `clearance`, explaining the asker `askers` resolves in a direct message, and outside one
    * telling the model the room's label (e.g. "This room is labelled [level: internal].") then
    * [[OwnClearanceNote]]; each call read from `store`; an error [[Outcome]], [[Unread]], when
    * that read fails.
    */
  def tool(askers: Askers, store: Db^): Tool[Unit]^{store} =
    described.calling((_, at) =>
      store
        .read(Subject.Turn(at.turn)) { (tx: Tx^) ?=>
          Tx.clearance(tx).own match {
            case Some(own) if own.room.direct =>
              askers.of(at.turn).map(asker => Explanation.text(Tx.explain(asker, Tx.floor(tx))))
            case Some(own) =>
              Right(s"This room is labelled ${Label.shown(own.label)}. $OwnClearanceNote")
            case None => Right(OwnClearanceNote)
          }
        }
        .fold(_ => Outcome.Failed(Unread), Outcome.Done(_))
    )

  /** What the model is told of it. */
  private val described: Hosted[Unit] =
    Hosted(
      ToolSpec(
        Name,
        "What the person asking is cleared for, and why: the labels they read here and " +
          "elsewhere, and the groups that clear them. Call it when the person asks what they " +
          "can see, what they are cleared for, or why something is not shown to them. This " +
          "room's own label is in your instructions: answer a question about it directly. " +
          "It tells a person's clearance only in a direct message with them.",
        Args.of(NamedTuple.Empty).map(_ => ()),
        retry = Retry.Rerun
      ),
      Gate.Free,
      _ => ""
    )
}
