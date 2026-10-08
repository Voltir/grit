package grit.tools

import scala.NamedTuple

import grit.core.identity.Evidence
import grit.core.store.{Askers, Db, Tx}
import grit.core.tool.{Args, Gate, Hosted, Outcome, Retry, Tool, ToolName, ToolSpec}
import grit.core.visibility.{Explanation, GroupName, Label, Subject}

/** `clearance`: what the person asking is cleared for, and why. It takes no argument, so it
  * concerns only the asker. Each call reads in one transaction opened for the call's turn
  * ([[grit.core.visibility.Subject.Turn]]). In a direct message
  * ([[grit.core.place.Place.direct]]) it answers [[render]] of
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
              askers.of(at.turn).map(asker => render(Tx.explain(asker, Tx.floor(tx))))
            case Some(own) =>
              Right(s"This room is labelled ${label(own.label)}. $OwnClearanceNote")
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

  /** `e` in plain words: the room's label, what the asker reads beyond it, who they are taken
    * to be (each account by its kind alone), the groups named (each by how they are in it:
    * named, a full member, or added through grit), the two rules (a room's members
    * read its own speech up to its label; anything said elsewhere is read there up to the
    * room's label met with the asker's clearance), and that it never says whether anything is
    * hidden. It names nothing `e` does not hold.
    */
  def render(e: Explanation): String =
    Vector(
      s"This conversation is labelled ${label(e.room)}.",
      s"From anywhere else, I read for you up to ${label(e.beyond)}.",
      who(e.asker),
      groups(e.groups),
      Rules,
      Hidden
    ).mkString("\n\n")

  /** How a room's speech and everything else is read, and how a label is written. */
  private val Rules: String =
    "How it works: what is said in a room is read there, by its members, up to the room's " +
      "label. Anything said elsewhere is read here up to this room's label met with your " +
      "clearance. A label is shown as its level, then the compartments it is in."

  /** The line every answer ends with. */
  private val Hidden: String = "I never say whether anything is hidden from you."

  private def label(l: Label): String = Label.shown(l)

  private def who(asker: Explanation.Asker): String =
    asker match {
      case Explanation.Asker.Nobody =>
        "No one I know asked this, so I tell no one's clearance."
      case Explanation.Asker.Grit =>
        "This turn is my own, not a person's, so there is no person's clearance to tell."
      case Explanation.Asker.Person(accounts) =>
        "I know you by " + accounts.map(shown).mkString("; ") + "."
    }

  private def shown(s: Explanation.Shown): String = {
    val how = s.evidence match {
      case Evidence.Home => "your own"
      case Evidence.Vouched => "linked to you by an email a trusted source confirmed"
    }
    val member = if (s.member) ", a full member of its source" else ""
    s"a ${Explanation.Namespace.value(s.namespace)} account, $how$member"
  }

  private def groups(in: Vector[Explanation.In]): String =
    if (in.isEmpty) "You are in no group that clears you for anything here."
    else
      ("Your groups, each with what it clears you for:" +: in.map { i =>
        val ways =
          i.through.toVector.sorted
            .map(n => s"through your ${Explanation.Namespace.value(n)} account") ++
            i.members.toVector.sorted
              .map(n => s"as a full member of a ${Explanation.Namespace.value(n)} source") ++
            i.added.toVector.sorted
              .map(n => s"added through grit for your ${Explanation.Namespace.value(n)} account")
        s"- ${GroupName.value(i.group)}: ${label(i.label)}, ${ways.mkString(" and ")}"
      }).mkString("\n")
}
