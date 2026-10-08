package grit.tools

import scala.NamedTuple

import grit.core.identity.Evidence
import grit.core.store.{Askers, Db, Tx}
import grit.core.tool.{Args, Gate, Hosted, Outcome, Retry, Tool, ToolName, ToolSpec}
import grit.core.visibility.{Explanation, GroupName, Label, Subject, Visibility}

/** `clearance`: what the person asking is cleared for, and why. It takes no argument, so it
  * concerns only the asker. Each call reads in one transaction opened for the call's turn
  * ([[grit.core.visibility.Subject.Turn]]). In a direct message
  * ([[grit.core.place.Place.direct]]) it answers [[render]] of
  * [[grit.core.visibility.Visibility.explain]] of the turn's asker
  * ([[grit.core.store.Askers.of]]) and the label that transaction writes at (`Tx.floor`: the
  * thread's label met with the person's clearance now). Anywhere else it answers
  * [[OnlyDirect]], whoever asks, and records nothing.
  */
object Cleared {

  /** Its tool's name, `clearance`. */
  val Name: ToolName = ToolName("clearance")

  /** The answer outside a direct message. */
  val OnlyDirect: String =
    "I tell people their clearance only in a direct message to me. Write to me there and ask " +
      "again."

  /** The answer when the store cannot be read: it names nothing of what failed. */
  val Unread: String = "I could not read your clearance just now. Ask again in a moment."

  /** `clearance`, explaining the asker `askers` resolves under `visibility` (the engine's), each
    * call read from `store`; an error [[Outcome]], [[Unread]], when that read fails.
    */
  def tool(askers: Askers, visibility: Visibility, store: Db^): Tool[Unit]^{store} =
    described.calling((_, at) =>
      store
        .read(Subject.Turn(at.turn)) { (tx: Tx^) ?=>
          Tx.clearance(tx).own match {
            case Some(own) if own.room.direct =>
              askers.of(at.turn).map(asker => render(visibility.explain(asker, Tx.floor(tx))))
            case _ => Right(OnlyDirect)
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
          "can see, what they are cleared for, or why something is not shown to them. It " +
          "answers only in a direct message with them; anywhere else it says so.",
        Args.of(NamedTuple.Empty).map(_ => ()),
        retry = Retry.Rerun
      ),
      Gate.Free,
      _ => ""
    )

  /** `e` in plain words: the room's label, what the asker reads beyond it, who they are taken
    * to be (each account by its kind alone), the groups named, the two rules (a room's members
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
      "clearance. A label is written as its level, then its compartments, joined by +."

  /** The line every answer ends with. */
  private val Hidden: String = "I never say whether anything is hidden from you."

  private def label(l: Label): String = Label.written(l)

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
              .map(n => s"as a full member of a ${Explanation.Namespace.value(n)} source")
        s"- ${GroupName.value(i.group)}: ${label(i.label)}, ${ways.mkString(" and ")}"
      }).mkString("\n")
}
