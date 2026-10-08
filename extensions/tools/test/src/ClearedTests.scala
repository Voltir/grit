package grit.tools

import grit.core.id.{TestCallSlots, ToolCallId, TurnRef}
import grit.core.identity.{Principal, TestAccounts}
import grit.core.message.AssistantBlock
import grit.core.place.Place
import grit.core.store.{Askers, Db, Origin, StoreError, Tx}
import grit.core.tool.{Bound, Outcome, Repairs, Toolbox}
import grit.core.visibility.{Clearance, Explained, Label, Level, Subject, Visibility}
import grit.dbos.sql.TestTx

import utest.*

/** [[Cleared]]: the asker's clearance in plain words in a direct message, and the room's label
  * with one fixed line anywhere else.
  */
object ClearedTests extends TestSuite {

  import Explained.{Above, After, Before, Dana, Named, confidentialTrial, names}

  /** Askers answering Dana for every turn. */
  private val danaAsks: Askers = new Askers {
    def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Principal]] = Right(Some(Dana))
  }

  private val direct: Place = Origin.Direct(TestAccounts.sourced("slack:T/U-dana"), "1.0").room

  private val channel: Place = Origin.Slack("T", "C", "1.0").room

  /** What a call of `clearance` at [[TestCallSlots.First]] comes to, under `visibility`, a read
    * for that call's turn opened in `room` at `label` for an asker cleared for `asker`; a read
    * for any other subject, or any when `down`, failing.
    */
  private def called(
      visibility: Visibility,
      room: Place,
      label: Label,
      asker: Label,
      down: Boolean = false
  ): Outcome = {
    val at = TestCallSlots.First
    val store: Db^ = new Db {
      def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
        if (down || subject != Subject.Turn(at.turn)) Left(StoreError.DatabaseError(s"$subject"))
        else body(using TestTx.fake(Clearance.inRoom(room, label, asker), visibility))
    }
    Toolbox
      .of(Cleared.tool(danaAsks, visibility, store))
      .fold(d => throw new java.lang.AssertionError(s"duplicate $d"), identity)
      .bind(
        AssistantBlock.ToolCall(ToolCallId("c1"), "clearance", ujson.Obj()),
        Repairs.All
      ) match {
      case Right(b: Bound.Free) => b(at)
      case other => throw new java.lang.AssertionError(s"not free: $other")
    }
  }

  private def text(o: Outcome): String = o match {
    case Outcome.Done(t) => t
    case other => throw new java.lang.AssertionError(s"not done: $other")
  }

  val tests = Tests {
    test(
      "in a direct message it answers the asker's explanation at the thread's floor, not their clearance, with the two rules and the hidden-content line"
    ) {
      // A thread labelled below what Dana is cleared for: its floor is its label.
      val floor = Label.at(Level.Internal)
      val told = text(called(After, direct, floor, After.cleared(Dana)))
      (
        told.linesIterator.toVector.headOption,
        told == Cleared.render(After.explain(Some(Dana), floor)),
        Vector(
          "what is said in a room is read there, by its members, up to the room's label",
          "Anything said elsewhere is read here up to this room's label met with your clearance",
          "I never say whether anything is hidden from you."
        ).filterNot(told.contains)
      ) ==> (Some("This conversation is labelled [level: internal]."), true, Vector())
    }

    test(
      "outside a direct message it answers the room's label and then one fixed line, whatever the visibility and whoever asks"
    ) {
      val internal = Label.at(Level.Internal)
      (
        called(Before, channel, internal, Before.cleared(Dana)),
        called(Visibility.Shipped, channel, internal, Label.Public)
      ) ==> (
        Outcome.Done(
          "This conversation is labelled [level: internal]. I tell people their own clearance " +
            "only in a direct message to me. Write to me there and ask again."
        ),
        Outcome.Done(
          "This conversation is labelled [level: internal]. I tell people their own clearance " +
            "only in a direct message to me. Write to me there and ask again."
        )
      )
    }

    test(
      "outside a direct message it never names the clearance of an asker cleared above the room"
    ) {
      val internal = Label.at(Level.Internal)
      val cleared = After.cleared(Dana)
      val told = text(called(After, channel, internal, cleared))
      (
        cleared.dominates(internal) && cleared != internal,
        told.contains(Label.shown(cleared)),
        told.contains("trial")
      ) ==> (true, false, false)
    }

    test("a store it cannot read is an error that names nothing of what failed") {
      called(After, direct, confidentialTrial, confidentialTrial, down = true) ==>
        Outcome.Failed(Cleared.Unread)
    }

    test(
      "the rendered text of a sealed or raised thread names no compartment or group above it, and no account's or realm's name"
    ) {
      val sealedText = Cleared.render(Explained.sealedThread)
      val raisedText = Cleared.render(Explained.raisedThread)
      val fullText = Cleared.render(Explained.fullClearance)
      (
        names(sealedText, Above*),
        names(raisedText, Above*),
        Vector(sealedText, raisedText, fullText).flatMap(names(_, Named*))
      ) ==> (Vector(), Vector(), Vector())
    }

    test("the rendered text shows each account by kind and how it is the person's") {
      Cleared.render(Explained.fullClearance) ==>
        """This conversation is labelled [level: confidential, in: {finance, trial}].
          |
          |From anywhere else, I read for you up to [level: confidential, in: {finance, trial}].
          |
          |I know you by a slack account, linked to you by an email a trusted source confirmed, a full member of its source; a test account, your own.
          |
          |Your groups, each with what it clears you for:
          |- trial: [level: confidential, in: {trial}], through your slack account and through your test account
          |- leadership: [level: confidential, in: {finance, trial}], through your slack account
          |- staff: [level: internal], as a full member of a slack source
          |
          |How it works: what is said in a room is read there, by its members, up to the room's label. Anything said elsewhere is read here up to this room's label met with your clearance. A label is shown as its level, then the compartments it is in.
          |
          |I never say whether anything is hidden from you.""".stripMargin
    }

    test("no one, and grit, are told as such") {
      (
        Cleared.render(Before.explain(None, Label.Public)).linesIterator.toVector.lift(4),
        Cleared
          .render(Before.explain(Some(Principal.Grit), Label.Public))
          .linesIterator
          .toVector
          .lift(4)
      ) ==> (
        Some("No one I know asked this, so I tell no one's clearance."),
        Some("This turn is my own, not a person's, so there is no person's clearance to tell.")
      )
    }
  }
}
