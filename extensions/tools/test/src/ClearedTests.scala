package grit.tools

import grit.core.id.{TestCallSlots, ToolCallId, TurnRef}
import grit.core.identity.{Principal, TestAccounts}
import grit.core.message.AssistantBlock
import grit.core.place.Place
import grit.core.store.{Askers, Db, Origin, StoreError, Tx}
import grit.core.tool.{Bound, Outcome, Repairs, Toolbox}
import grit.core.visibility.{Clearance, Explained, Explanation, Label, Level, Subject, Visibility}
import grit.dbos.sql.TestTx

import utest.*

/** [[Cleared]]: the asker's clearance in plain words in a direct message, and anywhere else the
  * room's label with one fixed line for the model.
  */
object ClearedTests extends TestSuite {

  import Explained.{After, Before, Dana, cleared, confidentialTrial, explain}

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
  ): Outcome =
    calledUnder(visibility, Clearance.inRoom(room, label, asker), down)

  /** As [[called]], for a read opened under `clearance`. */
  private def calledUnder(
      visibility: Visibility,
      clearance: Clearance,
      down: Boolean = false
  ): Outcome = {
    val at = TestCallSlots.First
    val store: Db^ = new Db {
      def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
        if (down || subject != Subject.Turn(at.turn)) Left(StoreError.DatabaseError(s"$subject"))
        else body(using TestTx.fake(clearance, visibility))
    }
    Toolbox
      .of(Cleared.tool(danaAsks, store))
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
      val told = text(called(After, direct, floor, cleared(After, Dana)))
      (
        told.linesIterator.toVector.headOption,
        told == Explanation.text(explain(After, Some(Dana), floor)),
        Vector(
          "what is said in a room is read there, by its members, up to the room's label",
          "Anything said elsewhere is read here up to this room's label met with your clearance",
          "I never say whether anything is hidden from you."
        ).filterNot(told.contains)
      ) ==> (Some("This conversation is labelled [level: internal]."), true, Vector())
    }

    test(
      "outside a direct message it tells the model the room's label and then one fixed line, whatever the visibility and whoever asks"
    ) {
      val told =
        "This room is labelled [level: confidential, in: {trial}]. A person's own clearance is " +
          "told only in a direct message with them; say so only if they asked about their own " +
          "clearance."
      (
        called(Before, channel, confidentialTrial, cleared(Before, Dana)),
        called(Visibility.Shipped, channel, confidentialTrial, confidentialTrial)
      ) ==> (Outcome.Done(told), Outcome.Done(told))
    }

    test("a turn with no room of its own tells the model the fixed line alone") {
      calledUnder(After, Clearance.of(confidentialTrial)) ==> Outcome.Done(
        "A person's own clearance is told only in a direct message with them; say so only if " +
          "they asked about their own clearance."
      )
    }

    test(
      "outside a direct message it never names the clearance of an asker cleared above the room"
    ) {
      val internal = Label.at(Level.Internal)
      val danas = cleared(After, Dana)
      val told = text(called(After, channel, internal, danas))
      (
        danas.dominates(internal) && danas != internal,
        told.contains(Label.shown(danas)),
        told.contains("trial")
      ) ==> (true, false, false)
    }

    test("a store it cannot read is an error that names nothing of what failed") {
      called(After, direct, confidentialTrial, confidentialTrial, down = true) ==>
        Outcome.Failed(Cleared.Unread)
    }
  }
}
