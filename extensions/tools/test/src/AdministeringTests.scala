package grit.tools

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.admin.{
  AdministrationContract,
  Answer,
  Change,
  Command,
  InMemoryAdministration,
  Refusal
}
import grit.core.approval.Approval
import grit.core.clock.Clock
import grit.core.flow.TestTainted
import grit.core.id.{ConversationId, TestCallSlots, ToolCallId, TurnRef, TurnSeq}
import grit.core.message.AssistantBlock
import grit.core.store.{InMemoryVoucher, Origin}
import grit.core.tool.{Believed, Bound, Outcome, Repairs, Toolbox}
import grit.core.visibility.RoomAccess

import utest.*

object AdministeringTests extends TestSuite {
  import AdministrationContract.{Declared, Kept, T1, gus, mia, publicRoom}

  private val now = Instant.parse("2026-10-21T19:00:00Z")

  /** The time, stopped at [[now]]. */
  private final class Stopped extends Clock {
    def now(): Instant = AdministeringTests.now
    def millis(): Long = 0L
    def sleep(duration: FiniteDuration): Unit = ()
  }

  /** An administration under [[Declared]] with mia a full member and gus seen, the public room
    * reported open, and a turn mia asked in it.
    */
  private final class World {
    val admin: InMemoryAdministration^ =
      new InMemoryAdministration(Declared, new InMemoryVoucher(Set(T1), Set.empty, Declared))
    admin.member(mia)
    admin.voucher.saw(gus)
    admin.reported(publicRoom, RoomAccess.Open)
    val turn: TurnRef = TurnRef(ConversationId("asked"), TurnSeq(1))
    admin.began(turn, Origin.Slack("T1", "C-public", "1.0"), Some(mia))
    val clock: Clock^ = new Stopped
    val box: Toolbox[caps.CapSet^{admin, clock}] =
      Toolbox
        .of[caps.CapSet^{admin, clock}](Administering.tools(admin, clock)*)
        .fold(d => sys.error(d.toString), identity)

    def bind(tool: String, args: (String, ujson.Value)*) =
      box.bind(
        TestTainted.authority(
          AssistantBlock.ToolCall(ToolCallId("c1"), tool, ujson.Obj.from(args))
        ),
        Repairs.All
      )

    def gated(tool: String, args: (String, ujson.Value)*): Bound.Gated =
      bind(tool, args*) match {
        case Right(g: Bound.Gated) => g
        case other => sys.error(s"not gated: $other")
      }
  }

  val tests = Tests {
    test("label_room asks first, shows the change in words, and waits 15 minutes") {
      val w = new World
      val label = w.gated("label_room", "label" -> "confidential finance")
      val unlabel = w.gated("label_room", "label" -> "default")
      val quiet = w.gated("quiet_room", "on" -> true)
      val speak = w.gated("quiet_room", "on" -> false)
      (
        Vector(label, unlabel, quiet, speak).map(_.ask),
        Vector(label, unlabel, quiet, speak).map(_.within),
        w.admin.kept
      ) ==> (
        Vector(
          "Set this room's label to confidential, finance.",
          "Set this room's label back to its default.",
          "Make this room quiet: grit posts nothing here unasked.",
          "Let grit post in this room unasked again."
        ),
        Vector.fill(4)(15.minutes),
        Vector.empty
      )
    }

    test(
      "on approval it runs as the turn's asker in its room, and answers with the change's words"
    ) {
      val w = new World
      val at = TestCallSlots.at(w.turn)
      val on = new Change.Quiet(publicRoom, true)
      (
        w.gated("quiet_room", "on" -> true)(Approval.Declined(None), at),
        w.admin.kept,
        w.gated("quiet_room", "on" -> true)(Approval.Approved, at),
        w.gated("label_room", "label" -> "internal")(Approval.Approved, at),
        w.admin.kept
      ) ==> (
        Outcome.Declined(None),
        Vector.empty,
        Outcome.Done(Answer.quieted(on).text),
        // A member relabels no public room: the refusal's words, as the slash command's.
        Outcome.Failed(Refusal.PublicRoom.text),
        Vector(Kept(mia, now, on, Some(w.turn.workflowId)))
      )
    }

    test("a label that does not read is refused before anyone is asked") {
      val w = new World
      def refused(words: String) = w.bind("label_room", "label" -> words).left.map(_.message)
      def sent(words: String) = " You sent: " + ujson.Obj("label" -> words).render()
      val not = "The call to `label_room` was not run: "
      (refused("secret"), refused("internal at slack:T1/C-other"), refused(" "), w.admin.kept) ==> (
        Left(
          not + "secret: not a level; the levels are public, internal, confidential and " +
            s"restricted\n${Command.Usage}" + sent("secret")
        ),
        Left(
          not + "label_room labels only the room this conversation is in; it names no other place." +
            sent("internal at slack:T1/C-other")
        ),
        Left(
          not + "label_room takes a level, then any compartments, or default." + sent(" ")
        ),
        Vector.empty
      )
    }

    test("both say what they did, or why not, as grit's own words") {
      val w = new World
      Administering.tools(w.admin, w.clock).map(_.output) ==> Vector(Believed.Own, Believed.Own)
    }
  }
}
