package grit.turn

import grit.core.message.{Message, Usage}
import grit.core.stitch.{Link, StitchJson}
import grit.core.store.Payload
import grit.dbos.sql.TestTx

import utest.*

/** A Slack thread's first message, said to grit at a channel's top level, placed by its own
  * workflow, which the turn waits for before its topics are placed (ADR 0023).
  */
object TurnStitchTests extends TestSuite {
  import TurnFixtures.*

  /** What a turn records from its offer on, patch markers left out. */
  private def ran(durable: grit.core.durable.InMemoryDurable, ch: StitchChannel): Vector[String] =
    durable.recordedSteps(ch.turn.workflowId).filterNot(_.startsWith("DBOS.")).drop(2)

  val tests = Tests {
    test(
      "an addressed first message waits, before its topics, for its placement, which follows the exchange"
    ) {
      val ch = new StitchChannel(Payload.Message(Message.User("@grit is this a real question?")))
      val jev = new FirstOption
      val (durable, _) = ch.run(jev)
      ran(durable, ch).take(2) ==> Vector("stitched", "classify")
      ch.waited.map(_.ref.turn) ==> Vector(ch.turn)
      ch.stitches.links(Vector(ch.b))(using TestTx.fake) ==> Right(Vector(Link(ch.b, ch.a)))
    }

    test("a first message already placed, as its triage placed a heard one, is not asked again") {
      val ch = new StitchChannel(Payload.Heard("Is this a real question"))
      ch.stitches.record(ch.root.id, kept, StitchedAt)(using TestTx.fake) ==> Right(true)
      val jev = new FirstOption
      val (durable, _) = ch.run(jev)
      ran(durable, ch).take(2) ==> Vector("stitched", "classify")
      ch.stitches.placed(ch.root.id)(using TestTx.fake) ==> Right(Some(kept))
      jev.calls ==> 0
    }

    test("a turn that passed the change before it shipped places its first message itself") {
      val ch = new StitchChannel(Payload.Message(Message.User("@grit is this a real question?")))
      val jev = new FirstOption
      val (durable, _) = ch.run(jev, Set(Turn.Patches.StitchInRoomOrder))
      ran(durable, ch).take(3) ==> Vector("stitch", "record-stitch", "classify")
      ch.waited ==> Vector.empty
      ch.stitches.links(Vector(ch.b))(using TestTx.fake) ==> Right(Vector(Link(ch.b, ch.a)))
    }
  }

  /** A placement kept already: beginning something new, shown nothing. */
  private val kept = StitchJson
    .read(
      ujson.Obj(
        "kind" -> "begins",
        "p" -> 0.2,
        "model" -> "jev",
        "usage" -> grit.core.store.PayloadJson.writeUsage(Usage.Zero),
        "seen" -> ujson.Obj(
          "state" -> ujson.Obj(),
          "offered" -> ujson.Arr(),
          "tuning" -> ujson.Obj(
            "horizon_seconds" -> 604800,
            "recent" -> 2,
            "lexical" -> 2,
            "follows_at" -> 0.6,
            "window_tokens" -> 1500,
            "strand_chars" -> 800
          )
        )
      )
    )
    .fold(e => sys.error(e), identity)
}
