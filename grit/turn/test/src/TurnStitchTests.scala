package grit.turn

import grit.core.message.{Message, Usage}
import grit.core.stitch.{Link, StitchJson}
import grit.core.store.Payload
import grit.dbos.sql.TestTx

import utest.*

/** A Slack thread's first message, said to grit at a channel's top level, stitched to the
  * exchange it follows before its topics are placed (ADR 0023).
  */
object TurnStitchTests extends TestSuite {
  import TurnFixtures.*

  val tests = Tests {
    test(
      "an addressed first message is stitched once, before its topics, to the exchange it follows"
    ) {
      val ch = new StitchChannel(Payload.Message(Message.User("@grit is this a real question?")))
      val jev = new FirstOption
      val (durable, _) = ch.run(jev)
      durable.recordedSteps(ch.turn.workflowId).filterNot(_.startsWith("DBOS.")).slice(2, 5) ==>
        Vector("stitch", "record-stitch", "classify")
      ch.stitches.links(Vector(ch.b))(using TestTx.fake) ==> Right(Vector(Link(ch.b, ch.a)))
      jev.calls ==> 1
    }

    test("a first message already placed, as its triage placed a heard one, is not asked again") {
      val ch = new StitchChannel(Payload.Heard("Is this a real question"))
      val kept = StitchJson
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
      ch.stitches.record(ch.root.id, kept, StitchedAt)(using TestTx.fake) ==> Right(true)
      val jev = new FirstOption
      val (durable, _) = ch.run(jev)
      durable
        .recordedSteps(ch.turn.workflowId)
        .filterNot(_.startsWith("DBOS."))
        .slice(2, 4) ==> Vector("stitch", "classify")
      ch.stitches.placed(ch.root.id)(using TestTx.fake) ==> Right(Some(kept))
    }
  }
}
