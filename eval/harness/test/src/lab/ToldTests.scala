package grit.eval.harness.lab

import grit.core.id.ConversationId
import grit.core.period.Probability
import grit.core.stitch.{Placed, StitchJson}

import utest.*

/** [[Told]]: the placement a room's truth gives an opening, in place of Jev's. */
object ToldTests extends TestSuite {

  private val seen = ujson.Obj(
    "state" -> ujson.Obj("new_message" -> "shown"),
    "offered" -> ujson.Arr(),
    "tuning" -> ujson.Obj(
      "horizon_seconds" -> 604_800,
      "recent" -> 2,
      "lexical" -> 2,
      "follows_at" -> 0.6,
      "window_tokens" -> 1_500,
      "strand_chars" -> 800
    )
  )

  private def placed(v: ujson.Value): Placed =
    StitchJson.read(v).fold(e => throw new java.lang.AssertionError(e), identity)

  private val usage = grit.core.store.PayloadJson.writeUsage(grit.core.message.Usage.Zero)

  private val follows = placed(
    ujson.Obj(
      "kind" -> "follows",
      "root" -> "c-wrong",
      "p" -> 0.7,
      "model" -> "jev",
      "usage" -> usage,
      "seen" -> seen
    )
  )
  private val begins = placed(
    ujson.Obj("kind" -> "begins", "p" -> 0.3, "model" -> "jev", "usage" -> usage, "seen" -> seen)
  )
  private val unread = placed(ujson.Obj("kind" -> "unread", "why" -> "no answer", "seen" -> seen))

  /** What a kept placement says: whom it follows, at what, by whom, and what was shown. */
  private def said(p: Placed): (Option[ConversationId], Double, ujson.Value) = p match {
    case f: Placed.Follows => (Some(f.root), Probability.value(f.p), f.seen.state)
    case b: Placed.Begins => (None, Probability.value(b.p), b.seen.state)
    case u: Placed.Unread => (None, 0.0, u.seen.state)
  }

  val tests = Tests {
    test("an opening the truth says follows is kept following its root, with what Jev was shown") {
      val root = ConversationId("c-root")
      Vector(follows, begins, unread).map(p => Told.placement(Some(root), p).map(_.map(said))) ==>
        Vector.fill(3)(Right(Some((Some(root), 1.0, seen("state")))))
    }

    test("an opening the truth says begins keeps nothing, so it links to no strand") {
      Vector(follows, begins, unread).map(Told.placement(None, _)) ==> Vector.fill(3)(Right(None))
    }
  }
}
