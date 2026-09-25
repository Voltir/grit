package grit.app.chat

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, TurnSeq}
import grit.core.store.{Entry, Payload}
import grit.core.topic.{Band, Placement, TopicEvent, TopicId, Verdict}

import utest.*

/** The topics tab's view, from the conversation's entries alone. */
object TopicsViewTests extends TestSuite {

  private val c = ConversationId("c")
  private val (a, b) = (TopicId("a"), TopicId("b"))

  private def topic(seq: Long, turn: Long, events: TopicEvent*): Entry =
    Entry(
      EntryId(s"t$seq"),
      c,
      TurnSeq(turn),
      None,
      seq,
      Payload.Topic(events.toVector),
      Instant.EPOCH
    )

  private def whole(turn: Long, t: TopicId, by: Placement) =
    TopicEvent.Placed(TurnSeq(turn), Vector(t -> 1.0), 0.0, by)

  private val entries: Vector[Entry] = Vector(
    topic(0, 0, TopicEvent.Opened(a), whole(0, a, Placement.First)),
    topic(1, 0, TopicEvent.Described(a, "Knots", "which knot holds")),
    topic(
      2,
      1,
      TopicEvent.Opened(b),
      TopicEvent.Placed(
        TurnSeq(1),
        Vector(a -> 0.1, b -> 0.72),
        0.18,
        Placement.Classified(0.1, Band.Changed, Vector(Some(a) -> 0.2, None -> 0.8))
      )
    ),
    topic(
      3,
      2,
      TopicEvent.Placed(
        TurnSeq(2),
        Vector(b -> 0.6),
        0.4,
        Placement.Classified(0.6, Band.Uncertain, Vector.empty)
      )
    ),
    topic(4, 2, whole(2, a, Placement.Asked(Verdict.Earlier("Knots"), Some("dropped a call"))))
  )

  val tests = Tests {
    test("topics most recent first, with their messages; the current one marked") {
      TopicsView.of(entries, None).topics ==> Vector(
        TopicsView.Row("Knots", 2, current = true),
        TopicsView.Row("new topic", 1, current = false)
      )
    }

    test("the latest turn's placing: the classifier's numbers beside the model's verdict") {
      val p = TopicsView.of(entries, None).placing.getOrElse(sys.error("no placing"))
      p.turn ==> TurnSeq(2)
      (p.pSame, p.band) ==> (Some(0.6), Some(Band.Uncertain))
      p.verdict ==> Some(Verdict.Earlier("Knots"))
      p.anomaly ==> Some("dropped a call")
      p.placed ==> Some("Knots")
      p.weights ==> Vector("Knots" -> 1.0)
      // Leaning same (0.6) while the model said an earlier topic.
      p.disagree ==> true
    }

    test("a pinned turn's placing: level two's choice by name, most probable first") {
      val p = TopicsView.of(entries, Some(TurnSeq(1))).placing.getOrElse(sys.error("no placing"))
      p.choice ==> Vector(TopicsView.NewOption -> 0.8, "Knots" -> 0.2)
      p.weights ==> Vector("new topic" -> 0.72, "Knots" -> 0.1)
      assert(math.abs(p.elsewhere - 0.18) < 1e-9, !p.disagree, p.verdict.isEmpty)
    }

    test("the first message, and a turn never placed, show as such") {
      TopicsView.of(entries, Some(TurnSeq(0))).placing.map(_.first) ==> Some(true)
      TopicsView.of(Vector.empty, Some(TurnSeq(5))) ==> TopicsView(Vector.empty, None)
    }

    test("agreement is no flag: leaning same, and the model said current") {
      val agreed = entries.dropRight(1) :+
        topic(4, 2, whole(2, b, Placement.Asked(Verdict.Current, None)))
      TopicsView.of(agreed, None).placing.map(_.disagree) ==> Some(false)
    }
  }
}
