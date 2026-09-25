package grit.core.topic

import grit.core.id.TurnSeq

import utest.*

object TopicsTests extends TestSuite {

  private val (a, b, c) = (TopicId("a"), TopicId("b"), TopicId("c"))

  private def placed(turn: Long, topic: TopicId, by: Placement = Placement.First) =
    TopicEvent.Placed(TurnSeq(turn), Vector(topic -> 1.0), 0.0, by)

  private def close(x: Double, y: Double): Boolean = math.abs(x - y) < 1e-9

  val tests = Tests {
    test("nothing recorded: no topics, no current one") {
      val t = Topics.fold(Vector.empty)
      assert(t.topics.isEmpty, t.current.isEmpty, t.earlier.isEmpty)
    }

    test("topics come most recently spoken in first; the current is the latest turn's") {
      val t = Topics.fold(
        Vector(
          TopicEvent.Opened(a),
          placed(0, a),
          TopicEvent.Opened(b),
          placed(1, b),
          placed(2, a),
          TopicEvent.Opened(c),
          placed(3, c),
          placed(4, b)
        )
      )
      t.topics.map(_.id) ==> Vector(b, c, a)
      t.current.map(_.id) ==> Some(b)
      t.earlier.map(_.id) ==> Vector(c, a)
      t.get(a).map(_.turns) ==> Some(Vector(TurnSeq(0), TurnSeq(2)))
    }

    test("a turn placed again is where its latest placement puts it") {
      val t = Topics.fold(
        Vector(
          TopicEvent.Opened(a),
          placed(0, a),
          TopicEvent.Placed(TurnSeq(1), Vector(a -> 0.4), 0.6, Placement.First),
          TopicEvent.Opened(b),
          placed(1, b, Placement.Asked(Verdict.New(None), None))
        )
      )
      t.placed(TurnSeq(1)) ==> Some(b)
      t.placements.get(TurnSeq(1)).map(_.size) ==> Some(2)
      t.get(a).map(_.turns) ==> Some(Vector(TurnSeq(0)))
      t.current.map(_.id) ==> Some(b)
    }

    test("the placed topic is the heaviest, even when more of the weight is elsewhere") {
      Topics.top(TopicEvent.Placed(TurnSeq(0), Vector(a -> 0.3), 0.7, Placement.First)) ==> Some(a)
      Topics.top(
        TopicEvent.Placed(TurnSeq(0), Vector(a -> 0.2, b -> 0.5), 0.3, Placement.First)
      ) ==>
        Some(b)
      Topics.top(TopicEvent.Placed(TurnSeq(0), Vector.empty, 1.0, Placement.First)) ==> None
    }

    test("a description names a topic; the latest one stands; unnamed shows as new topic") {
      val t = Topics.fold(
        Vector(
          TopicEvent.Opened(a),
          placed(0, a),
          TopicEvent.Described(a, "Knots", "one"),
          TopicEvent.Described(a, "Knots", "two"),
          TopicEvent.Opened(b),
          placed(1, b)
        )
      )
      t.get(a).map(x => (x.shown, x.summary)) ==> Some(("Knots", Some("two")))
      t.get(b).map(_.shown) ==> Some(Topic.Unnamed)
    }

    test("keys are distinct: a repeated name is numbered") {
      val ts = Vector(
        Topic(a, None, None, Vector()),
        Topic(b, Some("X"), None, Vector()),
        Topic(c, None, None, Vector())
      )
      Topics.keys(ts).map(_._2) ==> Vector("new topic", "X", "new topic (2)")
    }

    test("bands: 0.8 and up same, below 0.2 changed, between uncertain") {
      Vector(1.0, 0.8, 0.79, 0.2, 0.19, 0.0).map(Band.of) ==>
        Vector(Band.Same, Band.Same, Band.Uncertain, Band.Uncertain, Band.Changed, Band.Changed)
    }

    test("weights: level 1 keeps the remainder elsewhere") {
      val (w, e) = Weights.same(a, 0.6)
      w ==> Vector(a -> 0.6)
      assert(close(e, 0.4))
    }

    test("weights: level 2 scales the choice by 1 - p(same); new goes to the opened topic") {
      val (w, e) = Weights.changed(a, 0.1, Vector(Some(b) -> 0.25, None -> 0.75), Some(c))
      w.map(_._1) ==> Vector(a, b, c)
      assert(
        close(w(0)._2, 0.1),
        close(w(1)._2, 0.225),
        close(w(2)._2, 0.675),
        close(e, 0.0),
        close(w.map(_._2).sum + e, 1.0)
      )
    }

    test("weights: level 2's share for new is elsewhere when no topic was opened") {
      val (w, e) = Weights.changed(a, 0.1, Vector(Some(b) -> 0.75, None -> 0.25), None)
      w.map(_._1) ==> Vector(a, b)
      assert(close(w(1)._2, 0.675), close(e, 0.225))
    }

    test("weights: the current topic among level 2's options adds up, once") {
      val (w, _) = Weights.changed(a, 0.1, Vector(Some(a) -> 0.5, None -> 0.5), None)
      w.map(_._1) ==> Vector(a)
      assert(close(w(0)._2, 0.55))
    }

    test("weights: whole") {
      Weights.whole(a) ==> (Vector(a -> 1.0), 0.0)
    }
  }
}
