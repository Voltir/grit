package grit.core.topic

import grit.core.id.TurnSeq

import utest.*

object TopicsTests extends TestSuite {

  private val (a, b, c) = (TopicId("a"), TopicId("b"), TopicId("c"))

  private def placed(turn: Long, topic: TopicId, by: Placement = Placement.First) =
    TopicEvent.Placed(TurnSeq(turn), Weights.whole(topic), by)

  private def weights(elsewhere: Double, shares: (TopicId, Double)*): Weights =
    Weights
      .of(shares.toVector.map(Weights.Share(_, _)), elsewhere)
      .fold(e => sys.error(s"not weights: $e"), identity)

  private def close(x: Double, y: Double): Boolean = math.abs(x - y) < 1e-9

  private def choice(chances: (Option[TopicId], Double)*): Vector[Placement.Chance] =
    chances.toVector.map(Placement.Chance(_, _))

  val tests = Tests {
    test("nothing recorded: no topics, no current one") {
      val t = Topics.fold(Vector.empty, Vector.empty)
      assert(t.topics.isEmpty, t.current.isEmpty, t.earlier.isEmpty)
    }

    test("before any message is placed, the current topic is the first carried one, named") {
      // Carried in an order neither id nor name order gives.
      val t = Topics.fold(
        Vector(Topics.Carried(b, "Photo Rename", None), Topics.Carried(a, "Backup", None)),
        Vector.empty
      )
      t.current.map(c => (c.id, c.name, c.summary, c.turns)) ==>
        Some((b, Some("Photo Rename"), None, Vector()))
      t.topics.map(_.id) ==> Vector(b, a)
    }

    test("a carried topic keeps its summary until it is described again") {
      val carried = Vector(Topics.Carried(a, "Photo Rename", Some("renaming photos")))
      Topics.fold(carried, Vector.empty).get(a).flatMap(_.summary) ==> Some("renaming photos")
      Topics
        .fold(carried, Vector(TopicEvent.Described(a, "Photo Rename", "renaming HEIC photos")))
        .get(a)
        .flatMap(_.summary) ==> Some("renaming HEIC photos")
    }

    test("a carried topic ranks by its latest turn once spoken in; the rest follow in order") {
      // Carried in an order neither id nor name order gives.
      val t = Topics.fold(
        Vector(
          Topics.Carried(b, "B", None),
          Topics.Carried(c, "C", None),
          Topics.Carried(a, "A", None)
        ),
        Vector(
          placed(5, c),
          TopicEvent.Described(c, "C", "all about c")
        )
      )
      t.topics.map(_.id) ==> Vector(c, b, a)
      t.current.map(x => (x.id, x.summary)) ==> Some((c, Some("all about c")))
    }

    test("topics come most recently spoken in first; the current is the latest turn's") {
      val t = Topics.fold(
        Vector.empty,
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
        Vector.empty,
        Vector(
          TopicEvent.Opened(a),
          placed(0, a),
          TopicEvent.Placed(TurnSeq(1), weights(0.6, a -> 0.4), Placement.First),
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
      weights(0.7, a -> 0.3).heaviest ==> a
      weights(0.3, a -> 0.2, b -> 0.5).heaviest ==> b
      weights(0.0, a -> 0.5, b -> 0.5).heaviest ==> a
    }

    test("weights: only shares that sum to 1, each topic once, none negative, are weights") {
      Weights.of(Vector.empty, 1.0) ==> Left(WeightsError.NoTopic)
      Weights.of(Vector(Weights.Share(a, 0.5), Weights.Share(a, 0.5)), 0.0) ==>
        Left(WeightsError.Repeated(a))
      Weights.of(Vector(Weights.Share(a, -0.5)), 1.5) ==> Left(WeightsError.Negative(Some(a)))
      Weights.of(Vector(Weights.Share(a, 1.5)), -0.5) ==> Left(WeightsError.Negative(None))
      Weights.of(Vector(Weights.Share(a, Double.NaN)), 0.0) ==>
        Left(WeightsError.NotFinite(Some(a)))
      Weights.of(Vector(Weights.Share(a, 0.5)), Double.PositiveInfinity) ==>
        Left(WeightsError.NotFinite(None))
      Weights.of(Vector(Weights.Share(a, 0.5)), 0.4) match {
        case Left(WeightsError.SumOff(sum)) => assert(close(sum, 0.9))
        case other => sys.error(s"not SumOff: $other")
      }
      Weights
        .of(Vector(Weights.Share(a, 0.1), Weights.Share(b, 0.81)), 0.09000000000000001)
        .map(_.byTopic.map(_.topic)) ==> Right(Vector(a, b))
    }

    test("weights: made only by their constructors") {
      import scala.compiletime.testing.typeChecks
      assert(
        typeChecks("Weights.whole(TopicId(\"a\")).elsewhere"),
        !typeChecks("Weights(Weights.Share(TopicId(\"a\"), 2.0), Vector.empty, 0.0)"),
        !typeChecks("Weights.whole(TopicId(\"a\")).copy(elsewhere = 5.0)"),
        typeChecks("Topics.empty.topics"),
        !typeChecks("Topics(Vector.empty, Map.empty)")
      )
    }

    test("a description names a topic; the latest one stands; unnamed shows as new topic") {
      val t = Topics.fold(
        Vector.empty,
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

    test("bands: 0.8 and up same, below 0.2 changed, between uncertain") {
      Vector(1.0, 0.8, 0.79, 0.2, 0.19, 0.0).map(Band.of) ==>
        Vector(Band.Same, Band.Same, Band.Uncertain, Band.Uncertain, Band.Changed, Band.Changed)
    }

    test("weights: level 1 keeps the remainder elsewhere") {
      val w = Weights.same(a, 0.6)
      w.byTopic ==> Vector(Weights.Share(a, 0.6))
      assert(close(w.elsewhere, 0.4))
    }

    test("weights: level 2 scales the choice by 1 - p(same); new goes to the opened topic") {
      val w = Weights.changed(a, 0.1, choice(Some(b) -> 0.25, None -> 0.75), Some(c))
      w.byTopic.map(_.topic) ==> Vector(a, b, c)
      assert(
        w.byTopic.map(_.weight).zip(Vector(0.1, 0.225, 0.675)).forall((x, y) => close(x, y)),
        close(w.elsewhere, 0.0),
        close(w.byTopic.map(_.weight).sum + w.elsewhere, 1.0)
      )
    }

    test("weights: level 2's share for new is elsewhere when no topic was opened") {
      val w = Weights.changed(a, 0.1, choice(Some(b) -> 0.75, None -> 0.25), None)
      w.byTopic.map(_.topic) ==> Vector(a, b)
      assert(w.byTopic.lastOption.exists(s => close(s.weight, 0.675)), close(w.elsewhere, 0.225))
    }

    test("weights: the current topic among level 2's options adds up, once") {
      val w = Weights.changed(a, 0.1, choice(Some(a) -> 0.5, None -> 0.5), None)
      w.byTopic.map(_.topic) ==> Vector(a)
      assert(close(w.lead.weight, 0.55))
    }

    test("weights: a choice with no probability leaves the rest elsewhere") {
      val w = Weights.changed(a, 0.3, choice(Some(b) -> Double.NaN, None -> -1.0), None)
      w.byTopic.map(_.topic) ==> Vector(a, b)
      assert(close(w.lead.weight, 0.3), close(w.elsewhere, 0.7))
    }

    test("weights: whole") {
      Weights.whole(a).byTopic ==> Vector(Weights.Share(a, 1.0))
      Weights.whole(a).elsewhere ==> 0.0
    }
  }
}
