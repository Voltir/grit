package grit.eval

import java.time.LocalDate

import grit.eval.TopicScore.Truth

import utest.*

/** A topics run's placement scored against a hand-made truth. */
object TopicScoreTests extends TestSuite {

  private val Day1 = LocalDate.parse("2026-10-12")
  private val Day2 = Day1.plusDays(1)

  /** The score's counts: precision's and recall's hits of n, then splits, merges, mergeable,
    * cross-day and unplaced.
    */
  private def counts(s: TopicScore) =
    (
      (s.precision.count(_._2), s.precision.size),
      (s.recall.count(_._2), s.recall.size),
      s.splits,
      s.merges,
      s.mergeable,
      s.crossDay,
      s.unplaced
    )

  val tests = Tests {
    test("a room's truth holds its openings, then its cards, each dated in the zone given") {
      val room = Room
        .read(
          "r",
          ujson
            .Obj(
              "team" -> "T",
              "channel" -> "C",
              "messages" -> ujson.Arr(
                ujson.Obj(
                  "id" -> "m1",
                  "at" -> "2026-10-12T23:30:00Z",
                  "who" -> "Ana",
                  "text" -> "a",
                  "topic" -> "ci"
                ),
                ujson.Obj(
                  "id" -> "r1",
                  "at" -> "2026-10-12T23:40:00Z",
                  "who" -> "Ben",
                  "text" -> "b",
                  "thread" -> "m1"
                )
              ),
              "cards" -> ujson.Arr(
                ujson.Obj(
                  "id" -> "c1",
                  "at" -> "2026-10-12T10:00:00Z",
                  "title" -> "Plan",
                  "text" -> "c"
                )
              )
            )
            .render()
        )
        .fold(e => throw new java.lang.AssertionError(e), identity)
      TopicScore.truth(room, java.time.ZoneOffset.ofHours(2)) ==> Vector(
        Truth("m1", Some("ci"), Day2),
        Truth("c1", None, Day1)
      )
    }

    test(
      "pairwise precision and recall, splits, merges and mergeable pairs, over a hand-made placement"
    ) {
      val truth = Vector(
        Truth("o1", Some("deploy"), Day1),
        Truth("o2", Some("deploy"), Day1),
        Truth("o3", Some("deploy"), Day2),
        Truth("o4", Some("hire"), Day1),
        Truth("o5", Some("hire"), Day2),
        Truth("o6", None, Day1)
      )
      val placed = Map(
        "o1" -> Set("t1"),
        "o2" -> Set("t1"),
        "o3" -> Set("t2"),
        "o4" -> Set("t2"),
        "o5" -> Set("t3"),
        "o6" -> Set("t3")
      )
      // Precision: t1's pair is of one topic, t2's is not; chatter pairs with nothing. Recall: of
      // the four pairs of one topic, only o1 and o2 share one. t2 holds both topics, one each,
      // so its most-held is the greater name, hire, which t3 holds too.
      counts(TopicScore.of(truth, placed)) ==> ((1, 2), (1, 4), 2, 1, 1, 2, 0)
    }

    test("a topic holding two truth topics as often is most-held by the greater name") {
      val truth = Vector(
        Truth("o1", Some("deploy"), Day1),
        Truth("o4", Some("hire"), Day1),
        Truth("o5", Some("hire"), Day2)
      )
      // t2 holds deploy and hire once each: only its greater name, hire, pairs it with t3.
      val s = TopicScore.of(truth, Map("o1" -> Set("t2"), "o4" -> Set("t2"), "o5" -> Set("t3")))
      (s.precision, s.mergeable) ==> (Vector("t2" -> false), 1)
    }

    test("a placement that is the truth scores whole, and an opening in no topic is unplaced") {
      val truth = Vector(
        Truth("o1", Some("deploy"), Day1),
        Truth("o2", Some("deploy"), Day2),
        Truth("o3", Some("hire"), Day1)
      )
      (
        counts(TopicScore.of(truth, Map("o1" -> Set("t1"), "o2" -> Set("t1"), "o3" -> Set("t2")))),
        counts(TopicScore.of(truth, Map("o1" -> Set("t1"), "o3" -> Set("t2"))))
      ) ==> (
        ((1, 1), (1, 1), 0, 0, 0, 0, 0),
        ((0, 0), (0, 1), 0, 0, 0, 0, 1)
      )
    }

    test("a topic split across days is a cross-day split; one split within a day is not") {
      val split = Map("o1" -> Set("t1"), "o2" -> Set("t2"))
      def truth(second: LocalDate) =
        Vector(Truth("o1", Some("deploy"), Day1), Truth("o2", Some("deploy"), second))
      (counts(TopicScore.of(truth(Day2), split)), counts(TopicScore.of(truth(Day1), split))) ==> (
        ((0, 0), (0, 1), 1, 0, 1, 1, 0),
        ((0, 0), (0, 1), 1, 0, 1, 0, 0)
      )
    }
  }
}
