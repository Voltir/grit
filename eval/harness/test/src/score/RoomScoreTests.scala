package grit.eval.harness.score

import grit.eval.Room

import utest.*

/** [[RoomScore]]: a room's placings against its truth. */
object RoomScoreTests extends TestSuite {

  private def message(id: String, minute: Int, who: String, topic: Option[String]): ujson.Value = {
    val m = ujson.Obj(
      "id" -> id,
      "at" -> f"2026-10-12T09:$minute%02d:00Z",
      "who" -> who,
      "text" -> s"text of $id"
    )
    topic.foreach(t => m("topic") = t)
    m
  }

  // Said: m1 (0), m3 (1, Ana's burst after m1), m2 (10), m4 (11, Ben's burst after m2), m5 (20).
  private val room: Room = Room
    .read(
      "r",
      ujson
        .Obj(
          "team" -> "T",
          "channel" -> "C",
          "messages" -> ujson.Arr(
            message("m1", 0, "Ana", Some("ci")),
            message("m3", 1, "Ana", Some("ci")),
            message("m2", 10, "Ben", Some("csv")),
            message("m4", 11, "Ben", Some("lunch")),
            message("m5", 20, "Dev", Some("csv"))
          )
        )
        .render()
    )
    .fold(e => throw new java.lang.AssertionError(e), identity)

  private def offer(root: String, p: Double, recent: Boolean = true) =
    RoomPlacing.Offer(root, recent, Some(p))

  private val placings: Vector[RoomPlacing] = Vector(
    RoomPlacing("m1", RoomPlacing.Made.Unoffered, Vector.empty),
    RoomPlacing(
      "m3",
      RoomPlacing.Made.Follows("m1", 0.8),
      Vector(offer("m1", 0.8, recent = false))
    ),
    RoomPlacing("m2", RoomPlacing.Made.Begins(0.2), Vector(offer("m1", 0.2))),
    RoomPlacing("m4", RoomPlacing.Made.Follows("m2", 0.7), Vector(offer("m2", 0.7))),
    RoomPlacing("m5", RoomPlacing.Made.Begins(0.5), Vector(offer("m2", 0.5), offer("m1", 0.1)))
  )

  private val strands: Vector[Vector[String]] = Vector(Vector("m1", "m3"), Vector("m2", "m4"))

  private def counts(p: grit.eval.harness.stats.Proportion): (Long, Long) = (p.hits, p.n)

  val tests = Tests {
    test("an opening offered its topic but begun is a recall hit and a choice missed") {
      val s = RoomScore.of(room, placings, strands)
      (counts(s.right), counts(s.recall), counts(s.strict), counts(s.chosen)) ==>
        ((3L, 5L), (2L, 2L), (2L, 2L), (1L, 2L))
    }

    test("a follow of another topic is a wrong follow, not a begin, in the confusion") {
      val s = RoomScore.of(room, placings, strands)
      s.wrongFollows ==> Vector("m4")
      s.confusion ==> Vector(
        (true, RoomScore.Got.FollowsRight) -> 1,
        (true, RoomScore.Got.Begins) -> 1,
        (false, RoomScore.Got.FollowsWrong) -> 1,
        (false, RoomScore.Got.Begins) -> 1,
        (false, RoomScore.Got.Unoffered) -> 1
      )
    }

    test("a burst's rule is right when it continues; Jev agrees when it follows the strand") {
      RoomScore.of(room, placings, strands).bursts ==> Vector(
        RoomScore.Burst("m3", "m1", 60, continues = true, right = true, agrees = true),
        RoomScore.Burst("m4", "m2", 60, continues = false, right = false, agrees = true)
      )
    }

    test("a topic in two strands is split; a strand of two topics is mixed") {
      val s = RoomScore.of(room, placings, strands)
      (s.split, s.mixed) ==> (Vector("csv" -> 2), Vector("m2" -> Vector("csv", "lunch")))
    }

    test("an opening skipped at the cap is counted apart, in no proportion") {
      val s = RoomScore.of(
        room,
        placings.dropRight(1) :+ RoomPlacing("m5", RoomPlacing.Made.Skipped, Vector.empty),
        strands
      )
      (s.openings, s.skipped, counts(s.right), counts(s.recall)) ==> (5, 1, (3L, 4L), (1L, 1L))
    }
  }
}
