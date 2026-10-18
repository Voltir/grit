package grit.eval.harness.score

import utest.*
import RoomFixtures.*

/** [[RoomScore]]: a room's placings against its truth. */
object RoomScoreTests extends TestSuite {

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
