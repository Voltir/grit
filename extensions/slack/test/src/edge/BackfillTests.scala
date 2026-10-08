package grit.slack.edge

import utest.*

/** [[Backfill.of]]: the bounds a deployment may give a join's backfill. */
object BackfillTests extends TestSuite {

  val tests = Tests {
    test("bounds of at least 1 each are a backfill's, as given") {
      (
        Backfill.of(1, 1, 1).map(b => (b.days, b.messages, b.joinsPerDay)),
        Backfill.of(7, 500, 3).map(b => (b.days, b.messages, b.joinsPerDay))
      ) ==> (Right((1, 1, 1)), Right((7, 500, 3)))
    }

    test("a bound under 1 is refused, naming it and its value") {
      Vector(Backfill.of(0, 100, 10), Backfill.of(2, -1, 10), Backfill.of(2, 100, 0)) ==> Vector(
        Left("days is 0: a backfill's days must be at least 1"),
        Left("messages is -1: a backfill's messages must be at least 1"),
        Left("joinsPerDay is 0: a backfill's joinsPerDay must be at least 1")
      )
    }
  }
}
