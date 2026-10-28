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

    test(
      "the days backfill reads are GRIT_BACKFILL_DAYS, 2 when unset, refused unless a whole number above zero"
    ) {
      (
        Backfill.days(Map.empty),
        Backfill.days(Map("GRIT_BACKFILL_DAYS" -> " 7 ")),
        Backfill.days(Map("GRIT_BACKFILL_DAYS" -> "0")),
        Backfill.days(Map("GRIT_BACKFILL_DAYS" -> "1.5"))
      ) ==> (
        Right(2),
        Right(7),
        Left("GRIT_BACKFILL_DAYS is a whole number of days above zero, not '0'"),
        Left("GRIT_BACKFILL_DAYS is a whole number of days above zero, not '1.5'")
      )
    }

    test(
      "a join's backfill is bounded by GRIT_BACKFILL_DAYS, GRIT_BACKFILL_MESSAGES and GRIT_BACKFILL_JOINS_PER_DAY, the default's where unset, each refused by name unless a whole number above zero"
    ) {
      (
        Backfill.read(Map.empty),
        Backfill
          .read(
            Map(
              "GRIT_BACKFILL_DAYS" -> "3",
              "GRIT_BACKFILL_MESSAGES" -> " 50 ",
              "GRIT_BACKFILL_JOINS_PER_DAY" -> "4"
            )
          )
          .map(b => (b.days, b.messages, b.joinsPerDay)),
        Backfill.read(Map("GRIT_BACKFILL_MESSAGES" -> "0")),
        Backfill.read(Map("GRIT_BACKFILL_JOINS_PER_DAY" -> "ten"))
      ) ==> (
        Right(Backfill.Default),
        Right((3, 50, 4)),
        Left("GRIT_BACKFILL_MESSAGES is a whole number above zero, not '0'"),
        Left("GRIT_BACKFILL_JOINS_PER_DAY is a whole number above zero, not 'ten'")
      )
    }
  }
}
