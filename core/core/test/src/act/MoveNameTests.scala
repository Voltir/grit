package grit.core.act

import utest.*

/** [[MoveName]] and [[MoveLimits]]: what names a move, and how many a run may make. */
object MoveNameTests extends TestSuite {

  val tests = Tests {
    test("a move's name is 1 to 200 characters") {
      val longest = "a" * 200
      MoveName.of(longest).map(MoveName.value) ==> Right(longest)
      MoveName.of("a" * 201) ==> Left("a move's name is at most 200 characters")
      MoveName.of("") ==> Left("a move's name is not empty")
    }

    // A move `a`'s steps are `move:a`, then `move:a:{phase}`: a name holding `:` could name
    // another move's phase step.
    test("a move's name holds no ':' and no control character") {
      val good = Vector("summarise", "post the digest", "étape-2")
      good.map(MoveName.of(_).map(MoveName.value)) ==> good.map(Right(_))
      val bad = Vector("a:record", ":", "a\nb", "a\tb", "a\u0000")
      bad.map(MoveName.of) ==> bad.map(_ =>
        Left("a move's name holds no ':' and no control character")
      )
    }

    test("a run's limits are each at least zero") {
      MoveLimits.of(0, 0) ==> Right(MoveLimits.Zero)
      MoveLimits.of(3, 1).map(l => (l.asks, l.calls)) ==> Right((3, 1))
      MoveLimits.of(-1, 0) ==> Left("a run's limits are each at least zero: asks -1, calls 0")
      MoveLimits.of(0, -1) ==> Left("a run's limits are each at least zero: asks 0, calls -1")
    }
  }
}
