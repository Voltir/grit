package grit.databases.declare

import scala.concurrent.duration.*

import utest.*

/** Which limits [[Limits.of]] takes. */
object LimitsTests extends TestSuite {

  val tests = Tests {
    test("positive rows and bytes and a timeout of a millisecond or more are limits") {
      assert(
        Limits.of(1, 1, 1.millisecond).map(l => (l.rows, l.bytes, l.timeout)) == Right(
          (1, 1, 1.millisecond)
        )
      )
    }

    test("no rows or negative bytes are refused, naming which") {
      assert(
        Limits.of(0, 10, 1.second) == Left("a result's rows must be positive: 0"),
        Limits.of(10, -1, 1.second) == Left("a result's bytes must be positive: -1")
      )
    }

    // 500 µs is 0 ms, and a statement_timeout of 0 is none at all.
    test("a timeout under a millisecond is refused, since Postgres would read it as none") {
      assert(
        Limits.of(10, 10, 500.microseconds) == Left(
          "a statement's timeout is at least a millisecond: 500 microseconds"
        )
      )
    }

    test("a timeout past Postgres's most is refused") {
      assert(
        Limits.of(10, 10, (Int.MaxValue.toLong + 1).milliseconds) ==
          Left("a statement's timeout is at most 2147483647 ms: 2147483648 milliseconds")
      )
    }
  }
}
