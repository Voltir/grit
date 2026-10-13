package grit.core.store

import grit.core.act.MovesFixtures.FakeDb
import grit.core.visibility.{Clearance, Label, Level, Subject}

import utest.*

object DbWithinTests extends TestSuite {

  private val internal = Label.at(Level.Internal)
  private val restricted = Label.at(Level.Restricted)

  /** Whether the body of a read through `db` ran, and what the read returned. */
  private def ran(db: Db^): (Boolean, Either[StoreError, Unit]) = {
    var body = false
    val result = db.read(Subject.Public) { body = true; Right(()) }
    (body, result)
  }

  val tests = Tests {
    test("a read whose transaction knows no more than the bound runs, at it or under") {
      ran(Db.within(FakeDb(Clearance.of(internal)), internal)) ==> (true, Right(()))
      ran(Db.within(FakeDb(Clearance.of(Label.Public)), internal)) ==> (true, Right(()))
    }

    test("a read whose transaction knows more than the bound runs nothing, naming both") {
      ran(Db.within(FakeDb(Clearance.of(restricted)), internal)) ==> (
        false,
        Left(
          StoreError.Invalid(
            s"a read at ${Label.shown(restricted)} is above the ${Label.shown(internal)} its " +
              "calls were routed for, which changed since: ask again"
          )
        )
      )
    }

    test("reads for one subject made from a bounded db keep its bound") {
      val reads = Db.within(FakeDb(Clearance.of(restricted)), internal).as(Subject.Public)
      var body = false
      val result = reads.read { body = true; Right(()) }
      (body, result.isLeft) ==> (false, true)
    }
  }
}
