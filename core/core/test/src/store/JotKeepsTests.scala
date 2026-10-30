package grit.core.store

import grit.core.visibility.{Clearance, Label, Level, Subject}
import grit.dbos.sql.TestTx

import utest.*

/** [[Jot.as]] and [[Jot.within]]: a write's verdict decides its transaction, and a bound made of
  * a jot holds for every write through it.
  */
object JotKeepsTests extends TestSuite {

  private val internal = Label.at(Level.Internal)
  private val restricted = Label.at(Level.Restricted)

  /** A jot whose transactions are opened at `clearance`, recording which subject each was for
    * and whether it was committed, as [[Jot]] says: committed on a `Right` alone.
    */
  private final class Recording(clearance: Clearance = Clearance.of(Label.Public)) extends Jot {
    // Holds an immutable value, written and read on the test's one thread.
    @caps.unsafe.untrackedCaptures
    var ended: Vector[(Subject, Boolean)] = Vector.empty
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] = {
      val result = body(using TestTx.fake(clearance))
      ended = ended :+ (subject, result.isRight)
      result
    }
  }

  private val subject = Subject.Public

  val tests = Tests {
    test("a write that commits is committed for its subject, and one that discards rolled back") {
      val jot = new Recording()
      val keeps = jot.as(subject)
      val committed = keeps.write(Right(Keeps.Verdict.Commit("kept")))
      val discarded = keeps.write(Right(Keeps.Verdict.Discard("undone")))
      (committed, discarded, jot.ended) ==> (
        Right(Keeps.Verdict.Commit("kept")),
        Right(Keeps.Verdict.Discard("undone")),
        Vector(subject -> true, subject -> false)
      )
    }

    test("a write whose body fails is rolled back, and fails as it did") {
      val jot = new Recording()
      val failed = jot.as(subject).write[Unit, Unit](Left(StoreError.Full(3)))
      (failed, jot.ended) ==> (Left(StoreError.Full(3)), Vector(subject -> false))
    }

    test("a write through a bounded jot whose transaction knows more than the bound runs nothing") {
      var ran = false
      val keeps = Jot.within(new Recording(Clearance.of(restricted)), internal).as(subject)
      val result = keeps.write[Unit, Unit] { ran = true; Right(Keeps.Verdict.Commit(())) }
      (ran, result) ==> (
        false,
        Left(
          StoreError.Invalid(
            s"a read at ${Label.shown(restricted)} is above the ${Label.clearance(internal)} its " +
              "calls were routed for, which changed since: ask again"
          )
        )
      )
    }
  }
}
