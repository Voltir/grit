package grit.core.review

import java.time.Instant

import grit.core.identity.{Account, TestAccounts}
import grit.core.place.{Namespace, Place}
import grit.core.store.{StoreError, Tx}
import grit.dbos.sql.TestTx

import utest.*

object ReviewsAsTests extends TestSuite {

  private final case class Kept(n: Int) extends caps.Pure
  private final case class Shown(n: Int) extends caps.Pure
  private enum Stored extends caps.Pure { case Yes, No }
  private enum Word extends caps.Pure { case Good, Bad }

  /** Reviews waiting on `Kept(1)`, `Kept(2)` and `Kept(3)`, each answer saying which subject or
    * verdict it was asked of: only `Kept(2)` posts at "a", only `Kept(3)` drops, only `Yes` is
    * kept and only `No` withdrawn.
    */
  private object Under extends Reviews[Kept, Stored] {
    def unposted(to: Place)(using Tx^): Either[StoreError, Vector[Kept]] =
      Right(Vector(Kept(1), Kept(2), Kept(3)))
    def posted(subject: Kept, address: String, at: Instant)(using
        Tx^
    ): Either[StoreError, Boolean] = Right(subject == Kept(2) && address == "a")
    def dropped(subject: Kept, at: Instant)(using Tx^): Either[StoreError, Boolean] =
      Right(subject == Kept(3))
    def reacted(address: String, rater: Account, verdict: Stored, at: Instant)(using
        Tx^
    ): Either[StoreError, Boolean] = Right(verdict == Stored.Yes)
    def unreacted(address: String, rater: Account, verdict: Stored)(using
        Tx^
    ): Either[StoreError, Boolean] = Right(verdict == Stored.No)
  }

  private val shown: Reviews[Shown, Word] = Reviews.as(Under)(
    k => Option.when(k.n != 1)(Shown(k.n * 10)),
    s => Kept(s.n / 10),
    {
      case Word.Good => Stored.Yes
      case Word.Bad => Stored.No
    }
  )

  private val At = Instant.parse("2026-03-01T00:00:00Z")
  private val rater = TestAccounts.account("slack:T/U1")
  private val to = Place.under(Namespace.Slack, Vector("T", "C1"))

  val tests = Tests {
    test("a subject read as none is never offered; the rest are, as read, in their order") {
      shown.unposted(to)(using TestTx.fake) ==> Right(Vector(Shown(20), Shown(30)))
    }

    test("a subject is posted and dropped as the one it was read from") {
      given Tx = TestTx.fake
      Vector(
        shown.posted(Shown(20), "a", At),
        shown.posted(Shown(30), "a", At),
        shown.dropped(Shown(30), At),
        shown.dropped(Shown(20), At)
      ) ==> Vector(Right(true), Right(false), Right(true), Right(false))
    }

    test("a verdict is kept and withdrawn as the one it maps to") {
      given Tx = TestTx.fake
      Vector(
        shown.reacted("a", rater, Word.Good, At),
        shown.reacted("a", rater, Word.Bad, At),
        shown.unreacted("a", rater, Word.Bad),
        shown.unreacted("a", rater, Word.Good)
      ) ==> Vector(Right(true), Right(false), Right(true), Right(false))
    }
  }
}
