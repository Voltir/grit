package grit.kit.run

import grit.core.edge.Attesting
import grit.core.identity.{Realm, TestAccounts}
import grit.core.store.Linking
import grit.core.visibility.Label

import utest.*

/** How loudly the serving loop logs what an edge's check or look reported. */
object ReportedTests extends TestSuite {

  private val realm = Realm.of("slack", "T0123").fold(sys.error, identity)
  private val account = TestAccounts.account("slack:T0123/U1")

  /** What [[Kit.reported]] said of `report` at info, warn and error. */
  private def levels(report: Attesting.Report): (Vector[String], Vector[String], Vector[String]) = {
    val infos = Vector.newBuilder[String]
    val warns = Vector.newBuilder[String]
    val errors = Vector.newBuilder[String]
    Kit.reported(report, infos += _, warns += _, errors += _)
    (infos.result(), warns.result(), errors.result())
  }

  val tests = Tests {
    test("a change is information") {
      levels(
        Attesting.Report.Changed(
          Linking.Standing(account, member = false, Label.Public, Label.Public)
        )
      ) ==> (
        Vector("identity: slack:T0123/U1 is not a full member of its realm: public -> public"),
        Vector(),
        Vector()
      )
    }

    test("a source that could not be reached is a warning") {
      levels(Attesting.Report.Unreached(realm, 2, "ratelimited")) ==> (
        Vector(),
        Vector(
          "identity: slack:T0123/ could not be asked about 2 of its accounts (ratelimited); what it said before stands"
        ),
        Vector()
      )
    }

    test("accounts overdue are an error") {
      levels(Attesting.Report.Overdue(realm, 1, "ratelimited")) ==> (
        Vector(),
        Vector(),
        Vector(
          "identity: slack:T0123/: 1 of its accounts have had no answer for 48 hours or more, or ever (ratelimited)"
        )
      )
    }
  }
}
