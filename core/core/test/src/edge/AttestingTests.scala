package grit.core.edge

import scala.concurrent.duration.*

import grit.core.identity.{Realm, TestAccounts}
import grit.core.store.Linking
import grit.core.visibility.{Label, TestLabels}

import utest.*

/** What core's re-check tells the log, and the three constants its docs give the values of. */
object AttestingTests extends TestSuite {

  private val realm =
    Realm.of("slack", "T1").fold(e => throw new java.lang.AssertionError(e), identity)
  private val account = TestAccounts.account("slack:T1/U1")

  val tests = Tests {
    test("a change's line is its linking's own") {
      Attesting.Report
        .Changed(Linking.Standing(account, member = true, Label.Public, TestLabels.Trial))
        .message ==> "slack:T1/U1 is a full member of its realm: public -> public+trial"
    }

    test("an unreached source names its realm, how many it could not ask about, and why") {
      Attesting.Report.Unreached(realm, 3, "ratelimited").message ==>
        "slack:T1/ could not be asked about 3 of its accounts (ratelimited); what it said before stands"
    }

    test("an overdue realm names how many have gone unanswered twice Due, and why") {
      Attesting.Report.Overdue(realm, 2, "invalid_auth").message ==>
        "slack:T1/: 2 of its accounts have had no answer for 48 hours or more, or ever (invalid_auth)"
    }

    test("Due is 24 hours, Fresh one minute, and a look every ten minutes") {
      (Attesting.Due, Attesting.Fresh, Attesting.Every) ==> (24.hours, 1.minute, 10.minutes)
    }
  }
}
