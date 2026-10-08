package grit.core.store

import grit.core.identity.TestAccounts
import grit.core.visibility.{Label, TestLabels}

import utest.*

/** A vouching's changes as the log writes them. */
object LinkingTests extends TestSuite {

  private val slack = TestAccounts.account("slack:T1/U1")
  private val person = TestAccounts.principalId(TestAccounts.account("slack:T1/U9"))

  val tests = Tests {
    test("a link or unlink names its account as spelled, the person and the clearances") {
      Vector(
        Linking.Linked(slack, person, Label.Public, TestLabels.Trial),
        Linking.Unlinked(slack, person, TestLabels.Trial, Label.Public)
      ).map(_.message) ==> Vector(
        "slack:T1/U1 linked to slack:T1/U9: public -> public+trial",
        "slack:T1/U1 unlinked from slack:T1/U9: public+trial -> public"
      )
    }

    test("a standing names its account, whether it is a member, and the clearances") {
      (
        Linking.Standing(slack, member = true, Label.Public, TestLabels.Trial).message,
        Linking.Standing(slack, member = false, TestLabels.Trial, Label.Public).message
      ) ==> (
        "slack:T1/U1 is a full member of its realm: public -> public+trial",
        "slack:T1/U1 is not a full member of its realm: public+trial -> public"
      )
    }

    test("an account outside the voucher's realms, and an unclaimed email, each say so apart") {
      (Linking.Outside(slack).message, Linking.Unclaimed(slack).message) ==> (
        "slack:T1/U1 not recorded: no realm the voucher holds holds it",
        "slack:T1/U1's verified email is in no domain the deployment claims: no email kept"
      )
    }
  }
}
