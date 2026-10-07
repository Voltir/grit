package grit.core.store

import grit.core.id.ShortHash
import grit.core.identity.TestAccounts
import grit.core.visibility.{Label, TestLabels}

import utest.*

/** A vouching's changes as the log writes them. */
object LinkingTests extends TestSuite {

  private val mail = TestAccounts.account("email:a@b.c")
  private val slack = TestAccounts.account("slack:T1/U1")
  private val nick = TestAccounts.principalId(slack)
  private val hashed = s"email:#${ShortHash.of("a@b.c")}"

  val tests = Tests {
    test("a link or unlink of an email account writes its address's hash, never the address") {
      val lines = Vector(
        Linking.Linked(mail, nick, Label.Public, TestLabels.Trial),
        Linking.Unlinked(mail, nick, TestLabels.Trial, Label.Public)
      ).map(_.message)
      (lines, lines.exists(_.contains("@"))) ==> (
        Vector(
          s"$hashed linked to slack:T1/U1: public -> public+trial",
          s"$hashed unlinked from slack:T1/U1: public+trial -> public"
        ),
        false
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

    test("a refusal names its account and why, never an address") {
      Vector(
        Linking.Refused(slack, LinkRefusal.OutsideRealms),
        Linking.Refused(slack, LinkRefusal.Declared),
        Linking.Refused(mail, LinkRefusal.Joins(nick))
      ).map(_.message) ==> Vector(
        "slack:T1/U1 not linked: no realm vouched for holds it",
        "slack:T1/U1 not linked: the deployment declares it",
        s"$hashed not linked: its email is slack:T1/U1's, and it is not alone in its own person"
      )
    }
  }
}
