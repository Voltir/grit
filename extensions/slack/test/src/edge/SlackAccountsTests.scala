package grit.slack.edge

import grit.core.identity.{Account, TestAccounts}
import grit.slack.event.{TeamId, UserId}

import utest.*

object SlackAccountsTests extends TestSuite {

  val tests = Tests {
    test("a Slack user's account is slack:{team}/{user}, as stored accounts are spelled") {
      SlackAccounts.account(TeamId("T0123"), UserId("U0456")).map(Account.written) ==>
        Right("slack:T0123/U0456")
    }

    test("an id that is blank or holds / or whitespace makes no account, naming the id") {
      SlackAccounts.account(TeamId("T1/x"), UserId("U1")) ==>
        Left("a Slack team id is not blank and holds no / or whitespace: T1/x")
      SlackAccounts.account(TeamId("T1"), UserId("U 1")) ==>
        Left("a Slack user id is not blank and holds no / or whitespace: U 1")
      SlackAccounts.account(TeamId("T1"), UserId("")) ==>
        Left("a Slack user id is not blank and holds no / or whitespace: ")
    }

    test("a team's realm holds its users' accounts and no other team's") {
      val holds = for {
        realm <- SlackAccounts.realm(TeamId("T0123"))
        ours <- SlackAccounts.account(TeamId("T0123"), UserId("U0456"))
        theirs <- SlackAccounts.account(TeamId("T01234"), UserId("U0456"))
      } yield (realm.holds(ours), realm.holds(theirs))
      holds ==> Right((true, false))
      SlackAccounts.realm(TeamId("T 1")) ==>
        Left("a Slack team id is not blank and holds no / or whitespace: T 1")
    }

    test("the user an account names is read back only in the account's own team") {
      val team = TeamId("T0123")
      val read = SlackAccounts.account(team, UserId("U0456")).map { a =>
        (SlackAccounts.user(a, team), SlackAccounts.user(a, TeamId("T0999")))
      }
      read ==> Right((Some(UserId("U0456")), None))
      Vector("local", "test:T0123/U0456", "slack:T0123U0456").map(written =>
        SlackAccounts.user(TestAccounts.account(written), team)
      ) ==> Vector(None, None, None)
    }

    test("an edge cannot pass an account's spelling off as a principal: PrincipalId has no apply") {
      val error = assertCompileError("""grit.core.id.PrincipalId("slack:T/U")""")
      error.msg ==> "object PrincipalId in package grit.core.id does not take parameters"
    }
  }
}
