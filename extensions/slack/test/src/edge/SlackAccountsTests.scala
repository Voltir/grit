package grit.slack.edge

import grit.core.identity.Account
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
  }
}
