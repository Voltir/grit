package grit.core.identity

import utest.*

object RealmTests extends TestSuite {

  private def account(text: String): Account =
    Account.read(text).getOrElse(throw new java.lang.AssertionError(text))

  val tests = Tests {
    test("a realm holds the accounts its namespace names under its scope, and no others") {
      val t1 = Realm.of("slack", "T1").getOrElse(throw new java.lang.AssertionError("T1"))
      List("slack:T1/U1", "slack:T10/U1", "slack:T1", "slack:T1U1", "teams:T1/U1")
        .map(a => t1.holds(account(a))) ==> List(true, false, false, false, false)
      assert(!t1.holds(Account.Local))
    }

    test("a realm's namespace is an account's, never email, and its scope one segment") {
      Realm.of("email", "x") ==>
        Left("email is no account's namespace: an address is what a realm attests of an account")
      List("", " ", "T/1", "T 1").map(Realm.of("slack", _).isRight) ==>
        List(false, false, false, false)
      Realm.of("slack", "T/1") ==>
        Left("a realm's scope is not blank and holds no / or whitespace: T/1")
      Realm.of("Slack", "T1").isRight ==> false
    }
  }
}
