package grit.core.identity

import utest.*

object AccountTests extends TestSuite {

  private def account(namespace: String, name: String): Account =
    Account.of(namespace, name).getOrElse(throw new java.lang.AssertionError(s"$namespace:$name"))

  private val Refused =
    "email is no account's namespace: an address is what a realm attests of an account"

  val tests = Tests {
    test("a namespace is a lowercase letter, then lowercase letters, digits or -") {
      List("slack", "google-workspace", "a1")
        .map(Account.of(_, "U1").isRight) ==> List(true, true, true)
      List("", "Slack", "1a", "-a", "a_b", "a:b", "a b")
        .map(Account.of(_, "U1").isRight) ==> List(false, false, false, false, false, false, false)
      Account.of("Slack", "U1") ==>
        Left(
          "an account's namespace is a lowercase letter, then lowercase letters, digits or -: Slack"
        )
    }

    test("a name is not blank and holds no whitespace") {
      List("T0123/U0456", "a:b", "x").map(Account.of("slack", _).isRight) ==> List(true, true, true)
      List("", " ", "U 1", "U1\t").map(Account.of("slack", _).isRight) ==>
        List(false, false, false, false)
      Account.of("slack", "U 1") ==> Left(
        "an account's name is not blank and holds no whitespace: U 1"
      )
    }

    test("no account is an email address: of and read refuse the email namespace") {
      (Account.of("email", "alice@example.com"), Account.read("email:a@b.c")) ==> (
        Left(Refused),
        Left(Refused)
      )
    }

    test("a tripwire: Account has no email, so no account can be made from an address") {
      // Pins a deletion, not a rule: it fails only if `Account.email` comes back.
      val error = assertCompileError("""Email.of("a@b.c").map(Account.email)""")
      assert(error.msg.contains("email"))
    }

    test(
      "read takes back what written writes, local and grit included, and refuses text no account writes"
    ) {
      val each = List(
        account("slack", "T0123/U0456"),
        account("slack", "a:b"),
        Account.Local,
        Account.Grit
      )
      each.map(a => Account.read(Account.written(a))) ==> each.map(Right(_))
      // A pin of the stored form: the store keeps an account as written.
      each.map(Account.written) ==> List("slack:T0123/U0456", "slack:a:b", "local", "grit")
      List("slack", "slack:", ":U1", "Slack:U1", "Local")
        .map(Account.read(_).isRight) ==> List(false, false, false, false, false)
    }
  }
}
