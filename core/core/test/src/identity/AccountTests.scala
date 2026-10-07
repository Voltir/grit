package grit.core.identity

import utest.*

object AccountTests extends TestSuite {

  private def account(namespace: String, name: String): Account =
    Account.of(namespace, name).getOrElse(throw new java.lang.AssertionError(s"$namespace:$name"))

  private def email(raw: String): Email =
    Email.of(raw).getOrElse(throw new java.lang.AssertionError(raw))

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

    test("of refuses the email namespace, whose accounts only an address makes") {
      Account.of("email", "alice@example.com") ==>
        Left("email accounts are made from an address (Account.email), never named")
      Account.written(Account.email(email(" Alice@Example.com "))) ==> "email:alice@example.com"
    }

    test(
      "read takes back what written writes, local and grit included, and refuses text no account writes"
    ) {
      val each = List(
        account("slack", "T0123/U0456"),
        account("slack", "a:b"),
        Account.email(email("a.b+c@x")),
        Account.Local,
        Account.Grit
      )
      each.map(a => Account.read(Account.written(a))) ==> each.map(Right(_))
      // A pin of the stored form: the store keeps an account as written.
      each.map(Account.written) ==>
        List("slack:T0123/U0456", "slack:a:b", "email:a.b+c@x", "local", "grit")
      List("slack", "slack:", ":U1", "Slack:U1", "email:Alice@x", "email:a@", "Local")
        .map(Account.read(_).isRight) ==> List(false, false, false, false, false, false, false)
      Account.read("email:Alice@x") ==>
        Left("an email account's address is as Email.of writes it: Alice@x")
    }
  }
}
