package grit.core.secret

import utest.*

/** [[Secret]]: what it shows, when it is read, and what it equals. */
object SecretTests extends TestSuite {

  private val key = Variable("ANTHROPIC_API_KEY")

  val tests = Tests {
    test("a secret shows as its variable, never its value") {
      val s = Secret.read(Map("ANTHROPIC_API_KEY" -> "sk-ant-planted-value"), key)
      s.map(_.toString) ==> Right("Secret(ANTHROPIC_API_KEY)")
    }

    test(
      "a secret is read trimmed, only for a variable named like one; unset and blank are refused, naming it"
    ) {
      Secret.read(Map("ANTHROPIC_API_KEY" -> "  sk-ant-planted \n"), key).map(Reveal.text) ==>
        Right("sk-ant-planted")
      val home = Variable("HOME")
      Secret.read(Map("HOME" -> "/home/someone"), home) ==> Left(
        SecretRefusal.NotNamedLikeOne(home)
      )
      Secret.read(Map.empty, key) ==> Left(SecretRefusal.Unset(key))
      Secret.read(Map("ANTHROPIC_API_KEY" -> " \t"), key) ==> Left(SecretRefusal.Blank(key))
      Vector(
        SecretRefusal.NotNamedLikeOne(home),
        SecretRefusal.Unset(key),
        SecretRefusal.Blank(key)
      )
        .map(_.message) ==> Vector(
        "HOME is not named like a secret: its name holds none of KEY, TOKEN, SECRET or PASSWORD",
        "ANTHROPIC_API_KEY is not set",
        "ANTHROPIC_API_KEY is set but blank"
      )
    }

    test("two secrets read from one value are not equal") {
      val env = Map("ANTHROPIC_API_KEY" -> "sk-ant-planted-value")
      val both =
        for { a <- Secret.read(env, key); b <- Secret.read(env, key) } yield (a == b, a == a)
      both ==> Right((false, true))
    }
  }
}
