package grit.core.secret

import utest.*

/** [[Named]]: the one rule for what is named like a secret. */
object NamedTests extends TestSuite {

  val tests = Tests {
    test("KEY, TOKEN, SECRET or PASSWORD in any case names a secret; GRIT_WINDOW_SIZE does not") {
      Vector(
        "ANTHROPIC_API_KEY",
        "slack_bot_token",
        "AWS_SECRET_ACCESS_KEY",
        "GRIT_DATABASE_Password",
        "GRIT_WINDOW_SIZE",
        "HOME"
      ).filter(Named.variable) ==> Vector(
        "ANTHROPIC_API_KEY",
        "slack_bot_token",
        "AWS_SECRET_ACCESS_KEY",
        "GRIT_DATABASE_Password"
      )
    }

    test("`.env` and `.env.local` are secrets files; `.envrc`, `env` and `a.env` are not") {
      Vector(".env", ".env.local", ".envrc", "env", "a.env").filter(Named.file) ==>
        Vector(".env", ".env.local")
    }
  }
}
