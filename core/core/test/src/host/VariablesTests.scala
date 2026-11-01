package grit.core.host

import utest.*

/** [[Variables]]: what a program may be handed in its environment. */
object VariablesTests extends TestSuite {

  val tests = Tests {
    test("a variable named like a secret is refused, naming it; others are kept as given") {
      Variables.of(Map("CLAUDE_CONFIG_DIR" -> "/tmp/c", "GITHUB_TOKEN" -> "ghp-planted")) ==>
        Left("GITHUB_TOKEN is named like a secret: a secret is handed as a Secret, or not at all")
      Variables.of(Map("CLAUDE_CONFIG_DIR" -> "/tmp/c")).map(_.values) ==>
        Right(Map("CLAUDE_CONFIG_DIR" -> "/tmp/c"))
    }
  }
}
