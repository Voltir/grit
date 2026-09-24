package grit.app

import utest.*

object DotEnvTests extends TestSuite {

  val tests = Tests {
    test("names and values, skipping blanks and comments, unquoting one pair") {
      DotEnv.parse(
        """# local settings
          |OPENROUTER_API_KEY=sk-or-abc
          |
          |export GRIT_MODEL = "x/y:free"
          |GRIT_SESSION='work'
          |EMPTY=
          |URL=jdbc:postgresql://h/db?a=b
          |""".stripMargin
      ) ==> Right(
        Map(
          "OPENROUTER_API_KEY" -> "sk-or-abc",
          "GRIT_MODEL" -> "x/y:free",
          "GRIT_SESSION" -> "work",
          "EMPTY" -> "",
          "URL" -> "jdbc:postgresql://h/db?a=b"
        )
      )
    }

    test("an unreadable line is named by number, never by its value") {
      val bad = DotEnv.parse("A=1\nsk-or-secret\n")
      bad ==> Left(".env line 2: expected NAME=value")
      assert(!bad.left.exists(_.contains("secret")))
      DotEnv.parse("1BAD=secret") ==> Left(".env line 1: not a variable name")
    }

    test("the real environment wins over the file; no file is no change") {
      val dir = java.nio.file.Files.createTempDirectory("dotenv")
      val file = dir.resolve(".env")
      java.nio.file.Files.writeString(file, "A=file\nB=file\n")
      DotEnv.load(file, Map("A" -> "env")) ==> Right(Map("A" -> "env", "B" -> "file"))
      DotEnv.load(dir.resolve("missing"), Map("A" -> "env")) ==> Right(Map("A" -> "env"))
    }
  }
}
