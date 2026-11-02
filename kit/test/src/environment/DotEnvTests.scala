package grit.kit.environment

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
          |LONE="
          |MIXED="a'
          |""".stripMargin
      ) ==> Right(
        Map(
          "OPENROUTER_API_KEY" -> "sk-or-abc",
          "GRIT_MODEL" -> "x/y:free",
          "GRIT_SESSION" -> "work",
          "EMPTY" -> "",
          "URL" -> "jdbc:postgresql://h/db?a=b",
          "LONE" -> "\"",
          "MIXED" -> "\"a'"
        )
      )
    }

    test("an unreadable line is named by number, never by its value") {
      DotEnv.parse("A=1\nsk-or-secret\n") ==> Left(".env line 2: expected NAME=value")
      DotEnv.parse("1BAD=secret") ==> Left(".env line 1: not a variable name")
    }

    test("the real environment wins over the file; no file is no change") {
      val dir = java.nio.file.Files.createTempDirectory("dotenv")
      val file = dir.resolve(".env")
      java.nio.file.Files.writeString(file, "A=file\nB=file\n")
      DotEnv.load(file, Map("A" -> "env")) ==> Right(Map("A" -> "env", "B" -> "file"))
      DotEnv.load(dir.resolve("missing"), Map("A" -> "env")) ==> Right(Map("A" -> "env"))
    }

    test(
      "loadDefault reads the file GRIT_ENV_FILE names in the environment it is given, and .env in the working directory when that names none"
    ) {
      val dir = java.nio.file.Files.createTempDirectory("dotenv")
      val file = dir.resolve("deployment.env")
      java.nio.file.Files.writeString(file, "A=file\nB=file\n")
      val named = Map(DotEnv.FileVar -> file.toString, "A" -> "env")
      DotEnv.loadDefault(named) ==> Right(named + ("B" -> "file"))
      // The test's working directory holds no `.env`: the environment comes back as it is.
      java.nio.file.Files.exists(java.nio.file.Path.of(".env")) ==> false
      DotEnv.loadDefault(Map("A" -> "env")) ==> Right(Map("A" -> "env"))
    }
  }
}
