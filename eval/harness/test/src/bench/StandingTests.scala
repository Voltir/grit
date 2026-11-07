package grit.eval.harness.bench

import java.nio.file.Files

import utest.*

/** [[Standing]] read from its file: a missing or blank file refused, naming it. */
object StandingTests extends TestSuite {

  val tests = Tests {
    test("a standing text is the file's text whole") {
      val dir = Files.createTempDirectory("standing")
      val file = Files.writeString(dir.resolve("standing.md"), "You make one change.\n")
      Standing.read(file).map(_.text) ==> Right("You make one change.\n")
    }

    test("a missing standing file is refused, naming it") {
      val file = Files.createTempDirectory("standing").resolve("gone.md")
      Standing.read(file) ==> Left(s"the standing text's file $file cannot be read")
    }

    test("a blank standing file is refused, naming it") {
      val dir = Files.createTempDirectory("standing")
      val file = Files.writeString(dir.resolve("standing.md"), " \n\t\n")
      Standing.read(file) ==> Left(s"the standing text's file $file is blank")
    }
  }
}
