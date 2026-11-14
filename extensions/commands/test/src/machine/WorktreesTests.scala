package grit.commands.machine

import grit.core.place.Directory

import utest.*

/** [[Worktrees.at]] and [[MachineName.of]]: a directory is placed by the machine it is on. */
object WorktreesTests extends TestSuite {

  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), a => a)

  val tests = Tests {
    test("one path on two machines is two places, each under its machine") {
      val dir = got(Directory.of("/home/a/repo"))
      Vector("desktop", "7-sandbox").map(w => Worktrees.at(got(MachineName.of(w)), dir).written) ==>
        Vector("machine:desktop/home/a/repo", "machine:7-sandbox/home/a/repo")
    }

    test("a machine's name is a slug of at most 32 characters") {
      Vector("a" * 32, "a" * 33, "Desktop", "", "-desk").map(
        MachineName.of(_).toOption.map(MachineName.value)
      ) ==>
        Vector(Some("a" * 32), None, None, None, None)
    }
  }
}
