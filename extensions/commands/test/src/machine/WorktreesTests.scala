package grit.commands.machine

import grit.core.either.Got.got
import grit.core.place.Directory

import utest.*

/** [[Worktrees.at]] and [[MachineName.of]]: a directory is placed by the machine it is on. */
object WorktreesTests extends TestSuite {

  val tests = Tests {
    test("one path on two machines is two places, each under its machine") {
      val dir = got(Directory.of("/home/a/repo"))
      Vector("desktop", "7-sandbox").map(w => Worktrees.at(got(MachineName.of(w)), dir).written) ==>
        Vector("machine:desktop/home/a/repo", "machine:7-sandbox/home/a/repo")
    }

    test("a machine's name is a slug of at most 32 characters, which may start with a digit") {
      Vector("a" * 32, "7desk", "a" * 33, "Desktop", "", "-desk").map(
        MachineName.of(_).map(MachineName.value)
      ) ==> Vector(
        Right("a" * 32),
        Right("7desk"),
        Left(s"a machine's name is over 32 characters: ${"a" * 33}"),
        Left("a machine's name holds 'D', not a lowercase letter, a digit or '-': Desktop"),
        Left("a machine's name is empty"),
        Left("a machine's name starts with '-': -desk")
      )
    }
  }
}
