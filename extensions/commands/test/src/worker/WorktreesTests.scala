package grit.commands.worker

import grit.core.place.Directory

import utest.*

/** [[Worktrees.at]] and [[WorkerName.of]]: a directory is placed by the worker it is on. */
object WorktreesTests extends TestSuite {

  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), a => a)

  val tests = Tests {
    test("one path on two workers is two places, each under its worker") {
      val dir = got(Directory.of("/home/a/repo"))
      Vector("desktop", "7-sandbox").map(w => Worktrees.at(got(WorkerName.of(w)), dir).written) ==>
        Vector("worker:desktop/home/a/repo", "worker:7-sandbox/home/a/repo")
    }

    test("a worker's name is a slug of at most 32 characters") {
      Vector("a" * 32, "a" * 33, "Desktop", "", "-desk").map(
        WorkerName.of(_).toOption.map(WorkerName.value)
      ) ==>
        Vector(Some("a" * 32), None, None, None, None)
    }
  }
}
