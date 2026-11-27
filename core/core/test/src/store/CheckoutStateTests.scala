package grit.core.store

import utest.*

object CheckoutStateTests extends TestSuite {

  val tests = Tests {
    test(
      "a diff keeps its stat's first lines up to its bound, then git's ' ...'; its untracked files' first, counting the rest with those already left out"
    ) {
      val stat = (1 to 40).map(i => s" f$i.scala | 1 +").mkString("\n")
      val names = (1 to 25).map(i => s"n$i").toVector
      val diff = CheckoutState.Diff.of("origin/main", stat, names, 2)
      (
        diff.stat.split("\n").toVector.takeRight(2),
        diff.stat.split("\n").length,
        diff.untracked,
        diff.more
      ) ==> (
        Vector(s" f${CheckoutState.StatLines - 1}.scala | 1 +", CheckoutState.Elided),
        CheckoutState.StatLines,
        names.take(CheckoutState.UntrackedFiles),
        2 + 25 - CheckoutState.UntrackedFiles
      )
    }

    test("a diff's stat and untracked names keep within their bytes, whole lines and names only") {
      val stat = Vector.fill(10)(" " + "x" * 400 + " | 1 +").mkString("\n")
      val names = Vector.fill(10)("y" * 300)
      val diff = CheckoutState.Diff.of("origin/main", stat, names)
      (
        diff.stat.getBytes("UTF-8").length <= CheckoutState.StatBytes,
        diff.stat.split("\n").forall(l => l == CheckoutState.Elided || l.endsWith("| 1 +")),
        diff.untracked.size,
        diff.more
      ) ==> (true, true, 3, 7)
    }

    test("a failed command keeps its first lines within its bound, and an exit of 0 as none") {
      val lines = Vector.fill(100)("[error] " + "z" * 60).mkString("\n")
      val failed = CheckoutState.Failed.of("make", Some(0), lines)
      (
        failed.exit,
        failed.lines.getBytes("UTF-8").length <= CheckoutState.FailedBytes,
        failed.lines.split("\n").forall(_ == "[error] " + "z" * 60)
      ) ==> (None, true, true)
    }
  }
}
