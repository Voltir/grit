package grit.tools

import java.nio.charset.StandardCharsets.UTF_8

import grit.core.host.{Clipped, RelPath, RunOutput}

import utest.*

/** [[RunAnswer]] over real outputs, kept under `runs/` in the test resources: a Mill test run's
  * log with a failing utest assertion, and `scripts/check` with many failures and with one.
  */
object RunAnswerTests extends TestSuite {

  private val file: RelPath = RelPath.of(".grit/runs/p/3f9a2c1e.out") match {
    case Right(p) => p
    case Left(e) => throw new IllegalArgumentException(e.message)
  }

  private def fixture(name: String): String =
    Option(getClass.getResourceAsStream(s"/runs/$name")) match {
      case Some(in) =>
        try new String(in.readAllBytes(), UTF_8)
        finally in.close()
      case None => throw new IllegalStateException(s"no fixture runs/$name")
    }

  private def answer(text: String, failed: Boolean = true): String =
    RunAnswer.of("Output", RunOutput(text, file), failed)

  /** The answer's sections: its header, the flagged lines (when there are lines above the tail)
    * and its tail, as blank lines part them.
    */
  private def sections(answer: String): Vector[String] = answer.split("\n\n").toVector

  val tests = Tests {
    test(
      "an output of at most WholeBytes comes back whole after its heading; a byte more does not"
    ) {
      val one = fixture("check-one-failing.out")
      val full = "a" * RunAnswer.WholeBytes
      (answer(one), answer(full), sections(answer(full + "a")).headOption) ==> (
        s"Output:\n$one",
        s"Output:\n$full",
        Some(
          "Output: 1 line, 9 KB, too long to show whole. All of it is in " +
            "`.grit/runs/p/3f9a2c1e.out`, for `read` and `search`."
        )
      )
    }

    test(
      "a long scripts/check output: its size and file, its first failures numbered as the file has them, the rest counted, then its tail"
    ) {
      val text = fixture("check-many-failing.out")
      val got = sections(answer(text))
      val flagged = got.lift(1).toVector.flatMap(_.split("\n"))
      (
        got.headOption,
        flagged.take(5),
        flagged.lastOption,
        got.lift(2),
        got.size
      ) ==> (
        Some(
          "Output: 50 lines, 9 KB, too long to show whole. All of it is in " +
            "`.grit/runs/p/3f9a2c1e.out`, for `read` and `search`."
        ),
        Vector(
          "Of lines 1 to 21, those that look like errors, warnings or failing tests, and the 3 " +
            "after each:",
          "     2\tFAIL grit.core.session.SessionDigestTests > commits its agents made are one " +
            "line after the counters; a path under another worktree shows under its last segment",
          "     3\t" + Clipped.lines(text).lift(2).getOrElse("").take(500) + "…",
          "     4\t  at grit.core.session.SessionDigestTests$.$init$$$anonfun$1$$anonfun$2" +
            "(SessionDigestTests.scala:86)",
          "     5\tFAIL grit.core.recipe.PoolTests > a message source keeps the latest `most` " +
            "said in [t − within, t), shown oldest first"
        ),
        Some("Not shown: 4 more such lines; `search` the file for them."),
        // Line 42 alone is over 500 characters.
        Some(
          "Its last lines, 22 to 50:\n" + Clipped
            .lines(text)
            .drop(21)
            .map(l => if (l.length > 500) l.take(500) + "…" else l)
            .mkString("\n")
        ),
        3
      )
    }

    test(
      "a long Mill test log keeps utest's failing test and its AssertionError, numbered, and Mill's summary in its tail"
    ) {
      val got = sections(answer(fixture("mill-test-failing.out")))
      val flagged = got.lift(1).toVector.flatMap(_.split("\n"))
      (
        got.headOption,
        flagged.filter(l => l.contains(" X ") || l.contains("AssertionError")),
        got
          .lift(2)
          .exists(
            _.endsWith("grit.core.host.HostTests grit.core.host.HostTests.RelPath.normal form")
          )
      ) ==> (
        Some(
          "Output: 260 lines, 27 KB, too long to show whole. All of it is in " +
            "`.grit/runs/p/3f9a2c1e.out`, for `read` and `search`."
        ),
        Vector(
          "    48\t254-30] X grit.core.host.HostTests.RelPath.normal form 44ms ",
          "    49\t254-30]   java.lang.AssertionError: assertion failed: ==> assertion failed: " +
            "Right(.) != Right(..)",
          "    74\t254-30]     X normal form 44ms ",
          "    75\t254-30]       java.lang.AssertionError: assertion failed: ==> assertion " +
            "failed: Right(.) != Right(..)",
          "   143\t254-30] X grit.core.host.HostTests.RelPath.normal form 44ms ",
          "   144\t254-30]   java.lang.AssertionError: assertion failed: ==> assertion failed: " +
            "Right(.) != Right(..)"
        ),
        true
      )
    }

    test(
      "failing lines take the budget before warnings and exceptions do, and are shown in line order"
    ) {
      val warnings = (1 to 50).map(n =>
        f"[warn] src/pool/Lease$n%02d.scala:12:3: method borrow in class Pool is deprecated " +
          "since 2.1: use lease, which returns the connection when its block ends, instead"
      )
      val thrown = "java.lang.IllegalStateException: the pool was closed while a lease was out" +:
        Vector.fill(10)("\tat grit.pool.Pool.lease(Pool.scala:40)")
      val failure = Vector(
        "FAIL grit.pool.PoolTests > a lease returned is reused",
        "  assertion failed: ==> assertion failed: 1 != 2",
        "  at grit.pool.PoolTests$.$init$$$anonfun$1(PoolTests.scala:20)"
      )
      val text = (warnings ++ thrown ++ failure ++ (1 to 60).map(n => s"ok $n")).mkString("\n")
      val flagged = sections(answer(text)).lift(1).toVector.flatMap(_.split("\n"))
      (
        flagged.filter(_.contains("PoolTests")),
        flagged.lift(1).map(_.take(30)),
        flagged.lastOption
      ) ==> (
        Vector(
          "    62\tFAIL grit.pool.PoolTests > a lease returned is reused",
          "    64\t  at grit.pool.PoolTests$.$init$$$anonfun$1(PoolTests.scala:20)"
        ),
        Some("     1\t[warn] src/pool/Lease01"),
        Some("Not shown: 34 more such lines; `search` the file for them.")
      )
    }

    test(
      "with nothing flagged above the tail, a failed command is told what grit looks for, and a passing one that nothing looks wrong"
    ) {
      val text = ((1 to 300).map(n => s"step $n of the build went as it should") :+
        "FAIL grit.pool.PoolTests > a lease returned is reused").mkString("\n")
      (sections(answer(text)).lift(1), sections(answer(text, failed = false)).lift(1)) ==> (
        Some(
          "None of lines 1 to 261 matched what grit looks for (`[error]`, `FAILED`, `error:`, an " +
            "exception's name); `search` the file for what this command prints on a failure."
        ),
        Some("No line of lines 1 to 261 looks like an error, a warning or a failing test.")
      )
    }

    test(
      "Failing matches each tool's failure line, Noted a warning or an exception, and neither a passing line"
    ) {
      def kind(line: String): String =
        if (RunAnswer.Failing.findFirstIn(line).nonEmpty) "failing"
        else if (RunAnswer.Noted.findFirstIn(line).nonEmpty) "noted"
        else "neither"
      Vector(
        "190] [error] core/core/test/src/host/BlobsContract.scala:113:15",
        "FAIL grit.core.host.HostTests > RelPath.normal form",
        "254/254, 1 FAILED] ./mill grit.core.test 16s",
        "254-30]     X normal form 44ms",
        "  X normal form",
        "src/main.c:3:5: error: expected ';' before '}' token",
        "error[E0308]: mismatched types",
        "  assertion failed: ==> assertion failed: Right(.) != Right(..)",
        "-- [E007] Type Mismatch Error: src/A.scala:3:4",
        "--- FAIL: TestLease (0.00s)",
        "panic: runtime error: index out of range",
        "[warn] src/A.scala:3:4: unused import",
        "java.lang.IllegalStateException: the pool was closed",
        "ValueError: invalid literal for int()",
        "254-30]   + RelPath.normal form 44ms",
        "Tests: 109, Passed: 109, Failed: 0",
        "254-06] Running Test Class grit.core.files.FileErrorTests",
        "the X axis is drawn first"
      ).map(kind) ==> Vector.fill(11)("failing") ++ Vector.fill(3)("noted") ++
        Vector.fill(4)("neither")
    }
  }
}
