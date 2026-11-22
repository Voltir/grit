package grit.claudecode

import utest.*

/** scripts/check's record of a whole gate that passed, out/check/passed, over [[GateFixtures]]'
  * stubbed tiers.
  */
object GateRecordTests extends TestSuite {

  val tests = Tests {
    test(
      "a whole gate that passes records HEAD, its base, its flags and a clean tree at both ends"
    ) {
      GateFixtures { g =>
        val head = g.change("README.md" -> "a doc\n")
        val done = g.check(Seq("--timeout", "5m"))
        (done.exit, g.record) ==> ((
          0,
          Some(
            Vector(
              s"sha $head",
              s"end $head",
              s"base ${g.sha("main")}",
              "flags --timeout 5m",
              "it unreached",
              "clean-start yes",
              "clean-end yes"
            ).mkString("", "\n", "\n")
          )
        ))
      }
    }

    test("every run removes the record first, and a targeted run writes none") {
      GateFixtures { g =>
        val _ = g.change("README.md" -> "a doc\n")
        val whole = g.check(Seq()).exit
        val before = g.record.isDefined
        val targeted = g.check(Seq("grit.claudecode")).exit
        (whole, before, targeted, g.record) ==> ((0, true, 0, None))
      }
    }

    test("a gate with a failing tier, or run with --no-lint or --no-unused, writes no record") {
      GateFixtures { g =>
        val _ = g.change("README.md" -> "a doc\n")
        val failed = g.check(Seq(), Map("FAKE_MILL_FAILS" -> "__.test.testCached"))
        val failedRecord = g.record
        val unlinted = g.check(Seq("--no-lint"))
        val unlintedRecord = g.record
        val unused = g.check(Seq("--no-unused"))
        (failed.exit, failedRecord, unlinted.exit, unlintedRecord, unused.exit, g.record) ==>
          ((1, None, 0, None, 0, None))
      }
    }

    test("a tree with an untracked file when the gate begins is recorded as clean at neither end") {
      GateFixtures { g =>
        val _ = g.change("README.md" -> "a doc\n")
        val _ = g.machine.write(g.dir.resolve("stray.txt"), "untracked\n")
        val _ = g.check(Seq())
        g.record.map(_.linesIterator.filter(_.startsWith("clean")).toVector) ==>
          Some(Vector("clean-start no", "clean-end no"))
      }
    }

    test("a tree the gate dirtied, or a HEAD it moved, is recorded at the run's end") {
      GateFixtures { g =>
        val head = g.change("README.md" -> "a doc\n")
        val _ = g.check(Seq(), Map("FAKE_LAW_DIRTIES" -> "1"))
        val dirtied = g.record.map(_.linesIterator.filter(_.startsWith("clean")).toVector)
        val _ = g.sh("rm left-behind")
        val _ = g.check(Seq(), Map("FAKE_LAW_COMMITS" -> "1"))
        val moved = g.record.map(_.linesIterator.take(2).toVector)
        (dirtied, moved) ==> ((
          Some(Vector("clean-start yes", "clean-end no")),
          Some(Vector(s"sha $head", s"end ${g.sha("HEAD")}"))
        ))
      }
    }

    test(
      "a change to a module's sources reaches each it module depending on it: --it-reached runs those, a bare gate records them skipped"
    ) {
      GateFixtures { g =>
        val _ = g.change("core/dbos/src/A.scala" -> "object A\n")
        val bare = g.check(Seq())
        val bareIt = g.record.flatMap(_.linesIterator.find(_.startsWith("it ")))
        val reached = g.check(Seq("--it-reached"))
        val reachedIt = g.record.flatMap(_.linesIterator.find(_.startsWith("it ")))
        (bare.exit, bareIt, reached.exit, reachedIt, g.calls("scripts-it")) ==> ((
          0,
          Some("it skipped"),
          0,
          Some("it reached"),
          Vector("--color false --ticker false -k grit.app.it.testForked + grit.dbos.it.testForked")
        ))
      }
    }

    test(
      "a change reaching no it module records unreached; a build file, docker-compose.yml or an unreadable graph reaches every one"
    ) {
      GateFixtures { g =>
        def it(more: Map[String, String] = Map.empty): Option[String] = {
          val _ = g.check(Seq("--it-reached"), more)
          g.record.flatMap(_.linesIterator.find(_.startsWith("it ")))
        }
        val _ = g.change("extensions/claudecode/src/B.scala" -> "object B\n")
        val unreached = it()
        val _ = g.change("docker-compose.yml" -> "services: {}\n")
        val compose = it()
        val _ = g.change("README.md" -> "a doc\n")
        val unknown = it(Map("FAKE_MILL_FAILS" -> "show"))
        val all = "--color false --ticker false -k __.it.testForked"
        (unreached, compose, unknown, g.calls("scripts-it")) ==> ((
          Some("it unreached"),
          Some("it reached"),
          Some("it reached"),
          Vector(all, all)
        ))
      }
    }

    test(
      "--it records every integration suite run, and CHECK_BASE naming no commit is a usage error"
    ) {
      GateFixtures { g =>
        val _ = g.change("README.md" -> "a doc\n")
        val _ = g.check(Seq("--it"))
        val all = g.record.flatMap(_.linesIterator.find(_.startsWith("it ")))
        val bad = g.check(Seq(), Map("CHECK_BASE" -> "no-such-rev"))
        (all, bad.exit, bad.err.trim, g.record) ==>
          ((Some("it all"), 2, "check: CHECK_BASE=no-such-rev names no commit", None))
      }
    }
  }
}
