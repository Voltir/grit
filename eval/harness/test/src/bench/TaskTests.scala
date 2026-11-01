package grit.eval.harness.bench

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import grit.dbos.internal.BenchRows

import utest.*

object TaskTests extends TestSuite {

  private val name =
    BenchRows.TaskName.of("a-task").getOrElse(throw new java.lang.AssertionError("name"))

  private val base = "a" * 40
  private val known = "b" * 40

  private def json(
      base: String = s"\"$base\"",
      commits: String = s"[\"$known\"]",
      checks: String = """[{"runner": "check", "args": ["grit.core"]}, {"runner": "it",
                         |"args": ["grit.dbos.it.testOnly", "grit.dbos.engine.SqlStoreTests"]}]""".stripMargin
  ): String = s"""{"base": $base, "commits": $commits, "checks": $checks}"""

  /** The refusal of `text`, or what it read as when it was not refused. */
  private def refusal(text: String, brief: String = "Do it."): String =
    Task.of(name, text, brief).fold(identity, t => s"read: $t")

  val tests = Tests {
    test("a task file names its base, its commits in order, and each check with its runner") {
      val read = Task.of(name, json(commits = s"[\"$known\", \"${"c" * 40}\"]"), "Do it.")
      read.map(t =>
        (
          Commit.value(t.base),
          t.commits.map(Commit.value),
          t.checks,
          t.brief
        )
      ) ==> Right(
        (
          base,
          Vector(known, "c" * 40),
          Vector(
            Check(Runner.Check, Vector("grit.core")),
            Check(Runner.It, Vector("grit.dbos.it.testOnly", "grit.dbos.engine.SqlStoreTests"))
          ),
          "Do it.\n\n" + Task.closing
        )
      )
    }

    test("a task file is refused for each way it falls short, naming what") {
      Vector(
        refusal("not json"),
        refusal("[]"),
        refusal(json(base = "\"ABC\"")),
        refusal(json(commits = "[]")),
        refusal(json(commits = s"[\"$known\", \"short\"]")),
        refusal(json(checks = "[]")),
        refusal(json(checks = """[{"runner": "sh", "args": ["x"]}]""")),
        refusal(json(checks = """[{"runner": "check", "args": []}]""")),
        refusal(json(), brief = " \n")
      ) ==> Vector(
        "a-task's task.json is not JSON",
        "a-task's task.json names no base",
        "`ABC` is not a commit's full name: 40 lower-case hex digits",
        "a-task's task.json names no commits",
        "`short` is not a commit's full name: 40 lower-case hex digits",
        "a-task's task.json names no checks",
        "a-task's check runner `sh` is neither `check` nor `it`",
        "a-task's check of `check` has no arguments",
        "a-task's brief is empty"
      )
    }

    test("each frozen task's brief ends with the sentence the rig gives every coder") {
      val dir = Path.of(sys.env.getOrElse("GRIT_BENCH_TASKS", "")).toAbsolutePath
      val names = Files.list(dir).iterator.asScala.map(_.getFileName.toString).toVector.sorted
      val read = names.map { n =>
        for {
          taskName <- BenchRows.TaskName.of(n)
          task <- Task.of(
            taskName,
            Files.readString(dir.resolve(n).resolve("task.json")),
            Files.readString(dir.resolve(n).resolve("brief.md"))
          )
        } yield task.brief.endsWith(rigSentence)
      }
      (names, read) ==> (
        Vector("anthropic-provider", "blobs-remove-all-but", "commands-as-tools"),
        Vector(Right(true), Right(true), Right(true))
      )
    }
  }

  private val rigSentence =
    "When you stop, the rig runs this task's checks itself, so run only the suites you touch, " +
      "never a bare `scripts/check`."
}
