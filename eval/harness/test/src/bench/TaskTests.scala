package grit.eval.harness.bench

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import grit.core.host.RelPath
import grit.dbos.internal.TrialRows

import utest.*

object TaskTests extends TestSuite {

  private val name =
    TrialRows.TaskName.of("a-task").getOrElse(throw new java.lang.AssertionError("name"))

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
    BenchTask.of(name, text, brief, Map.empty).fold(identity, t => s"read: $t")

  val tests = Tests {
    test("a task file names its base, its commits in order, and each check with its runner") {
      val read =
        BenchTask.of(name, json(commits = s"[\"$known\", \"${"c" * 40}\"]"), "Do it.", Map.empty)
      read.map(t =>
        (
          Sha.value(t.base),
          t.commits.map(Sha.value),
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
          "Do it.\n\n" + BenchTask.closing
        )
      )
    }

    test("a task file's completion names its commits, its globs and the text its files hold") {
      val complete =
        """{"commits": 5, "exists": ["docs/decisions/0053-*.md"], "absent": ["**/*.orig"],
          |"holds": [{"path": "docs/decisions/0048-*.md", "text": "0053"}]}""".stripMargin
      val read = BenchTask.of(
        name,
        json().dropRight(1) + s""", "complete": $complete}""",
        "Do it.",
        Map.empty
      )
      (
        read.map(_.completion),
        BenchTask.of(name, json(), "Do it.", Map.empty).map(_.completion)
      ) ==> (
        Right(
          Some(
            Completion(
              5,
              Vector("docs/decisions/0053-*.md"),
              Vector("**/*.orig"),
              Vector(Holding("docs/decisions/0048-*.md", "0053"))
            )
          )
        ),
        Right(None)
      )
    }

    test("a task file's completion is refused for each way it falls short, naming what") {
      def completing(c: String) = refusal(json().dropRight(1) + s""", "complete": $c}""")
      Vector(
        completing("[]"),
        completing("""{"commits": -1, "exists": [], "absent": [], "holds": []}"""),
        completing("""{"commits": 1, "absent": [], "holds": []}"""),
        completing("""{"commits": 1, "exists": [], "absent": [], "holds": [{"path": "a"}]}""")
      ) ==> Vector(
        "a-task's complete names no commits",
        "a-task's complete's commits is below 0",
        "a-task's complete names no exists",
        "a-task's complete's holds names no text"
      )
    }

    test("a task's held files are read from its held directory, each at its repository path") {
      val dir = Files.createTempDirectory("grit-task").resolve("a-task")
      try {
        val at = dir.resolve("held/core/test/src/HeldATests.scala")
        val _ = Files.createDirectories(at.getParent)
        val _ = Files.writeString(at, "object HeldATests\n")
        val _ = Files.writeString(dir.resolve("brief.md"), "Do it.")
        val held =
          """{"files": ["core/test/src/HeldATests.scala"],
            |"checks": [{"runner": "check", "args": ["grit.core.HeldATests"]}]}""".stripMargin
        val _ =
          Files.writeString(dir.resolve("task.json"), json().dropRight(1) + s""", "held": $held}""")
        BenchTask
          .read(dir)
          .map(_.held.map(h => (h.files.map(f => (RelPath.value(f.path), f.text)), h.checks))) ==>
          Right(
            Some(
              (
                Vector("core/test/src/HeldATests.scala" -> "object HeldATests\n"),
                Vector(Check(Runner.Check, Vector("grit.core.HeldATests")))
              )
            )
          )
      } finally
        Files.walk(dir.getParent).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
    }

    test("a task's held tests are refused for each way they fall short, naming what") {
      def holding(h: String, files: Map[String, String] = Map("HeldATests.scala" -> "x")) =
        BenchTask
          .of(name, json().dropRight(1) + s""", "held": $h}""", "Do it.", files)
          .fold(identity, t => s"read: $t")
      val check = """[{"runner": "check", "args": ["grit.core.HeldATests"]}]"""
      Vector(
        holding("[]"),
        holding(s"""{"files": ["HeldBTests.scala"], "checks": $check}"""),
        holding(s"""{"files": ["ATests.scala"], "checks": $check}""", Map("ATests.scala" -> "x")),
        holding("""{"files": ["HeldATests.scala"], "checks": [{"runner": "check", "args": []}]}""")
      ) ==> Vector(
        "a-task's held names no files",
        "a-task's held file `HeldBTests.scala` is not under held/",
        "held file `ATests.scala` is not named `Held…`",
        "a-task's check of `check` has no arguments"
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

    test(
      "each frozen task reads, its brief.md lacking the sentence the rig gives every coder and its brief holding it once, at its end"
    ) {
      val dir = Path.of(sys.env.getOrElse("GRIT_BENCH_TASKS", "")).toAbsolutePath
      val names = Files.list(dir).iterator.asScala.map(_.getFileName.toString).toVector.sorted
      val read = names.map { n =>
        val written = Files.readString(dir.resolve(n).resolve("brief.md"))
        for {
          task <- BenchTask.read(dir.resolve(n))
        } yield (
          written.contains(rigSentence),
          task.brief.endsWith(rigSentence),
          task.brief.indexOf(rigSentence) == task.brief.lastIndexOf(rigSentence)
        )
      }
      (names, read) ==> (
        Vector(
          "anthropic-provider",
          "blobs-remove-all-but",
          "brief-raised",
          "commands-as-tools",
          "hook-keeps-events",
          "triage-retry"
        ),
        Vector.fill(6)(Right((false, true, true)))
      )
    }
  }

  private val rigSentence =
    "When you stop, the rig runs this task's checks itself. While you work, run the suite you " +
      "are changing; before you stop, run every module whose files you changed, each as " +
      "`scripts/check <module>`, and never a bare `scripts/check`."
}
