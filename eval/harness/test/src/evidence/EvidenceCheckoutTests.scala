package grit.eval.harness.evidence

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import grit.core.host.{Argv, Processes, RelPath}
import grit.dbos.internal.TrialRows
import grit.eval.harness.bench.{BenchTask, Check, Runner, Sha}
import grit.host.checkout.{LocalProcesses, Redaction}

import utest.*

/** [[EvidenceCheckout]] over a small repository made for each test: a base, and a commit after it.
  */
object EvidenceCheckoutTests extends TestSuite {

  private def rel(s: String): RelPath =
    RelPath.of(s).getOrElse(throw new java.lang.AssertionError(s))

  /** `git` with `args` in `dir`, its output trimmed; a failure throws, as a fixture's should. */
  private def git(work: Processes, dir: RelPath, args: String*): String =
    Argv
      .of(
        "git",
        (Seq("-c", "user.name=t", "-c", "user.email=t@t", "-c", "commit.gpgsign=false") ++ args)*
      )
      .left
      .map(new java.lang.AssertionError(_))
      .flatMap(work.run(_, dir, 30.seconds).left.map(e => new java.lang.AssertionError(e.message)))
      .fold(e => throw e, r => r.output.text.trim)

  /** A fresh root holding `origin`, a repository whose `a.txt` says `base`, then `later`, a commit
    * each; `body` of a processes over the root, the root, the origin's path and its base's task,
    * blind; the root deleted after.
    */
  private def withOrigin[A](body: (Processes, Path, String, BlindTask) => A): A = {
    val root = Files.createTempDirectory("grit-evidence")
    try {
      val work = new LocalProcesses(root, sys.env, Redaction.of(Map.empty))
      Files.createDirectory(root.resolve("origin"))
      val origin = rel("origin")
      val _ = git(work, origin, "init", "-q", "-b", "main")
      val commits = Vector("base", "later").map { text =>
        Files.writeString(root.resolve("origin/a.txt"), s"$text\n")
        val _ = git(work, origin, "add", "a.txt")
        val _ = git(work, origin, "commit", "-q", "-m", text)
        Sha
          .of(git(work, origin, "rev-parse", "HEAD"))
          .fold(e => throw new java.lang.AssertionError(e), identity)
      }
      val task = BenchTask(
        TrialRows.TaskName.of("fixture").getOrElse(throw new java.lang.AssertionError("name")),
        commits(0),
        "Do it.",
        commits.drop(1),
        Vector(Check(Runner.Check, Vector("grit.core")))
      )
      body(work, root, root.resolve("origin").toString, BlindTask.of(task))
    } finally Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
  }

  private def shown(files: (String, String)*): Shown =
    Shown
      .of(files.toVector.map((p, t) => rel(p) -> t))
      .fold(found => throw new java.lang.AssertionError(found.toString), identity)

  val tests = Tests {
    test("a checkout holds the task's base cloned under base, and each shown file under evidence") {
      withOrigin { (work, root, origin, task) =>
        val files =
          shown("brief.md" -> "Do it.", "trials/1/trajectory.md" -> "- calls `read` · é\n")
        EvidenceCheckout.write(work)(origin, task, files, rel("proposal")) ==> Right(())
        def read(p: String) = scala.util.Try(Files.readString(root.resolve(p))).toOption
        (
          read("proposal/base/a.txt"),
          read("proposal/evidence/brief.md"),
          read("proposal/evidence/trials/1/trajectory.md"),
          Files
            .list(root.resolve("proposal"))
            .iterator
            .asScala
            .map(_.getFileName.toString)
            .toVector
            .sorted
        ) ==> (
          Some("base\n"),
          Some("Do it."),
          Some("- calls `read` · é\n"),
          Vector("base", "evidence")
        )
        git(work, rel("proposal/base"), "rev-list", "--all").linesIterator.size ==> 1
      }
    }
    test(
      "a checkout is refused, with nothing written, for a file at the root, a path given twice, or one under another file"
    ) {
      withOrigin { (work, root, origin, task) =>
        val refused = Vector(
          shown("." -> "x"),
          shown("a.md" -> "x", "a.md" -> "y"),
          shown("a" -> "x", "a/b.md" -> "y")
        ).map(s => EvidenceCheckout.write(work)(origin, task, s, rel("proposal")))
        (refused, Files.exists(root.resolve("proposal"))) ==> (
          Vector(
            Left("an evidence file's path is the root: ."),
            Left("an evidence file's path is given twice: a.md"),
            Left("an evidence file's path lies under another's: a/b.md under a")
          ),
          false
        )
      }
    }

    test("a checkout into a directory that exists is refused, and what it holds is left alone") {
      withOrigin { (work, root, origin, task) =>
        Files.createDirectory(root.resolve("proposal"))
        Files.writeString(root.resolve("proposal/kept.md"), "mine")
        val refused =
          EvidenceCheckout.write(work)(origin, task, shown("brief.md" -> "Do it."), rel("proposal"))
        // mkdir's own words follow, in the machine's locale.
        (
          refused.left.map(_.startsWith("`mkdir` failed: mkdir: ")),
          Files
            .list(root.resolve("proposal"))
            .iterator
            .asScala
            .map(_.getFileName.toString)
            .toVector,
          Files.readString(root.resolve("proposal/kept.md"))
        ) ==> (Left(true), Vector("kept.md"), "mine")
      }
    }
  }
}
