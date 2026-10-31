package grit.eval.harness.bench

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import grit.core.host.{Argv, Processes, RelPath}
import grit.dbos.internal.BenchRows
import grit.host.checkout.LocalProcesses

import utest.*

/** [[Clone]] over a small repository made for each test: a base, the commits a task knows, and a
  * later one.
  */
object CloneTests extends TestSuite {

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

  /** Whether `commit` can be reached in the repository at `dir`. */
  private def reaches(work: Processes, dir: RelPath, commit: String): Boolean =
    Argv
      .of("git", "cat-file", "-e", s"$commit^{commit}")
      .toOption
      .flatMap(work.run(_, dir, 30.seconds).toOption)
      .exists(_.exit == 0)

  /** The fixture's commits, by what each made `a.txt` say. */
  private final case class Origin(path: String, commits: Map[String, Commit])

  /** A repository at `origin` under a fresh root whose `a.txt` says each of `texts` in turn, one
    * commit each; `body` of a processes over the root and it, the root deleted after.
    */
  private def withOrigin[A](texts: String*)(body: (Processes, Origin) => A): A = {
    val root = Files.createTempDirectory("grit-clone")
    try {
      val work = new LocalProcesses(root, sys.env)
      Files.createDirectory(root.resolve("origin"))
      val origin = rel("origin")
      val _ = git(work, origin, "init", "-q", "-b", "main")
      val commits = texts.map { text =>
        Files.writeString(root.resolve("origin/a.txt"), s"$text\n")
        val _ = git(work, origin, "add", "a.txt")
        val _ = git(work, origin, "commit", "-q", "-m", text)
        text -> Commit
          .of(git(work, origin, "rev-parse", "HEAD"))
          .fold(e => throw new java.lang.AssertionError(e), identity)
      }
      body(work, Origin(root.resolve("origin").toString, commits.toMap))
    } finally Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
  }

  private def task(o: Origin, base: String, known: String*): Task =
    Task(
      BenchRows.TaskName.of("fixture").getOrElse(throw new java.lang.AssertionError("name")),
      o.commits(base),
      "Do it.",
      known.map(o.commits).toVector,
      Vector(Check(Runner.Check, Vector("grit.core")))
    )

  val tests = Tests {
    test("a checkout of a task's base reaches the base and none of the commits after it") {
      withOrigin("base", "known", "later") { (work, o) =>
        val checkout = rel("checkout")
        Clone.fresh(work)(o.path, o.commits("base"), checkout) ==> Right(())
        Vector("base", "known", "later").map(c =>
          reaches(work, checkout, Commit.value(o.commits(c)))
        ) ==>
          Vector(true, false, false)
        (
          git(work, checkout, "branch", "--show-current"),
          git(work, checkout, "remote"),
          Files.readString(Path.of(o.path).resolveSibling("checkout/a.txt"))
        ) ==> ("bench", "", "base\n")
      }
    }

    test("a checkout is never made over a directory already there") {
      withOrigin("base") { (work, o) =>
        Clone.fresh(work)(o.path, o.commits("base"), rel("origin")).isLeft ==> true
      }
    }

    test("a checkout picked holds the task's commits applied in order, and none after them") {
      withOrigin("base", "known", "more", "later") { (work, o) =>
        val checkout = rel("picked")
        Clone.picked(work)(o.path, task(o, "base", "known", "more"), checkout) ==> Right(())
        (
          Files.readString(Path.of(o.path).resolveSibling("picked/a.txt")),
          reaches(work, checkout, Commit.value(o.commits("later")))
        ) ==> ("more\n", false)
      }
    }

    test(
      "a checkout's changes from its base are its source lines, new files never added among them"
    ) {
      withOrigin("base") { (work, o) =>
        val checkout = rel("coded")
        Clone.fresh(work)(o.path, o.commits("base"), checkout) ==> Right(())
        val dir = Path.of(o.path).resolveSibling("coded")
        Files.writeString(dir.resolve("A.scala"), "object A\n")
        val _ = git(work, checkout, "add", "A.scala")
        val _ = git(work, checkout, "commit", "-q", "-m", "A")
        Files.writeString(dir.resolve("A.scala"), "object A {\n  val x = 1\n}\n")
        Files.writeString(dir.resolve("B.scala"), "object B\n")
        Files.writeString(dir.resolve("a.txt"), "not source\n")
        Clone.changed(work)(checkout, o.commits("base")).map(_.lines) ==> Right(
          Map(
            ("A.scala", '+', "object A {") -> 1,
            ("A.scala", '+', "  val x = 1") -> 1,
            ("A.scala", '+', "}") -> 1,
            ("B.scala", '+', "object B") -> 1
          )
        )
      }
    }

    test("a checkout picked is refused when a task's commit does not apply to its base") {
      withOrigin("base", "between", "known") { (work, o) =>
        Clone.picked(work)(o.path, task(o, "base", "known"), rel("picked")).left.map(_.take(25)) ==>
          Left("`git cherry-pick` failed:")
      }
    }
  }
}
