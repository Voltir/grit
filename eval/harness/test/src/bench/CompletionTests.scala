package grit.eval.harness.bench

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import grit.core.host.{Argv, Processes, RelPath}
import grit.host.checkout.{LocalProcesses, Redaction}

import utest.*

/** [[Completion]] over a checkout made for each test, whose `./mill` stands in for grit's compile:
  * it fails while the tree holds `broken.txt`; and [[Grade.of]] over what the bench read.
  */
object CompletionTests extends TestSuite {

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

  /** A checkout at `c` under a fresh root, its base commit holding `./mill` and `docs/0048.md`
    * ("replaced by nothing"); `body` of a processes over the root, the checkout's path, a commit
    * of it (each written file added and committed, its output the new HEAD) and the base; the
    * root deleted after.
    */
  private def withCheckout[A](
      body: (Processes, Path, Map[String, String] => Unit, Sha) => A
  ): A = {
    val root = Files.createTempDirectory("grit-completion")
    try {
      val work = new LocalProcesses(root, sys.env, Redaction.of(Map.empty))
      val dir = root.resolve("c")
      Files.createDirectories(dir.resolve("docs"))
      val c = rel("c")
      val _ = git(work, c, "init", "-q", "-b", "bench")
      Files.writeString(dir.resolve(".gitignore"), "out/\n")
      Files.writeString(dir.resolve("mill"), "#!/bin/sh\n[ ! -e broken.txt ]\n")
      Files.setPosixFilePermissions(
        dir.resolve("mill"),
        PosixFilePermissions.fromString("rwxr-xr-x")
      )
      Files.writeString(dir.resolve("docs/0048.md"), "replaced by nothing\n")
      val commit = (files: Map[String, String]) => {
        files.foreach((p, t) =>
          if (t.isEmpty) Files.delete(dir.resolve(p)) else Files.writeString(dir.resolve(p), t)
        )
        val _ = git(work, c, "add", "-A")
        val _ = git(
          work,
          c,
          "commit",
          "-q",
          "-m",
          if (files.isEmpty) "base" else files.keys.toVector.sorted.mkString(" ")
        )
      }
      commit(Map.empty)
      val base = Sha
        .of(git(work, c, "rev-parse", "HEAD"))
        .fold(e => throw new java.lang.AssertionError(e), identity)
      body(work, dir, commit, base)
    } finally Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
  }

  private val wanted = Completion(
    commits = 2,
    exists = Vector("docs/0053-*.md"),
    absent = Vector("**.orig"),
    holds = Vector(Holding("docs/0048.md", "0053"))
  )

  private def read(work: Processes, base: Sha): Either[String, Completed] =
    Completion.of(work)(rel("c"), base, wanted, () => None)

  val tests = Tests {
    test("a checkout that meets every item misses none and reads them all") {
      withCheckout { (work, _, commit, base) =>
        commit(Map("docs/0053-tools.md" -> "# 0053"))
        commit(Map("docs/0048.md" -> "replaced by 0053\n"))
        read(work, base) ==> Right(Completed.Nothing)
      }
    }

    test("a tree left with a change since its last commit misses the clean tree") {
      withCheckout { (work, dir, commit, base) =>
        commit(Map("docs/0053-tools.md" -> "# 0053"))
        commit(Map("docs/0048.md" -> "replaced by 0053\n"))
        Files.writeString(dir.resolve("a.txt"), "left\n")
        Files.createDirectories(dir.resolve("out"))
        Files.writeString(dir.resolve("out/built"), "ignored\n")
        read(work, base).map(_.missed) ==>
          Right(Vector("tree: 1 paths changed since the last commit, as a.txt"))
      }
    }

    test("a commit before HEAD that does not compile is missed by its subject, though HEAD does") {
      withCheckout { (work, _, commit, base) =>
        commit(Map("broken.txt" -> "x"))
        commit(Map("broken.txt" -> ""))
        commit(Map("docs/0053-tools.md" -> "# 0053", "docs/0048.md" -> "replaced by 0053\n"))
        read(work, base).map(_.missed.map(_.replaceFirst("commit \\w+ ", "commit <sha> "))) ==>
          Right(Vector("compiles: commit <sha> broken.txt does not compile"))
      }
    }

    test(
      "a missing record, a path left that should be gone and a file not naming it are each missed"
    ) {
      withCheckout { (work, _, commit, base) =>
        commit(Map("a.orig" -> "x"))
        commit(Map("docs/0052-other.md" -> "# 0052"))
        read(work, base).map(_.missed) ==> Right(
          Vector(
            "exists: no file matches `docs/0053-*.md`",
            "absent: `a.orig` matches `**.orig`",
            "holds: `docs/0048.md` does not hold `0053`"
          )
        )
      }
    }

    test("a checkout with too few commits misses the count") {
      withCheckout { (work, _, commit, base) =>
        commit(Map("docs/0053-tools.md" -> "# 0053", "docs/0048.md" -> "replaced by 0053\n"))
        read(work, base).map(_.missed) ==>
          Right(Vector("commits: 1 since the base, fewer than 2"))
      }
    }

    test("a diff alone reads its path items and leaves the commit-level ones not kept") {
      withCheckout { (work, _, _, _) =>
        Completion.ofDiff(work)(rel("c"), wanted) ==> Right(
          Completed(
            Vector(
              "exists: no file matches `docs/0053-*.md`",
              "holds: `docs/0048.md` does not hold `0053`"
            ),
            Vector("commits: not kept", "tree: not kept", "compiles: not kept")
          )
        )
      }
    }

    test(
      "a grade is None with no diff, Broken on a red check, Partial on a red held check or a miss"
    ) {
      val green = Checked(Vector("scripts/check grit.core" -> true), None)
      val red = Checked(Vector("scripts/check grit.core" -> false), None)
      val stopped = Checked(Vector.empty, Some("memory"))
      val missed = Completed(Vector("commits: 0 since the base, fewer than 1"), Vector.empty)
      val unread = Completed(Vector.empty, Vector("tree: not kept"))
      Vector(
        Grade.of(green, green, Completed.Nothing, changed = false),
        Grade.of(red, green, Completed.Nothing, changed = true),
        Grade.of(stopped, green, Completed.Nothing, changed = true),
        Grade.of(green, red, Completed.Nothing, changed = true),
        Grade.of(green, green, missed, changed = true),
        Grade.of(red, red, missed, changed = true),
        Grade.of(green, green, unread, changed = true),
        Grade.of(green, Checked(Vector.empty, None), Completed.Nothing, changed = true)
      ) ==> Vector(
        Grade.None,
        Grade.Broken,
        Grade.Broken,
        Grade.Partial,
        Grade.Partial,
        Grade.Broken,
        Grade.Complete,
        Grade.Complete
      )
    }
  }
}
