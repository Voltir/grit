package grit.host.git

import java.net.URI
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import grit.core.clock.SetClock
import grit.core.host.{Argv, Branch, Handing, HostError, RelPath, Started, Variables}
import grit.core.secret.{Secret, TestSecrets, Variable}
import grit.host.checkout.{LocalHanding, LocalProcesses, Redaction}
import grit.host.git.GitFixture.*

import utest.*

/** [[LocalBranch]] over a real clone, its declared repository the fixture's bare origin, and a
  * remote repository kept in memory.
  */
object LocalBranchTests extends TestSuite {

  private val token = "ghs_plantedRemoteToken123"

  private val secret: Secret = TestSecrets.of(Variable("GRIT_REMOTE_REPOSITORY_TOKEN"), token)

  /** A [[Handing]] that keeps each argv it builds, and the variables it is given. */
  private final class Recording(inner: Handing^) extends Handing {
    val argvs = new java.util.concurrent.CopyOnWriteArrayList[Argv]()
    val variables = new java.util.concurrent.CopyOnWriteArrayList[Map[String, String]]()
    def started[A](dir: RelPath, secret: Secret, vars: Variables)(
        argv: String -> Either[String, Argv]
    )(body: Started^ => A): Either[HostError, A] = {
      val _ = variables.add(vars.values)
      inner.started(dir, secret, vars) { path =>
        val built = argv(path)
        built.foreach(a => argvs.add(a))
        built
      }(body)
    }
  }

  private def handing(repo: Repo^): LocalHanding =
    new LocalHanding(repo.source, repo.environment, Redaction.of(Map.empty))

  private def branch(repo: Repo^, remoteRepository: FakeRemoteRepository): LocalBranch^ =
    handedBy(repo, remoteRepository, handing(repo))

  /** `grit/w1` of `repo`'s clone, git run there, handed `secret` by `by`. */
  private def handedBy(
      repo: Repo^,
      remoteRepository: FakeRemoteRepository,
      by: Handing^
  ): LocalBranch^ = {
    val inClone = new LocalProcesses(repo.source, repo.environment, Redaction.of(Map.empty))
    PushTarget.of(remoteRepository) match {
      case Right(target) => new LocalBranch(target, work("w1"), inClone, by, secret)
      case Left(why) => throw new java.lang.AssertionError(why)
    }
  }

  private val commit = Seq("-c", "user.name=g", "-c", "user.email=g@example.invalid")

  /** `grit/w1` in the clone, one commit `subject` past main, the clone left on main. */
  private def onBranch(repo: Repo^, subject: String): Unit = {
    val _ = git(repo, repo.source, "checkout", "--quiet", "-B", "grit/w1")
    val _ = git(repo, repo.source, (commit ++ Seq("commit", "--allow-empty", "-qm", subject))*)
    val _ = git(repo, repo.source, "checkout", "--quiet", "main")
  }

  private def refs(repo: Repo^, in: Path): Vector[String] =
    git(repo, in, "for-each-ref", "--format=%(refname)").output.text.linesIterator.toVector

  private def at(repo: Repo^, in: Path, ref: String): String =
    git(repo, in, "rev-parse", ref).output.text.trim

  private def right[A](e: Either[HostError, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why.message), identity)

  val tests = Tests {
    test("offer pushes grit/{work} alone to the declared repository, and opens its pull request") {
      inRepository { repo =>
        onBranch(repo, "fix")
        val _ = git(repo, repo.source, "branch", "other")
        val _ = git(repo, repo.source, "tag", "v1")
        val _ = git(repo, repo.source, "config", "push.followTags", "true")
        val remoteRepository = new FakeRemoteRepository(repo.origin.toString)
        val offered = branch(repo, remoteRepository).offer("Fix it", "Fixed.")
        (
          offered,
          refs(repo, repo.origin),
          at(repo, repo.origin, "refs/heads/grit/w1") == at(repo, repo.source, "grit/w1"),
          remoteRepository.opens.asScala.toVector
        ) ==> (
          Right(URI.create("https://remote.invalid/pull/1")),
          Vector("refs/heads/grit/w1", "refs/heads/main"),
          true,
          Vector(("grit/w1", "Fix it", "Fixed."))
        )
      }
    }
    test(
      "an offer of a branch holding nothing origin's default lacks pushes nothing and opens nothing"
    ) {
      inRepository { repo =>
        val _ = git(repo, repo.source, "branch", "grit/w1", "main")
        val remoteRepository = new FakeRemoteRepository(repo.origin.toString)
        val offered = branch(repo, remoteRepository).offer("Fix it", "Fixed.")
        (
          offered,
          refs(repo, repo.origin),
          remoteRepository.opens.asScala.toVector,
          remoteRepository.asked.asScala.toVector
        ) ==> (
          Left(
            HostError.Failed(
              "grit/w1 holds nothing origin's default branch lacks, so nothing was pushed and " +
                "no pull request was opened. Commit your change first."
            )
          ),
          Vector("refs/heads/main"),
          Vector.empty,
          Vector.empty
        )
      }
    }
    test(
      "a branch the repository holds diverged from is refused, never forced, and nothing opens"
    ) {
      inRepository { repo =>
        onBranch(repo, "first")
        val _ = git(repo, repo.source, "push", "--quiet", "origin", "grit/w1")
        val held = at(repo, repo.origin, "refs/heads/grit/w1")
        val _ = git(repo, repo.source, "branch", "-f", "grit/w1", "main")
        onBranch(repo, "rewritten")
        val remoteRepository = new FakeRemoteRepository(repo.origin.toString)
        val offered = branch(repo, remoteRepository).offer("Fix it", "Fixed.")
        (
          offered.left.map(_.message).left.exists(_.contains("Pushing grit/w1")),
          at(repo, repo.origin, "refs/heads/grit/w1"),
          remoteRepository.opens.size
        ) ==> (true, held, 0)
      }
    }
    test("offered again while open, it pushes what was committed since and opens none") {
      inRepository { repo =>
        onBranch(repo, "first")
        val remoteRepository = new FakeRemoteRepository(repo.origin.toString)
        val first = branch(repo, remoteRepository).offer("Fix it", "Fixed.")
        val _ = git(repo, repo.source, "checkout", "--quiet", "grit/w1")
        val _ = git(repo, repo.source, (commit ++ Seq("commit", "--allow-empty", "-qm", "more"))*)
        val second = branch(repo, remoteRepository).offer("Fix it again", "Again.")
        (
          first == second,
          at(repo, repo.origin, "refs/heads/grit/w1") == at(repo, repo.source, "grit/w1"),
          remoteRepository.opens.size
        ) ==> (true, true, 1)
      }
    }
    test("offered after its pull request closed, it is refused, pushes nothing and opens none") {
      inRepository { repo =>
        onBranch(repo, "first")
        val remoteRepository = new FakeRemoteRepository(repo.origin.toString)
        val b = branch(repo, remoteRepository)
        val _ = right(b.offer("Fix it", "Fixed."))
        val pushed = at(repo, repo.origin, "refs/heads/grit/w1")
        remoteRepository.close("grit/w1", pushed)
        val _ = git(repo, repo.source, "checkout", "--quiet", "grit/w1")
        val _ = git(repo, repo.source, (commit ++ Seq("commit", "--allow-empty", "-qm", "more"))*)
        val again = b.offer("Fix it again", "Again.")
        (
          again.left.map(_.message),
          at(repo, repo.origin, "refs/heads/grit/w1") == pushed,
          remoteRepository.opens.size
        ) ==> (
          Left(
            "The pull request from grit/w1 at https://remote.invalid/pull/1 was closed; grit " +
              "does not open another. Nothing was pushed. Run the task under a new --run name " +
              "to offer the change again."
          ),
          true,
          1
        )
      }
    }
    test(
      "a branch stands Unpushed, Pushed with none opened, Open once offered, Closed or Merged whatever was committed since"
    ) {
      inRepository { repo =>
        onBranch(repo, "first")
        val remoteRepository = new FakeRemoteRepository(repo.origin.toString)
        val b = branch(repo, remoteRepository)
        val before = b.stands
        val _ = git(repo, repo.source, "push", "--quiet", repo.origin.toString, "grit/w1")
        val pushed = b.stands
        val offered = right(b.offer("Fix it", "Fixed."))
        val open = b.stands
        val head = at(repo, repo.origin, "refs/heads/grit/w1")
        val _ = git(repo, repo.source, "checkout", "--quiet", "grit/w1")
        val _ = git(repo, repo.source, (commit ++ Seq("commit", "--allow-empty", "-qm", "more"))*)
        val _ = git(repo, repo.source, "checkout", "--quiet", "main")
        val unpushed = b.stands
        remoteRepository.close("grit/w1", head)
        val closed = b.stands
        val merging = new FakeRemoteRepository(repo.origin.toString)
        val m = branch(repo, merging)
        val merged = right(m.offer("Fix it", "Fixed."))
        merging.merge("grit/w1", at(repo, repo.origin, "refs/heads/grit/w1"))
        (before, pushed, open, unpushed, closed, m.stands) ==> (
          Right(Branch.Stands.Unpushed),
          Right(Branch.Stands.Pushed),
          Right(Branch.Stands.Open(offered)),
          Right(Branch.Stands.Unpushed),
          Right(Branch.Stands.Closed(offered)),
          Right(Branch.Stands.Merged(merged))
        )
      }
    }
    test(
      "the token reaches git only by the helper's file, asked for the declared repository alone, hooks off"
    ) {
      inRepository { repo =>
        onBranch(repo, "first")
        val recording = new Recording(handing(repo))
        val _ = right(
          handedBy(repo, new FakeRemoteRepository(repo.origin.toString), recording)
            .offer("Fix it", "Fixed.")
        )
        val pushed = recording.argvs.asScala.toVector.headOption.map(_.args).getOrElse(Nil)
        val declared = repo.origin.toString
        (
          pushed.exists(_.contains(token)),
          recording.variables.asScala.exists(_.values.exists(_.contains(token))),
          pushed.take(6).map(_.takeWhile(_ != '!')),
          pushed.drop(6)
        ) ==> (
          false,
          false,
          List(
            "-c",
            "credential.helper=",
            "-c",
            s"credential.$declared.helper=",
            "-c",
            "core.hooksPath=/dev/null"
          ),
          List(
            "push",
            "--quiet",
            "--no-follow-tags",
            declared,
            "refs/heads/grit/w1:refs/heads/grit/w1"
          )
        )
      }
    }
    test("the helper, asked for a credential, answers the password read from its file") {
      val dir = Files.createTempDirectory("grit-helper")
      try {
        val file = Files.writeString(dir.resolve("secret"), token)
        val shell = LocalBranch.helper(file.toString).stripPrefix("!")
        val p = new ProcessBuilder("sh", "-c", s"$shell get").redirectErrorStream(true).start()
        val said = new String(p.getInputStream.readAllBytes())
        (p.waitFor(), said) ==> (0, s"username=x-access-token\npassword=$token\n")
      } finally {
        Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
      }
    }
    test("a handed path the helper could not name unquoted is refused, and nothing runs") {
      val to = Remote.of("/srv/repo.git")
      (
        to.flatMap(LocalBranch.argv(_, "/tmp/a b/secret", "push")).isLeft,
        to.flatMap(LocalBranch.argv(_, "/tmp/a;rm/secret", "push")).isLeft,
        to.flatMap(LocalBranch.argv(_, "/tmp/grit-handed-1/secret", "push")).isRight
      ) ==> (true, true, true)
    }
    test("a remote is an https address with no user in it, or an absolute path") {
      Vector(
        "https://github.com/acme/app.git",
        "/srv/app.git",
        "http://github.com/acme/app.git",
        "https://x:ghs_t@github.com/acme/app.git",
        "relative/app.git",
        "origin",
        "https://github.com/acme/app.git\n"
      ).map(Remote.of(_).isRight) ==> Vector(true, true, false, false, false, false, false)
    }
    test(
      "git fetches from a remote only at its own host and path, by https, or ssh as git on port 22"
    ) {
      val github = Remote.of("https://github.com/acme/app.git").fold(sys.error, identity)
      val path = Remote.of("/srv/app.git").fold(sys.error, identity)
      Vector(
        github -> "https://github.com/acme/app.git",
        github -> "https://GitHub.com/acme/app",
        github -> "https://github.com/acme/app/",
        github -> "git@github.com:acme/app.git",
        github -> "ssh://git@github.com/acme/app.git",
        github -> "ssh://git@github.com:22/acme/app",
        path -> "/srv/app.git",
        github -> "https://github.com/acme/App.git",
        github -> "https://github.com/acme/app.git.git",
        github -> "https://www.github.com/acme/app.git",
        github -> "http://github.com/acme/app.git",
        github -> "git://github.com/acme/app.git",
        github -> "https://x:ghs_t@github.com/acme/app.git",
        github -> "https://github.com:443/acme/app.git",
        github -> "me@github.com:acme/app.git",
        github -> "ssh://git@github.com:2222/acme/app.git",
        github -> "ssh://me@github.com/acme/app.git",
        github -> "https://github.com/acme/app.git/x",
        path -> "/srv/other.git",
        path -> "file:///srv/app.git"
      ).map((remote, url) => remote.fetchedBy(url)) ==>
        Vector.fill(7)(true) ++ Vector.fill(13)(false)
    }
    test("a remote's address is shown without the user and password in it") {
      Vector(
        "https://x:ghs_secret@github.com/acme/app.git",
        "https://ghs_secret@github.com/acme/app.git",
        "git@github.com:acme/app.git",
        "/srv/app.git"
      ).map(Remote.shown) ==> Vector(
        "https://github.com/acme/app.git",
        "https://github.com/acme/app.git",
        "git@github.com:acme/app.git",
        "/srv/app.git"
      )
    }
    test(
      "a copy whose .git a run rewrote still pushes the clone's branch to the declared repository"
    ) {
      inRepository { repo =>
        val copies = new WorkingCopies(
          repository,
          repo.source,
          repo.root,
          repo.processes,
          new SetClock(java.time.Instant.EPOCH)
        )
        val copy = right(copies.opened(work("w1")))
        val _ = git(repo, copy, (commit ++ Seq("commit", "--allow-empty", "-qm", "in copy"))*)
        val elsewhere = Files.createDirectories(repo.root.resolveSibling("planted"))
        Files.writeString(copy.resolve(".git"), s"gitdir: $elsewhere\n")
        val _ = right(
          branch(repo, new FakeRemoteRepository(repo.origin.toString)).offer("Fix it", "Fixed.")
        )
        at(repo, repo.origin, "refs/heads/grit/w1") ==> at(repo, repo.source, "grit/w1")
      }
    }
    test("the clone's hooks do not run when its branch is pushed") {
      inRepository { repo =>
        onBranch(repo, "first")
        val ran = repo.root.resolveSibling("hook-ran")
        val hook = repo.source.resolve(".git").resolve("hooks").resolve("pre-push")
        Files.createDirectories(hook.getParent)
        Files.writeString(hook, s"#!/bin/sh\ntouch $ran\n")
        hook.toFile.setExecutable(true)
        val offered =
          branch(repo, new FakeRemoteRepository(repo.origin.toString)).offer("Fix it", "Fixed.")
        (offered.isRight, Files.exists(ran)) ==> (true, false)
      }
    }
  }
}
