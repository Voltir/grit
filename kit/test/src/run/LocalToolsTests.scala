package grit.kit.run

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

import scala.concurrent.duration.DurationInt

import grit.core.clock.{Clock, Fresh}
import grit.core.edge.{Edges, Permit, Registration, ToolRequest}
import grit.core.host.{Allowlist, Argv, HostError}
import grit.core.id.{CallSlot, ConversationId, EdgeId, PrincipalId, TurnRef, TurnSeq}
import grit.core.place.{Directory, Namespace, Place, Repository, WorkName}
import grit.core.secret.{TestSecrets, Variable}
import grit.core.store.Origin
import grit.core.tool.{CallError, Outcome, Retry, ToolName}
import grit.host.checkout.Redaction
import grit.host.git.{GitFixture, WorkingCopies}
import grit.kit.deployment.Offered

import utest.*

/** [[LocalTools]] over an allowlisted offer, running real programs in a temporary checkout. */
object LocalToolsTests extends TestSuite {

  /** No classifier for the machine guard: what code does not clear is refused. */
  private val Unguarded: Either[String, grit.core.classify.Classifier^] = Left("none here")

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  /** Tools offering `offered`, given to `body`, closed after. */
  private def offering[A](offered: Offered, copies: Option[WorkingCopies^] = None)(
      body: LocalTools^ => A
  ): A = {
    val tools =
      new LocalTools(
        offered,
        Redaction.of(Map.empty),
        Clock.system(),
        Fresh.random(),
        copies,
        Unguarded,
        None
      )
    try body(tools)
    finally tools.close()
  }

  /** `tool` called with `arguments` by tools offering `offered` over the checkout `root`. */
  private def run(
      offered: Offered,
      root: Path,
      tool: String,
      arguments: ujson.Value,
      destination: Place => Option[Place] = Some(_)
  ): Outcome =
    offering(offered)(call(_, root, tool, arguments, destination))

  /** `tool` called with `arguments` by `tools` over the checkout `root`, its request's
    * destination what `destination` makes of the checkout's place.
    */
  private def call(
      tools: LocalTools^,
      root: Path,
      tool: String,
      arguments: ujson.Value,
      destination: Place => Option[Place] = _ => None
  ): Outcome = {
    val turn = TurnRef(ConversationId("c"), TurnSeq.First)
    val workspace = Place.of(right(Directory.of(root.toString)))
    val q = ToolRequest(
      CallSlot.of(turn, 0, 0).getOrElse(throw new java.lang.AssertionError()),
      ToolRequest.Protocol,
      turn.conversationId,
      workspace,
      PrincipalId.Local,
      right(ToolName.of(tool)),
      Permit.Free,
      Retry.Interrupt,
      arguments,
      Set.empty,
      destination(workspace)
    )
    val route = Edges
      .authorize(q, Registration(EdgeId("e"), PrincipalId.Local, Set(q.workspace)), None)
      .fold(r => throw new java.lang.AssertionError(r.toString), identity)
    tools.run(route, q, _ => ())
  }

  /** `tool` called with `arguments` by `tools` at `repository`'s place, from a conversation of
    * the run `run` of `of`, its request's destination `destination` of its workspace.
    */
  private def inRun(
      tools: LocalTools^,
      repository: Repository,
      of: Repository,
      run: WorkName,
      tool: String,
      arguments: ujson.Value,
      destination: Place => Option[Place] = Some(_)
  ): Outcome =
    from(tools, repository, Origin.Work(of, run), tool, arguments, destination)

  /** `tool` called with `arguments` by `tools` at `repository`'s place, from a conversation of
    * `origin`, its request's destination `destination` of its workspace.
    */
  private def from(
      tools: LocalTools^,
      repository: Repository,
      origin: Origin,
      tool: String,
      arguments: ujson.Value,
      destination: Place => Option[Place]
  ): Outcome = {
    val turn = TurnRef(ConversationId("c"), TurnSeq.First)
    val q = ToolRequest(
      CallSlot.of(turn, 0, 0).getOrElse(throw new java.lang.AssertionError()),
      ToolRequest.Protocol,
      turn.conversationId,
      repository.place,
      PrincipalId.Local,
      right(ToolName.of(tool)),
      Permit.Free,
      Retry.Interrupt,
      arguments,
      Set.empty,
      destination(repository.place)
    )
    val route = Edges
      .authorize(
        q,
        Registration(EdgeId("e"), PrincipalId.Local, Set(q.workspace)),
        Some(origin)
      )
      .fold(r => throw new java.lang.AssertionError(r.toString), identity)
    tools.run(route, q, _ => ())
  }

  /** The coding tools' writes and runs, asking no one. */
  private val writing = Offered.Allowlisted(Offered.ReferenceAllowlist)

  private def made(text: String): ujson.Value =
    ujson.Obj("path" -> "made.txt", "content" -> text)

  /** The remote repository's place the fixture's runs offer their branches at. */
  private val remotePlace = Place.under(Namespace.Service, Vector("github", "acme", "app"))

  /** `repo`'s runs pushed to its bare origin with a planted token, offered on `remoteRepository`. */
  private def pushing(
      repo: GitFixture.Repo^,
      remoteRepository: GitFixture.FakeRemoteRepository
  ): Pushing^ = {
    Pushing
      .of(
        repo.source,
        GitFixture.targetOf(remoteRepository),
        TestSecrets.of(Variable("GRIT_REMOTE_REPOSITORY_TOKEN"), "ghs_plantedRemoteToken123"),
        repo.environment,
        Redaction.of(Map.empty),
        remotePlace,
        "acme/app"
      ) match {
      case Right(p) => p
      case Left(why) => throw new java.lang.AssertionError(why)
    }
  }

  /** `body` given tools whose runs of the fixture's repository push to its origin on `remoteRepository`. */
  private def pushingIn[A](
      repo: GitFixture.Repo^,
      remoteRepository: GitFixture.FakeRemoteRepository
  )(
      body: (LocalTools^, WorkingCopies^) => A
  ): A = {
    val copies =
      new WorkingCopies(
        GitFixture.repository,
        repo.source,
        repo.root,
        repo.processes,
        Clock.system()
      )
    val tools = new LocalTools(
      writing,
      Redaction.of(Map.empty),
      Clock.system(),
      Fresh.random(),
      Some(copies),
      Unguarded,
      None,
      Some(pushing(repo, remoteRepository))
    )
    try body(tools, copies)
    finally tools.close()
  }

  private val offer =
    ujson.Obj("title" -> "Fix it", "body" -> "Fixed.", "to" -> "acme/app")

  private def originHead(repo: GitFixture.Repo^): Option[String] = {
    val ran = GitFixture.git(
      repo,
      repo.origin,
      "for-each-ref",
      "--format=%(objectname)",
      "refs/heads/grit/w1"
    )
    Some(ran.output.text.trim).filter(_.nonEmpty)
  }

  val tests = Tests {
    test(
      "a run's offer_pull_request pushes its branch to the declared repository and answers its pull request"
    ) {
      GitFixture.inRepository { repo =>
        val remoteRepository = new GitFixture.FakeRemoteRepository(repo.origin.toString)
        val w1 = GitFixture.work("w1")
        // The run's branch, one commit past main, as a run that committed its change leaves it.
        val _ = GitFixture.git(repo, repo.source, "checkout", "--quiet", "-B", "grit/w1")
        val _ = GitFixture.git(
          repo,
          repo.source,
          "-c",
          "user.name=t",
          "-c",
          "user.email=t@example.com",
          "commit",
          "--allow-empty",
          "-qm",
          "fix"
        )
        val _ = GitFixture.git(repo, repo.source, "checkout", "--quiet", "main")
        val said = pushingIn(repo, remoteRepository) { (tools, copies) =>
          copies
            .held(w1) { _ =>
              inRun(
                tools,
                GitFixture.repository,
                GitFixture.repository,
                w1,
                "offer_pull_request",
                offer,
                _ => Some(remotePlace)
              )
            }
            .left
            .map(_.message)
        }
        val local = GitFixture.git(repo, repo.source, "rev-parse", "grit/w1").output.text.trim
        (said, originHead(repo) == Some(local), remoteRepository.opens.size) ==> (
          Right(
            Outcome.Done(
              "The branch is pushed; its pull request is open at https://remote.invalid/pull/1."
            )
          ),
          true,
          1
        )
      }
    }
    test(
      "offer_pull_request told no destination, or its workspace, is misdirected and pushes nothing"
    ) {
      GitFixture.inRepository { repo =>
        val remoteRepository = new GitFixture.FakeRemoteRepository(repo.origin.toString)
        val w1 = GitFixture.work("w1")
        // The run's branch, one commit past main, so a push it were let make would reach origin.
        val _ = GitFixture.git(repo, repo.source, "checkout", "--quiet", "-B", "grit/w1")
        val _ = GitFixture.git(
          repo,
          repo.source,
          "-c",
          "user.name=t",
          "-c",
          "user.email=t@example.com",
          "commit",
          "--allow-empty",
          "-qm",
          "fix"
        )
        val _ = GitFixture.git(repo, repo.source, "checkout", "--quiet", "main")
        val said = pushingIn(repo, remoteRepository) { (tools, copies) =>
          copies
            .held(w1) { _ =>
              def told(to: Place => Option[Place]): Outcome =
                inRun(
                  tools,
                  GitFixture.repository,
                  GitFixture.repository,
                  w1,
                  "offer_pull_request",
                  offer,
                  to
                )
              Vector(told(_ => None), told(Some(_)))
            }
            .left
            .map(_.message)
        }
        def misdirected(to: Option[Place]): Outcome =
          CallError.Misdirected(right(ToolName.of("offer_pull_request")), to).outcome
        (said, originHead(repo), remoteRepository.opens.size) ==> (
          Right(
            Vector(misdirected(None), misdirected(Some(GitFixture.repository.place)))
          ),
          None,
          0
        )
      }
    }
    test(
      "offer_pull_request is offered at the repository's place alone, and only when its runs push"
    ) {
      GitFixture.inRepository { repo =>
        def names(set: Either[grit.core.tool.SetRefusal, grit.core.tool.ToolSet]) =
          set.toOption.toVector.flatMap(_.named(grit.tools.Versioning.Name)).map(_.name)
        val (atDirectory, atRepository) =
          pushingIn(repo, new GitFixture.FakeRemoteRepository(repo.origin.toString)) { (tools, _) =>
            (names(tools.hosted), names(tools.hostedInRepository))
          }
        val unpushed = offering(writing) { tools => names(tools.hostedInRepository) }
        (atDirectory, atRepository, unpushed) ==> (
          Vector(),
          Vector(ToolName("offer_pull_request")),
          Vector()
        )
      }
    }
    test("tools once closed run nothing") {
      val root = Files.createTempDirectory("closed").toRealPath()
      val _ = Files.writeString(root.resolve("a.txt"), "a")
      val tools =
        new LocalTools(
          Offered.Read,
          Redaction.of(Map.empty),
          Clock.system(),
          Fresh.random(),
          None,
          Unguarded,
          None
        )
      tools.close()
      call(tools, root, "list", ujson.Obj()) ==>
        Outcome.Failed("These coding tools are closed; nothing ran.")
    }

    test("a run's id reaches tail in a later request to the same checkout") {
      val root = Files.createTempDirectory("allowlisted").toRealPath()
      val script = root.resolve("slow")
      Files.writeString(script, "#!/bin/sh\necho begun\nsleep 1\necho finished\n")
      Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwx------"))
      val listed = Allowlist.Rule.Leading(right(Argv.of("./slow")))
      val offered =
        Offered.Allowlisted(right(Allowlist.of(Vector(listed), 5.seconds, Vector(listed))))
      offering(offered) { tools =>
        val started =
          call(tools, root, "run", ujson.Obj("program" -> "./slow", "wait" -> 0), Some(_))
        val id = started match {
          case Outcome.Done(text, _, _, _, _) =>
            "as `([0-9a-f]{8})`".r.findFirstMatchIn(text).map(_.group(1)).getOrElse("")
          case _ => ""
        }
        call(tools, root, "tail", ujson.Obj("id" -> id, "wait" -> 5)) ==>
          Outcome.Done("`./slow`\nExit code 0. Output:\nbegun\nfinished\n")
      }
    }

    test("a long run's failing line is numbered as its file has it: read there returns that line") {
      val root = Files.createTempDirectory("allowlisted").toRealPath()
      val script = root.resolve("build")
      // Coloured lines redrawn by carriage returns, then the failure on line 100.
      Files.writeString(
        script,
        "#!/bin/sh\nfor i in $(seq 1 99); do\n" +
          "  printf '\\033[32m+\\033[0m step %d of the build: 10%%\\r20%%\\r30%%\\n' \"$i\"\ndone\n" +
          "echo 'FAIL grit.pool.PoolTests > a lease returned is reused'\n" +
          "for i in $(seq 101 300); do echo \"ok $i: this step of the build went as it should\"; done\n" +
          "exit 1\n"
      )
      Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwx------"))
      val listed = Allowlist.Rule.Leading(right(Argv.of("./build")))
      val offered =
        Offered.Allowlisted(right(Allowlist.of(Vector(listed), 5.seconds, Vector(listed))))
      def text(o: Outcome): String = o match {
        case Outcome.Done(t, _, _, _, _) => t
        case other => s"not done: $other"
      }
      offering(offered) { tools =>
        val answer = text(call(tools, root, "run", ujson.Obj("program" -> "./build"), Some(_)))
        val file = "`(\\.grit/runs/[^`]+)`".r.findFirstMatchIn(answer).map(_.group(1)).getOrElse("")
        val read = text(
          call(tools, root, "read", ujson.Obj("path" -> file, "offset" -> 100, "limit" -> 1))
        )
        val failing = "   100\tFAIL grit.pool.PoolTests > a lease returned is reused"
        (answer.split("\n").filter(_.contains("FAIL")).toVector, read.split("\n").headOption) ==>
          (Vector(failing), Some(failing))
      }
    }

    test(
      "an allowlisted run starts a listed program named relative to the checkout's root there, and refuses an unlisted one, which leaves nothing"
    ) {
      val root = Files.createTempDirectory("allowlisted").toRealPath()
      val script = root.resolve("hello")
      Files.writeString(script, "#!/bin/sh\necho \"hello from $(pwd)\"\n")
      Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwx------"))
      val listed = Allowlist.Rule.Leading(right(Argv.of("./hello")))
      val offered =
        Offered.Allowlisted(right(Allowlist.of(Vector(listed), 5.seconds, Vector(listed))))
      val touch = right(Argv.of("touch", "x"))
      (
        run(offered, root, "run", ujson.Obj("program" -> "./hello")),
        run(offered, root, "run", ujson.Obj("program" -> "touch", "args" -> ujson.Arr("x"))),
        Files.exists(root.resolve("x"))
      ) ==> (
        Outcome.Done(s"Exit code 0. Output:\nhello from $root\n"),
        Outcome.Failed(
          s"${HostError.Unlisted(touch).message} Only these programs run here: `./hello`, each " +
            "as the run tool's description lists it."
        ),
        false
      )
    }

    test(
      "a cleared program whose argument leads out of the checkout through a link is refused by the machine guard, running nothing"
    ) {
      val base = Files.createTempDirectory("guarded").toRealPath()
      val root = Files.createDirectories(base.resolve("co"))
      val script = root.resolve("hello")
      Files.writeString(script, "#!/bin/sh\ntouch ran\n")
      Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwx------"))
      val _ = Files.createSymbolicLink(root.resolve("out"), base)
      val listed = Allowlist.Rule.Leading(right(Argv.of("./hello")))
      val offered =
        Offered.Allowlisted(right(Allowlist.of(Vector(listed), 5.seconds, Vector(listed))))
      val answered =
        run(offered, root, "run", ujson.Obj("program" -> "./hello", "args" -> ujson.Arr("out/x")))
      (answered, Files.exists(root.resolve("ran"))) ==> (
        Outcome.Failed(
          "Nothing ran: the check that guards this machine clears by itself only `git status …`, " +
            "`git diff …`, `git log …`, `git show …`, `ls …`, `cat …`, `head …`, `tail …`, " +
            "`wc …`, `grep …`, `diff …`, `find …`, `sed -n …`, `git add …`, `git commit …`, " +
            "`git restore …`, `git checkout -- …`, `git stash push …`, `git stash list …`, " +
            "`git stash show …`, `git stash apply …`, `git stash`, " +
            "`./hello …`, each with every argument inside the checkout, and this command has an " +
            s"argument leading outside it: `out/x` leads to $base/x. No check is configured here " +
            "to ask (none here), so it will be refused again: do not retry it."
        ),
        false
      )
    }

    test(
      "a change to the checkout runs only when its request's destination is the checkout: write, edit and run told none, or another place, are misdirected and change nothing"
    ) {
      val root = Files.createTempDirectory("changing").toRealPath()
      val script = root.resolve("hello")
      Files.writeString(script, "#!/bin/sh\necho hi > ran\n")
      Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwx------"))
      val listed = Allowlist.Rule.Leading(right(Argv.of("./hello")))
      val offered =
        Offered.Allowlisted(right(Allowlist.of(Vector(listed), 5.seconds, Vector(listed))))
      val other = Place.of(right(Directory.of("/elsewhere")))
      def misdirected(tool: String, to: Option[Place]): Outcome =
        CallError.Misdirected(right(ToolName.of(tool)), to).outcome
      val made = ujson.Obj("path" -> "made.txt", "content" -> "made\n")
      val edited = ujson.Obj(
        "path" -> "made.txt",
        "edits" -> ujson.Arr(ujson.Obj("oldText" -> "made", "newText" -> "edited"))
      )
      val hello = ujson.Obj("program" -> "./hello")
      (
        run(offered, root, "write", made, _ => None),
        run(offered, root, "write", made, _ => Some(other)),
        Files.exists(root.resolve("made.txt")),
        run(offered, root, "write", made),
        run(offered, root, "edit", edited, _ => None),
        run(offered, root, "run", hello, _ => None),
        Files.readString(root.resolve("made.txt")),
        Files.exists(root.resolve("ran"))
      ) ==> (
        misdirected("write", None),
        misdirected("write", Some(other)),
        false,
        Outcome.Done("Wrote made.txt: 1 line."),
        misdirected("edit", None),
        misdirected("run", None),
        "made\n",
        false
      )
    }

    test(
      "a repository's request runs in its run's working copy, and the clone is left as it was"
    ) {
      GitFixture.inRepository { repo =>
        val copies = new WorkingCopies(
          GitFixture.repository,
          repo.source,
          repo.root,
          repo.processes,
          Clock.system()
        )
        val w1 = GitFixture.work("w1")
        offering(writing, Some(copies)) { tools =>
          right(
            copies
              .held(w1) { copy =>
                (
                  inRun(
                    tools,
                    GitFixture.repository,
                    GitFixture.repository,
                    w1,
                    "write",
                    made("a\n")
                  ),
                  Some(copy.resolve("made.txt")).filter(Files.exists(_)).map(Files.readString),
                  Files.exists(repo.source.resolve("made.txt"))
                )
              }
              .left
              .map(_.message)
          )
        } ==> (Outcome.Done("Wrote made.txt: 1 line."), Some("a\n"), false)
      }
    }

    test(
      "a coding child of a run works in its own copy on grit/{name}, leaving the run's copy untouched"
    ) {
      GitFixture.inRepository { repo =>
        for ((k, v) <- Vector("user.name" -> "t", "user.email" -> "t@example.com"))
          GitFixture.git(repo, repo.source, "config", k, v)
        val remoteRepository = new GitFixture.FakeRemoteRepository(repo.origin.toString)
        val w1 = GitFixture.work("w1")
        val name = GitFixture.work("s1-0-0123456789abcdef")
        val child = Origin.Spawned(
          TurnRef(ConversationId("c-parent"), TurnSeq.First),
          name,
          GitFixture.repository.place
        )
        def asked(tool: String, arguments: ujson.Value, to: Place => Option[Place] = Some(_))(
            tools: LocalTools^
        ) = from(tools, GitFixture.repository, child, tool, arguments, to)
        val (said, parentCopy) = pushingIn(repo, remoteRepository) { (tools, copies) =>
          right(
            copies
              .held(w1) { mine =>
                // The run's own work in progress, never committed.
                val _ = Files.writeString(mine.resolve("draft.txt"), "draft\n")
                val said = Vector(
                  asked("write", made("fixed\n"))(tools),
                  asked(
                    "run",
                    ujson.Obj("program" -> "git", "args" -> ujson.Arr("add", "made.txt"))
                  )(
                    tools
                  ),
                  asked(
                    "run",
                    ujson.Obj("program" -> "git", "args" -> ujson.Arr("commit", "-qm", "fix"))
                  )(tools),
                  asked("offer_pull_request", offer, _ => Some(remotePlace))(tools)
                )
                (
                  said.map(_.getClass.getSimpleName),
                  (
                    Some(mine.resolve("draft.txt")).filter(Files.exists(_)).map(Files.readString),
                    Files.exists(mine.resolve("made.txt"))
                  )
                )
              }
              .left
              .map(_.message)
          )
        }
        val branch = s"grit/${WorkName.value(name)}"
        val local = GitFixture.git(repo, repo.source, "rev-parse", branch).output.text.trim
        val pushed = GitFixture
          .git(repo, repo.origin, "for-each-ref", "--format=%(objectname)", s"refs/heads/$branch")
          .output
          .text
          .trim
        val holds = GitFixture.git(repo, repo.source, "show", s"$branch:made.txt").output.text
        (said, parentCopy, pushed == local, holds, remoteRepository.opens.size) ==> (
          Vector("Done", "Done", "Done", "Done"),
          (Some("draft\n"), false),
          true,
          "fixed\n",
          1
        )
      }
    }

    test("a request of one run never lands in another run's copy") {
      GitFixture.inRepository { repo =>
        val copies = new WorkingCopies(
          GitFixture.repository,
          repo.source,
          repo.root,
          repo.processes,
          Clock.system()
        )
        val (w1, w2) = (GitFixture.work("w1"), GitFixture.work("w2"))
        offering(writing, Some(copies)) { tools =>
          right(
            copies
              .held(w1) { one =>
                right(
                  copies
                    .held(w2) { two =>
                      val _ = inRun(
                        tools,
                        GitFixture.repository,
                        GitFixture.repository,
                        w1,
                        "write",
                        made("one\n")
                      )
                      (
                        Some(one.resolve("made.txt")).filter(Files.exists(_)).map(Files.readString),
                        Files.exists(two.resolve("made.txt"))
                      )
                    }
                    .left
                    .map(_.message)
                )
              }
              .left
              .map(_.message)
          )
        } ==> (Some("one\n"), false)
      }
    }

    test(
      "a request for a repository whose copies these tools do not keep fails, and nothing runs"
    ) {
      GitFixture.inRepository { repo =>
        val copies = new WorkingCopies(
          GitFixture.repository,
          repo.source,
          repo.root,
          repo.processes,
          Clock.system()
        )
        val other = right(Repository.of("other"))
        val w1 = GitFixture.work("w1")
        offering(writing, Some(copies)) { tools =>
          inRun(tools, other, other, w1, "write", made("a\n"))
        } ==> Outcome.Failed("This edge keeps no working copy of repository:other; nothing ran.")
        Files.exists(repo.root.resolve("other")) ==> false
      }
    }
  }
}
