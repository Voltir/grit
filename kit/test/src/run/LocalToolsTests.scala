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

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  /** Tools offering `offered`, given to `body`, closed after. */
  private def offering[A](offered: Offered, copies: Option[WorkingCopies^] = None)(
      body: LocalTools^ => A
  ): A = {
    val tools =
      new LocalTools(offered, Redaction.of(Map.empty), Clock.system(), Fresh.random(), copies)
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
        Some(Origin.Work(of, run))
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
    test("a run's id reaches tail in a later request to the same checkout") {
      val root = Files.createTempDirectory("allowlisted").toRealPath()
      val script = root.resolve("slow")
      Files.writeString(script, "#!/bin/sh\necho begun\nsleep 1\necho finished\n")
      Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwx------"))
      val listed = Allowlist.Rule.Leading(right(Argv.of("./slow")))
      val offered = Offered.Allowlisted(right(Allowlist.of(Vector(listed), 5.seconds)))
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

    test(
      "an allowlisted run starts a listed program named relative to the checkout's root there, and refuses an unlisted one, which leaves nothing"
    ) {
      val root = Files.createTempDirectory("allowlisted").toRealPath()
      val script = root.resolve("hello")
      Files.writeString(script, "#!/bin/sh\necho \"hello from $(pwd)\"\n")
      Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwx------"))
      val listed = Allowlist.Rule.Leading(right(Argv.of("./hello")))
      val offered = Offered.Allowlisted(right(Allowlist.of(Vector(listed), 5.seconds)))
      val touch = right(Argv.of("touch", "x"))
      (
        run(offered, root, "run", ujson.Obj("program" -> "./hello")),
        run(offered, root, "run", ujson.Obj("program" -> "touch", "args" -> ujson.Arr("x"))),
        Files.exists(root.resolve("x"))
      ) ==> (
        Outcome.Done(s"Exit code 0. Output:\nhello from $root\n"),
        Outcome.Failed(HostError.Unlisted(touch).message),
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
      val offered = Offered.Allowlisted(right(Allowlist.of(Vector(listed), 5.seconds)))
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
