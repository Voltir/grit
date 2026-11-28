package grit.app.main

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

import scala.collection.mutable

import grit.app.config.RepositoriesEnv
import grit.core.clock.{Clock, Fresh}
import grit.core.edge.Console
import grit.core.host.{Argv, Processes, RelPath}
import grit.core.place.{Directory, Namespace, Place}
import grit.core.recipe.TurnRecipe
import grit.core.secret.{TestSecrets, Variable as SecretVariable}
import grit.core.store.{CheckoutState, Origin, Payload, WentOn}
import grit.core.visibility.Subject
import grit.dbos.sql.{DbConfig, TestPostgres}
import grit.host.checkout.{LocalProcesses, LocalRuns, Redaction}
import grit.host.git.{GitFixture, WorkingCopies}
import grit.kit.deployment.{Deployment, Offered}
import grit.kit.environment.Secrets
import grit.kit.rehearse.Rehearsed
import grit.kit.run.{Kit, Pushing}
import grit.kit.work.Work

import utest.*

/** `grit work` against Postgres and DBOS, composed as the program composes it: this directory's
  * edge served ([[ChatEdges.serving]]) through the console of an engine this process starts
  * ([[Kit.console]]), the stub answering, with a script in scout's place.
  */
object WorkLiveTests extends TestSuite {

  private def right[A](e: Either[String, A]): A = e.fold(why => sys.error(why), identity)

  /** An MCP server over stdio listing `find`, read-only, answering each call "found". */
  private val Script =
    """#!/bin/sh
      |while IFS= read -r line; do
      |  id=$(printf '%s' "$line" | sed -n 's/.*"id":\("[^"]*"\|[0-9]*\).*/\1/p')
      |  case "$line" in
      |    *'"tools/list"'*)
      |      printf '{"jsonrpc":"2.0","id":%s,"result":{"tools":[{"name":"find","description":"Finds a definition by name.","inputSchema":{"type":"object","properties":{"pattern":{"type":"string"},"root":{"type":"string"}},"required":["pattern"]},"annotations":{"readOnlyHint":true}}]}}\n' "$id";;
      |    *'"tools/call"'*)
      |      printf '{"jsonrpc":"2.0","id":%s,"result":{"content":[{"type":"text","text":"found"}],"isError":false}}\n' "$id";;
      |  esac
      |done
      |""".stripMargin

  /** A checkout holding the script, executable, handed to `body`, then deleted. */
  private def checkout[A](body: Path => A): A = {
    val root = Files.createTempDirectory("grit-work").toRealPath()
    try {
      val script = root.resolve("scout")
      val _ = Files.writeString(script, Script)
      val _ = Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwx------"))
      body(root)
    } finally
      Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.delete(p))
  }

  private val offered = Offered.Allowlisted(Offered.ReferenceAllowlist)

  /** The reference deployment as `grit work` builds it, scout declared at `root`'s script. */
  private def deployed(root: Path): Deployment =
    right(
      Deploying.deployment(
        Map(ChatEdges.ScoutVar -> root.resolve("scout").toString),
        offered,
        Vector.empty,
        Vector.empty,
        java.time.ZoneOffset.UTC,
        recipe = TurnRecipe.Tasked
      )
    )

  /** A turn's workflow id, as `grit work` prints it. */
  private val Id = "[0-9a-f]{8}-[0-9a-f-]+:[0-9]+"

  /** `body` over the runs `grit work` checks a task with in `root`, closed after, and the
    * programs it reads `root`'s diff with.
    */
  private def checking[A](root: Path)(body: (LocalRuns^, Processes^) => A): A = {
    val runs = new LocalRuns(root, sys.env, Redaction.of(Map.empty), Clock.system(), Fresh.random())
    try body(runs, new LocalProcesses(root, sys.env, Redaction.of(Map.empty)))
    finally runs.close()
  }

  /** `step` run as `grit work` runs its last step: `root`'s edge served, scout's among it,
    * through the console of the engine of `config`'s database.
    */
  private def working(root: Path, config: DbConfig, deployment: Deployment)(
      step: grit.core.edge.Console^ => Either[String, Unit]
  ): Either[String, Unit] = {
    val secrets =
      Secrets
        .of(ServedDeployment.env(config), deployment)
        .fold(r => sys.error(r.message), identity)
    val clock = Clock.system()
    right(
      ChatEdges.serving(
        right(Directory.of(root.toString)),
        root,
        offered,
        right(ChatEdges.scout(Map(ChatEdges.ScoutVar -> root.resolve("scout").toString))),
        Redaction.of(Map.empty),
        clock,
        Fresh.random(),
        Left("no machine check here"),
        None,
        None
      ) { here =>
        Kit.console(
          deployment,
          secrets,
          scala.concurrent.duration.Duration.Zero,
          clock,
          here,
          announced = true
        ) match {
          case Left(refused) => Left(refused.message)
          case Right(opener) => Work.opened(opener)(step)
        }
      }
    )
  }

  /** A run, `dry` or not, whose `admit` refuses either runner, beside an engine already holding
    * the database: whether the engine's own step ended well, what the run came to, what it
    * printed, and whether its turn began.
    */
  private def admitRefused(
      dry: Boolean
  ): (Either[String, Unit], Option[Either[String, Unit]], Vector[String], Option[Boolean]) =
    checkout { root =>
      val config = TestPostgres.freshDatabase(if (dry) "work_admit_dry" else "work_admit")
      val deployment = deployed(root)
      val origin = Origin.Tui(right(Directory.of(root.toString)), "bench")
      val secrets =
        Secrets
          .of(ServedDeployment.env(config), deployment)
          .fold(r => sys.error(r.message), identity)
      val clock = Clock.system()
      val printed = mutable.ArrayBuffer.empty[String]
      // Which runner it was asked about is in its refusal: the engine `working` started holds
      // the database, so the run's own console would attach to it.
      val under = Work.Under(
        deployment,
        secrets,
        Work.Where.At(origin, root),
        Map.empty,
        Redaction.of(Map.empty),
        scala.concurrent.duration.Duration.Zero,
        {
          case Console.Runner.Here => Left("refused here")
          case Console.Runner.Elsewhere(_) => Left("refused elsewhere")
        }
      )
      val asked = Work.Asked(root.resolve("task.md"), dry, None, None)
      var ran: Option[Either[String, Unit]] = None
      var begun: Option[Boolean] = None
      val held = working(root, config, deployment) { console =>
        ran = ChatEdges
          .serving(
            right(Directory.of(root.toString)),
            root,
            offered,
            None,
            Redaction.of(Map.empty),
            clock,
            Fresh.random(),
            Left("no machine check here"),
            None,
            None
          )(here =>
            Work.run(
              asked,
              "say hello",
              Work.DefaultTurns,
              under,
              here,
              clock,
              Fresh.random(),
              printed += _
            )
          )
          .toOption
        begun = console.inbox.begun(origin).toOption
        Right(())
      }
      (held, ran, printed.toVector, begun)
    }

  val tests: Tests = Tests {

    test(
      "a work run says its text once in the directory's session and prints its turn and reply; said again it awaits the same turn"
    ) {
      checkout { root =>
        val config = TestPostgres.freshDatabase("work_said")
        val deployment = deployed(root)
        val origin = Origin.Tui(right(Directory.of(root.toString)), "bench")
        val first, second = mutable.ArrayBuffer.empty[String]
        val ran = (
          working(root, config, deployment)(c =>
            checking(root)((r, g) =>
              Work
                .said(c, origin, "say hello", Work.DefaultTurns, None, r, g, () => None, first += _)
                .map(_ => ())
            )
          ),
          working(root, config, deployment)(c =>
            checking(root)((r, g) =>
              Work
                .said(
                  c,
                  origin,
                  "say hello",
                  Work.DefaultTurns,
                  None,
                  r,
                  g,
                  () => None,
                  second += _
                )
                .map(_ => ())
            )
          )
        )
        (ran, first.size, first.forall(_.startsWith("[main] ")), second.toVector) ==> (
          (Right(()), Right(())),
          1,
          true,
          first.toVector
        )
      }
    }

    test(
      "a work run checks its task once its turn answers: a failure goes on in a fresh turn, its note carrying the checkout's diff, a pass ends it; run again, it re-runs no check a later turn followed"
    ) {
      checkout { root =>
        val git: Processes^ = new LocalProcesses(root, sys.env, Redaction.of(Map.empty))
        def gitIn(args: String*) =
          right(
            Argv
              .of("git", args*)
              .flatMap(
                git
                  .run(_, RelPath.Root, scala.concurrent.duration.Duration(30, "s"))
                  .left
                  .map(_.message)
              )
          )
        val _ = Files.writeString(root.resolve(".gitignore"), ".grit/\n")
        val _ = gitIn("init", "--quiet", "-b", "main")
        val _ = gitIn("add", ".")
        val _ = gitIn(
          "-c",
          "user.name=t",
          "-c",
          "user.email=t@example.invalid",
          "-c",
          "commit.gpgsign=false",
          "commit",
          "--quiet",
          "-m",
          "a"
        )
        val _ = gitIn("update-ref", "refs/remotes/origin/main", "HEAD")
        val _ = gitIn("symbolic-ref", "refs/remotes/origin/HEAD", "refs/remotes/origin/main")
        val config = TestPostgres.freshDatabase("work_checked")
        val deployment = deployed(root)
        val origin = Origin.Tui(right(Directory.of(root.toString)), "bench")
        val check = Some(
          right(
            grit.core.store.WentOn.Command.of(
              "echo ran >> runs; test -f once || { touch once; echo 'once is missing'; exit 4; }"
            )
          )
        )
        val first, second = mutable.ArrayBuffer.empty[String]
        val noted = mutable.ArrayBuffer.empty[Payload]
        def run(lines: mutable.ArrayBuffer[String]) =
          working(root, config, deployment)(c =>
            checking(root)((r, g) =>
              Work
                .said(
                  c,
                  origin,
                  "say hello",
                  Work.DefaultTurns,
                  check,
                  r,
                  g,
                  () => None,
                  lines += _
                )
                .map(_ => ())
            ).map { _ =>
              c.inbox.ingested(origin, Work.source("say hello", 2)).toOption.flatten.foreach { t =>
                c.db
                  .read(Subject.Turn(t))(c.entries.ofTurn(t))
                  .foreach(es => noted ++= es.map(_.payload).take(1))
              }
            }
          )
        val ran = (run(first), Files.readAllLines(root.resolve("runs")).size)
        val again = (run(second), Files.readAllLines(root.resolve("runs")).size)
        def shapes(lines: mutable.ArrayBuffer[String]) =
          lines.toVector.map(_.replaceAll(Id, "<id>").takeWhile(_ != ':'))
        (
          ran,
          again,
          shapes(first),
          shapes(second),
          first.drop(1).take(1).map(_.replaceAll(Id, "<id>"))
        ) ==> (
          (Right(()), 2),
          (Right(()), 3),
          Vector(
            "[main] <id> -> replied",
            "[main] <id> ended its turn; checking",
            "[main] <id>",
            "[main] <id> -> replied",
            "[main] <id> ended its turn; checking",
            "[main] <id>"
          ),
          Vector(
            "[main] <id> -> replied",
            "[main] <id>",
            "[main] <id> -> replied",
            "[main] <id> ended its turn; checking",
            "[main] <id>"
          ),
          Vector(
            "[main] <id> ended its turn; checking: echo ran >> runs; test -f once || { touch " +
              "once; echo 'once is missing'; exit 4; }"
          )
        )
        (
          first(2).replaceAll(Id, "<id>"),
          first(5).replaceAll(Id, "<id>"),
          noted.headOption.collect { case Payload.Continuation(_, 2, _: WentOn.CheckFailed, read) =>
            read
          }
        ) ==> (
          "[main] <id>: the check failed (exit 4); going on in a fresh turn (turn 2 of at most 8)" +
            "\n  | once is missing",
          "[main] <id>: the check passed; the task is done",
          Some(
            CheckoutState(
              Some(CheckoutState.Diff.of("origin/main", "", Vector("once", "runs"))),
              None
            )
          )
        )
      }
    }

    test(
      "a dry run shows the turn's first call, offering the allowlisted coding tools, scout's and task_status, sends nothing and records no message"
    ) {
      checkout { root =>
        val config = TestPostgres.freshDatabase("work_dry")
        val deployment = deployed(root)
        val origin = Origin.Tui(right(Directory.of(root.toString)), "bench")
        var shown: Option[Rehearsed] = None
        var begun: Option[Boolean] = None
        val ran = working(root, config, deployment) { console =>
          val r = Work.rehearsed(config, deployment, origin, "say hello", Clock.system())
          shown = r.toOption
          begun = console.inbox.begun(origin).toOption
          r.map(_ => ())
        }
        val first = shown.flatMap(_.calls.lastOption)
        (
          ran,
          first.map(_.answered),
          first.map(_.input).collect { case Rehearsed.Input.Model(request) =>
            request.tools.map(_.name).sorted.mkString(",")
          },
          begun
        ) ==> (
          Right(()),
          Some(Rehearsed.Answered.Unsent),
          // The allowlisted coding tools and scout's, beside topic; none of grit's conversation
          // tools, nor read_hidden or extract, the turn briefed for one task in its checkout;
          // task_status from its first call, so the tools never change within the turn.
          Some("edit,list,read,run,scout_find,search,stop,tail,task_status,topic,write"),
          Some(false)
        )
      }
    }

    test("a run admit refuses says nothing and starts no turn") {
      admitRefused(dry = false) ==>
        (Right(()), Some(Left("refused elsewhere")), Vector.empty, Some(false))
    }

    test("a dry run admit refuses shows nothing") {
      admitRefused(dry = true) ==>
        (Right(()), Some(Left("refused elsewhere")), Vector.empty, Some(false))
    }

    test("a dry run's first call carries scout's declared guidance in its system prompt") {
      checkout { root =>
        val config = TestPostgres.freshDatabase("work_dry_guidance")
        val deployment = deployed(root)
        val origin = Origin.Tui(right(Directory.of(root.toString)), "bench")
        var system: Option[String] = None
        val _ = working(root, config, deployment) { _ =>
          Work.rehearsed(config, deployment, origin, "say hello", Clock.system()).map { r =>
            system = r.calls.lastOption.map(_.input).collect {
              case Rehearsed.Input.Model(request) => request.system
            }
          }
        }
        system.map(_.contains(ChatEdges.ScoutInstructions)) ==> Some(true)
      }
    }
    test(
      "a work run in a repository runs its check in its own copy on grit/<run>, and leaves it with nothing uncommitted"
    ) {
      checkout { root =>
        GitFixture.inRepository { repo =>
          val config = TestPostgres.freshDatabase("work_in_copy")
          val deployment = deployed(root)
          val secrets =
            Secrets
              .of(ServedDeployment.env(config), deployment)
              .fold(r => sys.error(r.message), identity)
          val clock = Clock.system()
          val copies = new WorkingCopies(
            GitFixture.repository,
            repo.source,
            repo.root,
            repo.processes,
            clock
          )
          val run = GitFixture.work("w1")
          val check =
            right(WentOn.Command.of("test \"$(git rev-parse --abbrev-ref HEAD)\" = grit/w1"))
          val asked = Work.Asked(
            root.resolve("task.md"),
            dry = false,
            Some(check),
            Some(Work.InRepository(GitFixture.repository, Some(run)))
          )
          val printed = mutable.ArrayBuffer.empty[String]
          val ran = right(
            ChatEdges.serving(
              right(Directory.of(root.toString)),
              root,
              offered,
              None,
              Redaction.of(Map.empty),
              clock,
              Fresh.random(),
              Left("no machine check here"),
              None,
              Some(copies)
            )(here =>
              Work.run(
                asked,
                "say hello",
                Work.DefaultTurns,
                Work.Under(
                  deployment,
                  secrets,
                  Work.Where.Copy(copies, run),
                  sys.env,
                  Redaction.of(Map.empty),
                  scala.concurrent.duration.Duration.Zero,
                  RepositoriesEnv.startedHere
                ),
                here,
                clock,
                Fresh.random(),
                printed += _
              )
            )
          )
          val copy = repo.root.resolve("grit").resolve("w1")
          (
            ran,
            printed.exists(_.contains(s"checking: ${check.text}")),
            printed.exists(_.contains("check failed")),
            GitFixture.git(repo, copy, "status", "--porcelain").output.text
          ) ==> (Right(()), true, false, "")
        }
      }
    }
    test(
      "a run whose branch was offered before it was killed, run again, prints its open pull request, opens none, and removes its copy"
    ) {
      checkout { root =>
        GitFixture.inRepository { repo =>
          val (ran, printed, remote) = pushed(root, repo, "work_offered_again", offerFirst = true)
          (
            ran,
            printed
              .filter(_.contains("pull request"))
              .map(_.replaceFirst("^\\[main\\] \\S+:", "")),
            remote.opens.size,
            Files.exists(repo.root.resolve("grit").resolve("w1")),
            GitFixture.hasBranch(repo, repo.source, "grit/w1")
          ) ==> (
            Right(()),
            Vector(" the check passed; its pull request is open: https://remote.invalid/pull/1"),
            1,
            false,
            true
          )
        }
      }
    }
    test(
      "a run whose check passes with its branch not offered goes on once with grit's note, then stops saying so"
    ) {
      checkout { root =>
        GitFixture.inRepository { repo =>
          val (ran, printed, remote) = pushed(root, repo, "work_not_offered", offerFirst = false)
          (
            ran.left.map(_.replaceFirst("^\\S+:", "")),
            printed.exists(
              _.endsWith(
                "the check passed, but its branch is not pushed; going on in a fresh turn (turn 2 " +
                  s"of at most ${Work.DefaultTurns})"
              )
            ),
            remote.opens.size,
            Files.exists(repo.root.resolve("grit").resolve("w1"))
          ) ==> (
            Left(
              " the check passed, but its branch is not pushed again after grit went on to ask " +
                "for it; stopping, since another turn is unlikely to change it"
            ),
            true,
            0,
            true
          )
        }
      }
    }
  }

  /** A run `w1` of `repo` said through the stub model, its check passing, its branch pushed to
    * `repo`'s bare origin and offered on a fake remote repository; with `offerFirst`, the branch
    * offered before the run, as a run killed after its offer left it. What it came to, what it
    * printed, and the fake.
    */
  private def pushed(
      root: Path,
      repo: GitFixture.Repo^,
      database: String,
      offerFirst: Boolean
  ): (Either[String, Unit], Vector[String], GitFixture.FakeRemoteRepository) = {
    val config = TestPostgres.freshDatabase(database)
    val deployment = deployed(root)
    val secrets =
      Secrets.of(ServedDeployment.env(config), deployment).fold(r => sys.error(r.message), identity)
    val clock = Clock.system()
    val remote = new GitFixture.FakeRemoteRepository(repo.origin.toString)
    val pushing = Pushing.of(
      repo.source,
      GitFixture.targetOf(remote),
      TestSecrets.of(SecretVariable("GRIT_REMOTE_REPOSITORY_TOKEN"), "ghs_plantedRemoteToken123"),
      repo.environment,
      Redaction.of(Map.empty),
      Place.under(Namespace.Service, Vector("github", "acme", "app")),
      "acme/app"
    ) match {
      case Right(p) => p
      case Left(why) => throw new java.lang.AssertionError(why)
    }
    val copies =
      new WorkingCopies(
        GitFixture.repository,
        repo.source,
        repo.root,
        repo.processes,
        clock,
        Some(pushing.branches)
      )
    val run = GitFixture.work("w1")
    if (offerFirst) {
      val copy =
        copies.opened(run).fold(e => throw new java.lang.AssertionError(e.message), identity)
      val _ = GitFixture.git(
        repo,
        copy,
        "-c",
        "user.name=t",
        "-c",
        "user.email=t@example.com",
        "commit",
        "--allow-empty",
        "-qm",
        "fix"
      )
      val _ = pushing.branch(run).offer("Fix", "Fixed.")
    }
    val check = right(WentOn.Command.of("test \"$(git rev-parse --abbrev-ref HEAD)\" = grit/w1"))
    val asked = Work.Asked(
      root.resolve("task.md"),
      dry = false,
      Some(check),
      Some(Work.InRepository(GitFixture.repository, Some(run)))
    )
    val printed = mutable.ArrayBuffer.empty[String]
    val ran = ChatEdges
      .serving(
        right(Directory.of(root.toString)),
        root,
        offered,
        None,
        Redaction.of(Map.empty),
        clock,
        Fresh.random(),
        Left("no machine check here"),
        None,
        Some(copies),
        Some(pushing)
      )(here =>
        Work.run(
          asked,
          "say hello",
          Work.DefaultTurns,
          Work.Under(
            deployment,
            secrets,
            Work.Where.Copy(copies, run, Some(pushing)),
            sys.env,
            Redaction.of(Map.empty),
            scala.concurrent.duration.Duration.Zero,
            RepositoriesEnv.startedHere
          ),
          here,
          clock,
          Fresh.random(),
          printed += _
        )
      )
      .flatten
    (ran, printed.toVector, remote)
  }
}
