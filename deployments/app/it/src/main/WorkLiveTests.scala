package grit.app.main

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

import scala.collection.mutable

import grit.core.clock.{Clock, Fresh}
import grit.core.place.Directory
import grit.core.recipe.TurnRecipe
import grit.core.store.Origin
import grit.dbos.sql.{DbConfig, TestPostgres}
import grit.host.checkout.{LocalRuns, Redaction}
import grit.kit.deployment.{Deployment, Offered}
import grit.kit.environment.Secrets
import grit.kit.rehearse.Rehearsed
import grit.kit.run.Kit

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

  /** `body` over the runs `grit work` checks a task with in `root`, closed after. */
  private def checking[A](root: Path)(body: LocalRuns^ => A): A = {
    val runs = new LocalRuns(root, sys.env, Redaction.of(Map.empty), Clock.system(), Fresh.random())
    try body(runs)
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
        Fresh.random()
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
            checking(root)(r =>
              Work.said(c, origin, "say hello", Work.DefaultTurns, None, r, first += _)
            )
          ),
          working(root, config, deployment)(c =>
            checking(root)(r =>
              Work.said(c, origin, "say hello", Work.DefaultTurns, None, r, second += _)
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
      "a work run checks its task once its turn answers: a failure goes on in a fresh turn, a pass ends it; run again, it re-runs no check a later turn followed"
    ) {
      checkout { root =>
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
        def run(lines: mutable.ArrayBuffer[String]) =
          working(root, config, deployment)(c =>
            checking(root)(r =>
              Work.said(c, origin, "say hello", Work.DefaultTurns, check, r, lines += _)
            )
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
        (first(2).replaceAll(Id, "<id>"), first(5).replaceAll(Id, "<id>")) ==> (
          "[main] <id>: the check failed (exit 4); going on in a fresh turn (turn 2 of at most 8)" +
            "\n  | once is missing",
          "[main] <id>: the check passed; the task is done"
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
          // The allowlisted coding tools and scout's, beside what every turn is offered; none of
          // grit's conversation tools, the turn briefed for one task; task_status from its first
          // call, so the tools never change within the turn.
          Some(
            "edit,extract,list,read,read_hidden,run,scout_find,search,stop,tail,task_status,topic,write"
          ),
          Some(false)
        )
      }
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
  }
}
