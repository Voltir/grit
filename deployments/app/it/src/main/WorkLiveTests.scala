package grit.app.main

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

import scala.collection.mutable

import grit.core.clock.{Clock, Fresh}
import grit.core.place.Directory
import grit.core.store.Origin
import grit.dbos.sql.{DbConfig, TestPostgres}
import grit.host.checkout.Redaction
import grit.kit.deployment.{Deployment, Offered}
import grit.kit.environment.Secrets
import grit.kit.rehearse.Rehearsed
import grit.kit.run.{Kit, Launch}

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
        java.time.ZoneOffset.UTC
      )
    )

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
          Launch.Run(announced = true, stubDelay = 0),
          clock,
          here
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
            Work.said(c, origin, "say hello", line => first += line)
          ),
          working(root, config, deployment)(c =>
            Work.said(c, origin, "say hello", line => second += line)
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
      "a dry run shows the turn's first call, offering the allowlisted coding tools and scout's, sends nothing and records no message"
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
          // The allowlisted coding tools and scout's, beside what every turn is offered.
          Some(
            "about,clearance,edit,extract,file_read,list,message_ids,read,read_hidden,run," +
              "scout_find,search,stop,tail,topic,write"
          ),
          Some(false)
        )
      }
    }
  }
}
