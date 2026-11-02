package grit.app.main

import grit.core.host.Argv
import grit.core.place.{Namespace, Place, Reaches, Service}
import grit.core.visibility.Integrity
import grit.kit.deployment.Offered
import grit.mcp.client.McpTransport

import utest.*

/** What the reference chat reads to serve scout, before it starts anything. */
object ChatEdgesTests extends TestSuite {

  val tests: Tests = Tests {

    test(
      "GRIT_SCOUT set: every directory's conversations reach service:scout, believed at member; unset, neither"
    ) {
      val scout = Service.of("scout").fold(sys.error, identity)
      val worktree = Place.under(Namespace.Fs, Vector("/work/repo/.claude/worktrees/fixer"))
      val thread = Place.under(Namespace.Slack, Vector("T0123", "C0456"))
      def declared(env: Map[String, String]) =
        Deploying
          .deployment(env, Offered.Read, Vector.empty, Vector.empty, java.time.ZoneOffset.UTC)
          .map(d =>
            (
              Reaches.of(d.reaches, worktree).map(_.place.written),
              Reaches.of(d.reaches, thread).map(_.place.written),
              Integrity.written(d.visibility.outputOf(scout))
            )
          )
      (declared(Map("GRIT_SCOUT" -> "/opt/scout")), declared(Map.empty)) ==> (
        Right((Vector("service:scout"), Vector(), "member")),
        Right((Vector(), Vector(), "untrusted"))
      )
    }

    test(
      "GRIT_SCOUT names a program run with mcp, its root set by grit; a relative path is refused naming the variable; unset, no scout"
    ) {
      (
        ChatEdges
          .scout(Map("GRIT_SCOUT" -> "/opt/scout"))
          .map(_.map(s => (s.name, s.transport, s.directoryArgument))),
        ChatEdges.scout(Map("GRIT_SCOUT" -> "bin/scout")),
        ChatEdges.scout(Map.empty)
      ) ==> (
        Right(
          Some(
            (
              "scout",
              McpTransport.Stdio(Argv.of("/opt/scout", "mcp").fold(sys.error, identity)),
              Some("root")
            )
          )
        ),
        Left("GRIT_SCOUT must be an absolute path to a scout binary, not bin/scout"),
        Right(None)
      )
    }
  }
}
