package grit.app.main

import grit.core.clock.{Clock, Fresh}
import grit.core.edge.{Console, Desks}
import grit.core.host.Argv
import grit.core.place.{Directory, Namespace, Place, Reaches, Service}
import grit.host.checkout.{LocalInstructions, LocalProcesses, Redaction}
import grit.kit.deployment.Offered
import grit.kit.run.{LocalEdge, LocalTools}
import grit.mcp.client.{McpServer, McpTransport}
import grit.mcp.edge.McpEdge
import grit.mcp.scope.McpScope

/** The edge the chat and `grit work` serve from their own process: the coding tools, and
  * scout's.
  */
private[main] object ChatEdges {

  /** `use` given the edge this process serves for `directory`: the coding tools `offered`
    * names, run in `root` ([[LocalTools]]), with the instruction files around it, and scout's
    * tools when `scout` declares its server, its program started now. Both are closed once `use`
    * returns or throws, which ends any command the tools left running. Why not, before `use`
    * runs: the coding tools' edge cannot be made, or scout's program cannot start or lists no
    * tool.
    */
  private[main] def serving[A](
      directory: Directory,
      root: java.nio.file.Path,
      offered: Offered,
      scout: Option[McpServer[McpTransport.Stdio]],
      redaction: Redaction,
      clock: Clock^,
      fresh: Fresh^
  )(use: Console.Edge^ => A): Either[String, A] = {
    val log = org.slf4j.LoggerFactory.getLogger("grit.edge")
    val tools = new LocalTools(offered, redaction, clock, fresh)
    LocalEdge.over(directory, tools, new LocalInstructions(), log) match {
      case Left(refused) =>
        tools.close()
        Left(refused.refusing("the coding tools"))
      case Right(coding) =>
        def served(here: Console.Edge^): Either[String, A] =
          try Right(use(here))
          finally tools.close()
        scout match {
          case None => served(coding)
          case Some(server) =>
            ScoutAt match {
              case Left(why) =>
                tools.close()
                Left(s"$ScoutVar: $why")
              case Right(at) =>
                // scout's program is started now, so a binary that cannot start refuses the start.
                McpEdge.local(
                  at,
                  server,
                  new LocalProcesses(root, sys.env, redaction),
                  clock,
                  Some(ScoutInstructions),
                  line => log.info(line)
                ) match {
                  case Left(why) =>
                    tools.close()
                    Left(s"$ScoutVar: $why")
                  case Right(local) =>
                    try
                      served((desks: Desks^) => {
                        coding.serve(desks)
                        local.serve(desks)
                      })
                    finally local.close()
                }
            }
        }
    }
  }

  /** The variable naming the absolute path of a scout binary (`~` is not expanded); unset, the
    * chat serves no scout.
    */
  private[main] val ScoutVar = "GRIT_SCOUT"

  /** The service scout's tools are hosted at: `service:scout`. */
  private[main] val ScoutAt: Either[String, Service] = Service.of("scout")

  /** What every conversation in a directory reaches when [[ScoutVar]] is set: `service:scout`.
    */
  private[main] def scoutReaches(scout: Service): Reaches =
    Reaches(Place.under(Namespace.Fs, Vector.empty), scout)

  /** The name scout's server is reached by, each of its tools offered as `scout_<tool>`. */
  private val ScoutServer = "scout"

  /** The tools of scout's a directory's conversations are offered, by scout's own names, in
    * order, each with what [[ScoutInstructions]] tells the model to ask it for. Its others
    * (`area`, `tests`) are not offered.
    */
  private[main] val ScoutTools: Vector[(String, String)] = Vector(
    "find" -> "for a name you don't know",
    "show" -> "for a signature or a body",
    "uses" -> "for every caller before you change a signature",
    "file" -> "for where a file's imports end and its definitions begin",
    "family" -> "for a trait and every implementation of it"
  )

  /** The Place layer advertised at `service:scout`, telling the model what scout reads and what
    * to ask each of [[ScoutTools]] for.
    */
  private[main] val ScoutInstructions: String = {
    val asks = ScoutTools.map((tool, what) => s"${ScoutServer}_$tool $what") match {
      case Vector(only) => only
      case init :+ last => s"${init.mkString(", ")}, and $last"
      case _ => ""
    }
    "scout reads this checkout's compiled Scala, not its source files: each answer is as " +
      "current as the last compile of the module it reads. After you edit a Scala file, compile " +
      s"its module with the build's own command (through run) before you ask scout about it. Ask " +
      s"$asks; use read for files that are not Scala."
  }

  /** scout's MCP server as `env` declares it: the program [[ScoutVar]] names, run as
    * `<path> mcp`, its tools [[ScoutTools]] offered ([[McpScope.Open]]), its `root` argument set
    * to the asking conversation's directory; `None` when unset; why not, naming the variable,
    * when the path is not absolute.
    */
  private[main] def scout(
      env: Map[String, String]
  ): Either[String, Option[McpServer[McpTransport.Stdio]]] =
    env.get(ScoutVar) match {
      case None => Right(None)
      case Some(path) if !scala.util.Try(java.nio.file.Path.of(path).isAbsolute).getOrElse(false) =>
        Left(s"$ScoutVar must be an absolute path to a scout binary, not $path")
      case Some(path) =>
        Argv
          .of(path, "mcp")
          .flatMap(
            McpServer.stdio(
              ScoutServer,
              _,
              McpScope.Open,
              allow = ScoutTools.map(_._1).toSet,
              directoryArgument = Some("root")
            )
          )
          .map(Some(_))
          .left
          .map(why => s"$ScoutVar: $why")
    }
}
