package grit.app.main

import grit.core.host.Argv
import grit.core.place.{Namespace, Place, Reaches, Service}
import grit.mcp.client.{McpServer, McpTransport}
import grit.mcp.scope.McpScope

/** The tools the chat serves from its own process besides the coding tools: scout's. */
private[main] object ChatEdges {

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

  /** The Place layer advertised at `service:scout`, telling the model what scout reads. */
  private[main] val ScoutInstructions: String =
    "scout reads this checkout's compiled Scala, not its source files: each answer is as " +
      "current as the last compile of the module it reads. After you edit a Scala file, compile " +
      "its module with the build's own command (through run) before you ask scout about it. Ask " +
      "scout_find for a name you don't know, scout_show for a signature or a body, scout_uses " +
      "for every caller before you change a signature, and scout_file for where a file's " +
      "imports end and its definitions begin; use read for files that are not Scala."

  /** scout's MCP server as `env` declares it: the program [[ScoutVar]] names, run as
    * `<path> mcp`, every tool it lists offered ([[McpScope.Open]]), its `root` argument set to
    * the asking conversation's directory; `None` when unset; why not, naming the variable, when
    * the path is not absolute.
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
          .flatMap(McpServer.stdio("scout", _, McpScope.Open, directoryArgument = Some("root")))
          .map(Some(_))
          .left
          .map(why => s"$ScoutVar: $why")
    }
}
