package grit.kit.run

import java.nio.file.Path

import grit.core.clock.{Clock, Fresh}
import grit.core.edge.{Route, ToolRequest}
import grit.core.host.Allowlisted
import grit.core.place.Directory
import grit.core.tool.{Outcome, SetRefusal, ToolSet}
import grit.edge.{Run, Tools}
import grit.host.checkout.{LocalEdits, LocalRuns, LocalShell, LocalWorkspace, Redaction}
import grit.kit.deployment.Offered
import grit.tools.Coding

/** The coding tools this process's edge runs a request with, over the directory it was routed
  * to (a request routed to a service is `Failed`, and nothing runs): those `offered` hosts
  * ([[Offered.hosted]]), an allowlisted `run` starting only what its allowlist permits. A command
  * sees the process's own environment, not `.env`'s; what a tool shows is cleared by
  * `redaction`, the deployment's one. A command an allowlisted `run` left going is one of its
  * directory's runs, which every later request routed there reaches, whichever conversation
  * sent it, until [[close]].
  */
final class LocalTools(offered: Offered, redaction: Redaction, clock: Clock^, fresh: Fresh^)
    extends Tools
    with AutoCloseable
    with caps.SharedCapability {

  /** Each directory's runs, made by its first allowlisted request. Only ever read or changed
    * under this object's lock.
    */
  private val runs: java.util.HashMap[Path, LocalRuns^] =
    new java.util.HashMap[Path, LocalRuns^]()

  /** The tools [[run]] runs, as the engine offers them; `Left` names a tool Coding cannot offer
    * with the others.
    */
  def hosted: Either[SetRefusal, ToolSet] =
    ToolSet.of(offered.hosted.map(_.entry))

  def run(route: Route, request: ToolRequest, said: String => Unit): Outcome = route match {
    case Route.Directory(dir) => over(Path.of(Directory.value(dir)), request)
    // This edge registers only directories, so a service route never reaches it.
    case Route.Service(service) =>
      Outcome.Failed(
        s"The coding tools act on a directory, not ${service.place.written}; nothing ran."
      )
  }

  private def over(root: Path, request: ToolRequest): Outcome = {
    val ws = new LocalWorkspace(root, redaction)
    def refused(why: SetRefusal): Outcome =
      Outcome.Failed(s"${why.refusing("The coding tools")}; nothing ran.")
    offered match {
      case Offered.Read =>
        Coding.readOnly(ws).fold(refused, tools => Run.request(request, tools))
      case Offered.All =>
        Coding
          .all(ws, new LocalEdits(root, redaction), new LocalShell(root, sys.env, redaction))
          .fold(refused, tools => Run.request(request, tools))
      case Offered.Allowlisted(allowlist) =>
        Coding
          .allowlisted(
            ws,
            new LocalEdits(root, redaction),
            Allowlisted.over(runsAt(root), allowlist)
          )
          .fold(refused, tools => Run.request(request, tools))
    }
  }

  /** Kills every command an allowlisted `run` left going, and removes their files. */
  def close(): Unit = {
    synchronized {
      runs.values.forEach(_.close())
      runs.clear()
    }
  }

  /** `root`'s runs, made when it has none. */
  private def runsAt(root: Path): LocalRuns^ = synchronized {
    if (runs.containsKey(root)) runs.get(root)
    else {
      val made: LocalRuns^ = new LocalRuns(root, sys.env, redaction, clock, fresh)
      val _ = runs.put(root, made)
      made
    }
  }
}
