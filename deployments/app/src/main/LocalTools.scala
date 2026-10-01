package grit.app.main

import java.nio.file.Path

import grit.core.edge.{Route, ToolRequest}
import grit.core.place.Directory
import grit.core.tool.{Outcome, ToolName}
import grit.edge.{Run, Tools}
import grit.host.{LocalEdits, LocalShell, LocalWorkspace}
import grit.kit.deployment.Offered
import grit.tools.Coding

/** The coding tools this process's edge runs a request with, over the directory it was routed
  * to (a request routed to a service is `Failed`, and nothing runs): `read`, `list` and `search` when `offered` is [[Offered.Read]], every coding
  * tool when it is [[Offered.All]]. A command sees the process's own environment,
  * not `.env`'s.
  */
private[main] final class LocalTools(offered: Offered) extends Tools {

  def run(route: Route, request: ToolRequest): Outcome = route match {
    case Route.Directory(dir) => over(Path.of(Directory.value(dir)), request)
    // This edge registers only directories, so a service route never reaches it.
    case Route.Service(service) =>
      Outcome.Failed(
        s"The coding tools act on a directory, not ${service.place.written}; nothing ran."
      )
  }

  private def over(root: Path, request: ToolRequest): Outcome = {
    val ws = new LocalWorkspace(root)
    def refused(name: ToolName): Outcome =
      Outcome.Failed(s"The coding tools offer ${ToolName.value(name)} twice; nothing ran.")
    offered match {
      case Offered.Read =>
        Coding.readOnly(ws).fold(d => refused(d.name), tools => Run.request(request, tools))
      case Offered.All =>
        Coding
          .all(ws, new LocalEdits(root), new LocalShell(root, sys.env))
          .fold(d => refused(d.name), tools => Run.request(request, tools))
    }
  }
}
